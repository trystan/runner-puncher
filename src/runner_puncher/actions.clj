(ns runner_puncher.actions
  (:require [runner_puncher.framework :refer :all]
            [runner_puncher.worldgen :refer :all]
            [runner_puncher.creatures :refer :all]
            [runner_puncher.items :refer :all]
            [runner_puncher.util :refer :all]))

(defn add-message [game id message]
  (if (= :player id)
    (update game :messages #(conj % {:text message :at (:tick game)}))
    game))

(defn add-effect [game x y ttl c fg bg]
  (let [effect {:is-effect true :x x :y y :ttl ttl :char c
                :fg fg :bg bg :id (str "effect-" (.toString (java.util.UUID/randomUUID)))}]
    (assoc game (:id effect) effect)))

(defn add-projectile [game x y path c fg bg]
  (let [effect {:is-effect true :x x :y y :path path :ttl 10 :char c
                :fg fg :bg bg :id (str "effect-" (.toString (java.util.UUID/randomUUID)))
                :does-damage true}]
    (assoc game (:id effect) effect)))

(defn drop-item [game player item]
  (-> game
      (assoc (:id item) item)
      (assoc-in [(:id item) :x] (:x player))
      (assoc-in [(:id item) :y] (:y player))))

(defn unequip-slot [game slot]
  (let [item (get-in game [:player slot])]
    (if (and slot item)
      (-> game
          (drop-item (get game :player) item)
          (assoc-in [:player slot] nil)
          (update :player #(merge-with - % (:effect item))))
      game)))

(defn equip-item [game item]
  (let [set-slot (fn [g] (if (:slot item)
                           (assoc-in g [:player (:slot item)] item)
                           g))
        existing-item (get-in game [:player (:slot item)])
        message-player (fn [g] (if existing-item
                      (add-message g :player (str "You drop your " (:name existing-item) " and pick up " (:name item) "."))
                      (add-message g :player (str "You stop to pick up " (:name item)))))]
    (-> game
        (message-player)
        (unequip-slot (:slot item))
        (set-slot)
        (update :player #(merge-with + % (:effect item)))
        (update :player (fn [p] (update p :health #(min % (:max-health p))))))))

(defn apply-purchace [game item]
  (-> game
      (update-in [:player :gold] #(- % (:price item)))
      (equip-item item)))

(defn fix-store-prices [items]
  (vec (for [i (range 0 (count items))]
         (assoc (nth items i) :price (+ 5 (* 5 i))))))

(defn restock-store-items [items]
  (let [items (remove nil? items)]
    (fix-store-prices (take 9 (concat (drop 3 items) (repeatedly 20 (partial random-item true)))))))

(defn enter-store [game]
  (push-screen (:store-screen game))
  (let [affect (get-in game [:player :affect-prices] 0)
        apply-discount (fn [i] (update i :price #(+ % affect)))]
    (-> game
        (assoc-in [:player :steps-remaining] (get-in game [:player :max-steps]))
        (update :store-items restock-store-items)
        (update :store-items #(mapv apply-discount %)))))

(defn creature-at [game xy]
  (first (for [[_ e] game
               :when (and (:is-creature e) (= xy [(:x e) (:y e)]))]
           e)))

(defn enemies [game]
  (for [[id e] game :when (and (not= :player id) (:is-creature e))] e))

(defn item-at [game xy]
  (first (for [[_ e] game
               :when (and (:is-item e) (= xy [(:x e) (:y e)]))]
           e)))

(defn items [game]
  (for [[_ e] game :when (:is-item e)] e))

(defn update-effect [effect]
  (if (seq (:path effect))
    (assoc effect
      :x (first (first (:path effect)))
      :y (second (first (:path effect)))
      :path (rest (:path effect)))
    (update effect :ttl dec)))

(defn apply-projectile-damage [game {:keys [x y]}]
  (let [c (creature-at game [x y])]
    (if c
      (update-in game [(:id c) :health] dec)
      game)))

(defn update-effects [game]
  (let [expired-projectiles (for [[id e] game
                                  :let [ttl (:ttl e 99)]
                                  :when (and (= ttl 0) (:does-damage e))]
                              e)
        game (into {} (for [[id e] game
                            :let [ttl (:ttl e 99)]
                            :when (> ttl 0)]
                        (if (:is-effect e)
                          [id (update-effect e)]
                          [id e])))]
    (reduce apply-projectile-damage game expired-projectiles)))

(defn spawn-creature-at [game x y allow-sumoner]
  (let [c (new-enemy (get-in game [:player :difficulty]) (:enemy-catalog game) [x y])]
    (if (and (not allow-sumoner) (any? #(= % [:summon-others]) (:on-death c)))
      (spawn-creature-at game x y allow-sumoner)
      (into game [[(:id c) c]]))))

(defn spawn-creature-near [game x y]
  (let [candidates (filter (fn [[x2 y2]] (nearby x y x2 y2 4 9)) (find-tiles :floor (:grid game)))
        c (if (empty? candidates)
            nil
            (new-enemy (get-in game [:player :difficulty]) (:enemy-catalog game) (rand-nth candidates)))]
    (if c
      (into game [[(:id c) c]])
      game)))

(defn remove-items [game]
  (reduce dissoc game (map :id (items game))))

(defn remove-enemies [game]
  (reduce dissoc game (map :id (enemies game))))

(defn until-blocked [game points]
  (loop [so-far []
         here (first points)
         remaining (rest points)]
    (cond
     (nil? here)
     (rest so-far)
     (nil? (get-in game [:grid here]))
     (rest so-far)
     (get-in tiles [(get-in game [:grid here]) :walkable])
     (recur
      (conj so-far here)
      (first remaining)
      (rest remaining))
     :else (rest so-far))))

(defn creature-can-see-tile [game from-id x y]
  (let [c1 (get game from-id)
        points (until-blocked game (bresenham (:x c1) (:y c1) x y))]
    (= [x y] (last points))))

(defn creature-can-see-creature [game from-id to-id]
  (let [c (get game to-id)]
    (creature-can-see-tile game from-id (:x c) (:y c))))

(defn knockback-creature [game id dx dy]
  (let [resistance-points (get-in game [id :resist-knockback] 0)
        resistance (loop [r 1.0
                          p resistance-points]
                     (if (= 0 p)
                       r
                       (recur (* 0.5 r) (dec p))))
        dx (int (* dx resistance))
        dy (int (* dy resistance))]
    (if (= 0 dx dy)
      game
      (let [kb (fn [c] (if (:immune-to-knockback c)
                         c
                         (-> c
                             (assoc :is-knocked-back true)
                             (assoc :path (until-blocked game (bresenham (:x c)
                                                                         (:y c)
                                                                         (+ dx (:x c))
                                                                         (+ dy (:y c))))))))]
        (update-in game [id] kb)))))

(defn attack-creature [game id-from id-to]
  (if (:is-knocked-back (get game id-from))
    (update game id-from end-movement)
    (let [attacker (get game id-from)
          attacked (get game id-to)
          damage (max 1 (- (:attack attacker 1) (:defence attacked 0)))
          dx (- (:x attacked) (:x attacker))
          dy (- (:y attacked) (:y attacker))
          show-message (fn [g] (cond
                                (= :player id-from)
                                (add-message g :player (str "You punch the " (creature-name attacked) "."))
                                (= :player id-to)
                                (add-message g :player (str "The " (creature-name attacker) " hits you for " damage " damage."))
                                :else
                                g))
          [dx dy] (mapv #(* (:knockback-amount attacker) %) [dx dy])
          affect-fn (case (:on-attack attacker)
                      [:poison]
                      poison-creature
                      [:steal-money]
                      (fn [c] (assoc c :gold (max 0 (- (:gold c) 10))))
                      identity)]
      (-> game
          (update id-from end-movement)
          (update id-to affect-fn)
          (update-in [id-to :health] #(max 0 (- % damage)))
          (show-message)
          (knockback-creature id-to dx dy)))))

(defn move-upstairs [game id]
  (let [creature (get game id)
        [ox oy] (:direction creature)]
    (if (= 1 (:dungeon-level creature))
      (do
        (swap-screen (:exit-screen game))
        game)
      (enter-store game))))

(defn exit-store-downstairs [game]
  (let [creature (get game :player)
        [ox oy] (:direction creature)
        stairs-from (if (= final-floor-depth (:dungeon-level creature)) :floor :stairs-up)
        stairs-to (if (= final-floor-depth (:dungeon-level creature)) :stairs-up :stairs-down)]
    (-> game
        (remove-enemies)
        (remove-items)
        (merge (generate-level (inc (:dungeon-level creature)) (inc (:dungeon-level creature)) (:enemy-catalog game) (:x creature) (:y creature) (+ ox (:x creature)) (+ oy (:y creature)) stairs-from stairs-to))
        (update-in [:player :dungeon-level] inc)
        (update-in [:player :difficulty] inc)
        (update-in [:player] unpoison-creature-once)
        (update-in [:player :x] #(max (inc min-x) (min (+ % ox) (- (global :width-in-characters) 2))))
        (update-in [:player :y] #(max (inc min-y) (min (+ % oy) (- (global :height-in-characters) 2))))
        (add-message :player (str "You go down into dungeon level " (inc (:dungeon-level creature)) ".")))))

(defn exit-store-upstairs [game]
  (let [creature (get game :player)
        [ox oy] (:direction creature)]
    (-> game
        (remove-enemies)
        (remove-items)
        (merge (generate-level (dec (:dungeon-level creature)) (inc (:dungeon-level creature)) (:enemy-catalog game) (:x creature) (:y creature) (+ ox (:x creature)) (+ oy (:y creature)) :stairs-down :stairs-up))
        (update-in [:player :dungeon-level] dec)
        (update-in [:player :difficulty] inc)
        (update-in [:player] unpoison-creature-once)
        (update-in [:player :x] #(max (inc min-x) (min (+ % ox) (- (global :width-in-characters) 2))))
        (update-in [:player :y] #(max (inc min-y) (min (+ % oy) (- (global :height-in-characters) 2))))
        (add-message :player (str "You go up to dungeon level " (inc (:dungeon-level creature)) ".")))))

(defn use-stairs [game id]
  (if (not= :player id)
    game
    (let [creature (get game id)
          [x y] [(:x creature) (:y creature)]]
      (case (get-in game [:grid [x y]])
        :stairs-down
        (if (> (:going-up creature) 0)
          game
          (enter-store game))
        :stairs-up
        (if (> (:going-up creature) 0)
          (move-upstairs game id)
          game)
        game))))

(defn open-door [game id x y]
  (let [c (get game id)
        msg (cond
             (and (= :player id) (:is-knocked-back c))
             "You fly back through the door."
             (= :player id)
             "You punch the door off its hinges."
             (:is-knocked-back c)
             (str "The " (creature-name c) " breaks through the door.")
             :else
             (str "The " (creature-name c) " opens the door."))]
    (if (= :door (get-in game [:grid [x y]]))
      (-> game
          (assoc-in [:grid [x y]] :floor)
          (add-message :player msg))
      game)))

(defn pickup-item [game id]
  (let [x (get-in game [id :x])
        y (get-in game [id :y])
        item (item-at game [x y])]
    (if (and (= :player id) item)
      (-> game
          (equip-item item)
          (dissoc (:id item))
          (update id end-movement)
          (spawn-creature-near x y))
      game)))

(defn acid-floor [game id x y]
  (if (and (= :acid-floor (get-in game [:grid [x y]])) (not (:ignore-acid-floor (get game id))))
    (update-in game [id :health] dec)
    game))

(defn apply-instadeath [game id x y]
  (let [msg (if (= :player id)
              "You are impailed on the wall spikes."
              (str "The " (creature-name (get game id)) " is impailed on the wall spikes."))]
    (if (:instadeath (tiles (get-in game [:grid [x y]])))
      (-> game
          (assoc-in [id :health] 0)
          (add-message id msg))
      game)))

(defn move-to [game id nx ny]
  (if (or (= 0 nx ny) (< nx 0) (< ny 0)
          (>= nx (global :width-in-characters)) (>= ny (global :height-in-characters))
          (nil? (get-in game [:grid [nx ny]])))
    game
    (let [x (get-in game [id :x])
          y (get-in game [id :y])
          direction [(- nx x) (- ny y)]
          target (get tiles (get-in game [:grid [nx ny]]) out-of-bounds)
          other-id (:id (creature-at game [nx ny]))]
      (cond
       (and (not (:is-knocked-back (get game id)))
            (= :web-floor (get-in game [:grid [x y]]))
            (not (:ignore-webs (get game id))))
       (-> game
           (assoc-in [:grid [x y]] :floor)
           (update id end-movement)
           (add-message id "You free yourself from the web."))
       (and (not (nil? other-id)) (not= id other-id))
       (attack-creature game id other-id)
       (:walkable target)
       (-> game
           (open-door id nx ny)
           (assoc-in [id :direction] direction)
           (assoc-in [id :x] nx)
           (assoc-in [id :y] ny)
           (acid-floor id nx ny)
           (apply-instadeath id nx ny)
           (use-stairs id)
           (pickup-item id))
       :else game))))

(defn move-by-path [game id]
  (let [step (first (get-in game [id :path]))]
    (-> game
        (move-to id (first step) (second step))
        (update-in [id :path] rest)
        (update-in [id] consume-step))))

(defn apply-replace-tiles-blast [game x y replacements radius]
  (let [get-points (if (any? #(= :wall %) (map first replacements))
                     (fn [dx dy] (bresenham x y (+ x dx) (+ y dy)))
                     (fn [dx dy] (until-blocked game (bresenham x y (+ x dx) (+ y dy)))))
        neighborhood (concat [[x y]]
                             (get-points radius 0)
                             (get-points radius radius)
                             (get-points 0 radius)
                             (get-points 0 0)
                             (get-points (- radius) 0)
                             (get-points (- radius) (- radius))
                             (get-points 0 (- radius))
                             (get-points (- radius) radius)
                             (get-points radius (- radius)))
        getter #(get replacements % %)
        add-effect-fn (fn [g [x y]] (add-effect g x y 12 "*" white nil))
        replace-fn (fn [g xy] (assoc-in g [:grid xy] (getter (get-in g [:grid xy]))))
        game (reduce add-effect-fn game neighborhood)
        game (reduce replace-fn game neighborhood)]
    (ensure-walls game)))

(defn apply-knockback-blast [game x y amount]
  (let [neighborhood (for [xo (range -2 3)
                           yo (range -2 3)]
                       [(+ x xo) (+ y yo)])
        creature-at (fn [g xy] (first (for [[id e] g
                                            :when (and (:is-creature e) (= xy [(:x e) (:y e)]))]
                                        e)))
        affect-fn (fn [g xy]
                    (let [c (creature-at game xy)
                          dx (if c (* amount (- (:x c) x)) 0)
                          dy (if c (* amount (- (:y c) y)) 0)]
                      (if c
                        (knockback-creature g (:id c) dx dy)
                        g)))
        add-effect-fn (fn [g [x y]] (add-effect g x y 3 "*" white nil))
        game (reduce add-effect-fn game neighborhood)
        game (reduce affect-fn game neighborhood)]
    game))

(defn apply-embiggen-blast [game x y]
  (let [neighborhood (for [xo (range -1 2)
                           yo (range -1 2)]
                       [(+ x xo) (+ y yo)])
        affect-fn (fn [g xy]
                    (let [c (creature-at game xy)]
                      (if c
                        (update g (:id c) embiggen-creature)
                        g)))
        add-effect-fn (fn [g [x y]] (add-effect g x y 3 "*" white nil))
        game (reduce add-effect-fn game neighborhood)
        game (reduce affect-fn game neighborhood)]
    game))

(defn apply-poison-blast [game x y]
  (let [neighborhood (for [xo (range -1 2)
                           yo (range -1 2)]
                       [(+ x xo) (+ y yo)])
        affect-fn (fn [g xy]
                    (let [c (creature-at game xy)]
                      (if c
                        (update g (:id c) poison-creature)
                        g)))
        add-effect-fn (fn [g [x y]] (add-effect g x y 3 "*" white nil))
        game (reduce add-effect-fn game neighborhood)
        game (reduce affect-fn game neighborhood)]
    game))

(defn apply-damage-blast [game x y amount]
  (let [neighborhood (for [xo (range -1 2)
                           yo (range -1 2)]
                       [(+ x xo) (+ y yo)])
        affect-fn (fn [g xy]
                    (let [c (creature-at game xy)]
                      (if c
                        (update-in g [(:id c) :health] dec)
                        g)))
        add-effect-fn (fn [g [x y]] (add-effect g x y 3 "*" white nil))
        game (reduce add-effect-fn game neighborhood)
        game (reduce affect-fn game neighborhood)]
    game))

(defn apply-summon-blast [game x y]
  (let [neighborhood (for [xo (range -1 2)
                           yo (range -1 2)]
                       [(+ x xo) (+ y yo)])
        affect-fn (fn [g [x y]]
                    (if (and (get-in game [:grid [x y] :walkable]) (creature-at game [x y]))
                      g
                      (spawn-creature-at g x y false)))
        add-effect-fn (fn [g [x y]] (add-effect g x y 3 "*" white nil))
        game (reduce add-effect-fn game neighborhood)
        game (reduce affect-fn game neighborhood)]
    game))

(defn apply-respawn [game creature]
  (let [c (unpoison-creature creature)
        c (assoc c
            :id (keyword "enemy-" (.toString (java.util.UUID/randomUUID)))
            :max-health (inc (:max-health creature 1))
            :health (inc (:max-health creature 1)))]
    (-> game
        (merge {(:id c) c})
        (add-message :player (str "The " (creature-name creature) " gets back up.")))))

(defn apply-on-death [game id]
  (let [creature (get game id)
        x (:x creature)
        y (:y creature)
        game (add-message game :player (str "The " (creature-name creature) " dies."))
        apply-effect (fn [g effect]
                       (case (first effect)
                         :replace-tiles
                         (apply-replace-tiles-blast g x y (second effect) (nth effect 2))
                         :knockback
                         (apply-knockback-blast g x y (second effect))
                         :embiggen
                         (apply-embiggen-blast g x y)
                         :poison
                         (apply-poison-blast g x y)
                         :damage
                         (apply-damage-blast g x y (second effect))
                         :summon-others
                         (apply-summon-blast g x y)
                         :respawn
                         (apply-respawn g creature)
                         g))]
    (reduce apply-effect game (:on-death creature))))

(defn remove-dead-enemies [game]
  (let [deads (for [[id e] game :when (and (not (= :player id)) (:health e) (< (:health e) 1))] id)]
    (reduce dissoc (reduce apply-on-death game deads) deads)))

(defn teleport-enemy [game id]
  (let [player (get game :player)
        creature (get game id)
        candidates (find-tiles :floor (:grid game))
        candidates (remove #(creature-at game %) candidates)
        candidates (if (< (rand) 0.5)
                     (filter (fn [[x y]] (nearby x y (:x player) (:y player) 1 3)) candidates)
                     (filter (fn [[x y]] (nearby x y (:x player) (:y player) 20 90)) candidates))
        [tx ty] (rand-nth candidates)]
    (-> game
        (add-effect (:x creature) (:y creature) 6 "*" light nil)
        (add-effect tx ty 6 "*" light nil)
        (move-to id tx ty))))

(defn ranged-attack [game id tx ty]
  (let [attacker (get game id)
        path (bresenham (:x attacker) (:y attacker) tx ty)]
    (add-projectile game (:x attacker) (:y attacker) path "*" red nil)))

(defn move-enemy [game id]
  (let [player (get game :player {:x 0 :y 0})
        ranged-target-x (+ (:x player) (rand-nth [-2 -1 0 1 2]))
        ranged-target-y (+ (:y player) (rand-nth [-2 -1 0 1 2]))
        can-see-target (creature-can-see-tile game id ranged-target-x ranged-target-y)]
    (cond
     (and (get-in game [id :teleportitis]) (< (rand) 0.2))
     (teleport-enemy game id)
     (and (get-in game [id :ranged-attack]) can-see-target)
     (ranged-attack game id ranged-target-x ranged-target-y)
     :else
     (let [c (get game id)
           occupied-by-ally (fn [xy] (any? #(= xy [(:x %) (:y %)]) (enemies game)))
           candidates (for [xo (range -1 2)
                            yo (range -1 2)
                            :when (not (= 0 xo yo))]
                        [(+ (:x c) xo) (+ (:y c) yo)])
           candidates (filter #(:walkable (get tiles (get-in game [:grid %]))) candidates)
           candidates (remove occupied-by-ally candidates)
           player (get game :player {:x 0 :y 0})
           toward-player (second (bresenham (:x c) (:y c) (:x player) (:y player)))
           [tx ty] (if (and (creature-can-see-creature game id :player) (not (occupied-by-ally toward-player)))
                     toward-player
                     (if (empty? candidates)
                       [0 0]
                       (rand-nth candidates)))]
       (move-to game id tx ty)))))

(defn move-enemies [game]
  (let [game (remove-dead-enemies game)
        ids (map :id (enemies game))]
    (reduce move-enemy game ids)))

(defn move-player-target [game mx my]
  (update game :target #(map + % [mx my])))

(defn move-player-to-target [game]
  (let [player (get game :player)
        points (take (:steps-remaining player) (rest (bresenham
                                                      (:x player)
                                                      (:y player)
                                                      (first (:target game))
                                                      (second (:target game)))))
        grid (:grid game)]
    (if (all? (fn [xy] (:walkable ((get grid xy out-of-bounds) tiles))) points)
      (assoc-in game [:player :path] points)
      game)))

(defn maybe-buy-item [game index]
  (let [player (:player game)
        item (nth (:store-items game) index)]
    (if (and item (<= (:price item) (:gold player)))
      (-> game
          (apply-purchace item)
          (assoc-in [:store-items index] nil))
      game)))

(defn new-game [win-screen store-screen]
  (let [g {:tick 0
           :enemy-catalog (new-enemy-catalog)
           :target [5 9]
           :store-items []
           :exit-screen win-screen
           :store-screen store-screen
           :messages []
           :player (new-player [5 9])}]
    (-> g
        (merge (generate-level 1 1 (:enemy-catalog g) 4 9 5 9 :stairs-up :stairs-down))
        (add-message :player "You are RUNNER_PUNCHER.")
        (add-message :player "Use the keyboard and enter or the mouse to move."))))
