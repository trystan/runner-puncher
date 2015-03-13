(ns runner_puncher.actions
  (:require [runner_puncher.framework :refer :all]
            [runner_puncher.worldgen :refer :all]
            [runner_puncher.creatures :refer :all]
            [runner_puncher.items :refer :all]
            [runner_puncher.util :refer :all]))

(defn add-message [game message]
  (update game :messages #(conj % {:text message :at (:tick game)})))

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
                      (add-message g (str "You drop your " (:name existing-item) " and pick up " (:name item) "."))
                      (add-message g (str "You stop to pick up " (:name item)))))]
    (-> game
        (message-player)
        (unequip-slot (:slot item))
        (set-slot)
        (update :player #(merge-with + % (:effect item))))))

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


(defn spawn-creature-at [game x y]
  (let [c (new-enemy [x y])]
    (into game [[(:id c) c]])))

(defn spawn-creature-near [game x y]
  (let [candidates (filter (fn [[x2 y2]] (nearby x y x2 y2 4 9)) (find-tiles :floor (:grid game)))
        c (if (empty? candidates)
            nil
            (new-enemy (rand-nth candidates)))]
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

(defn creature-can-see-creature [game from-id to-id]
  (let [c1 (get game from-id)
        c2 (get game to-id)
        points (until-blocked game (bresenham (:x c1) (:y c1) (:x c2) (:y c2)))]
    (= [(:x c2) (:y c2)] (last points))))

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
          dx (- (:x attacked) (:x attacker))
          dy (- (:y attacked) (:y attacker))
          [dx dy] (mapv #(* (:knockback-amount attacker) %) [dx dy])
          affect-fn (if (= [:poison] (:on-attack attacker))
                      poison-creature
                      identity)]
      (-> game
          (update id-from end-movement)
          (update id-to affect-fn)
          (update-in [id-to :health] #(- % (max 1 (- (:attack attacker 1) (:defence attacked 0)))))
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
        (merge (generate-level (inc (:dungeon-level creature)) (:x creature) (:y creature) (+ ox (:x creature)) (+ oy (:y creature)) stairs-from stairs-to))
        (update-in [:player :dungeon-level] inc)
        (update-in [:player] unpoison-creature-once)
        (update-in [:player :x] #(max (inc min-x) (min (+ % ox) (- (global :width-in-characters) 2))))
        (update-in [:player :y] #(max (inc min-y) (min (+ % oy) (- (global :height-in-characters) 2))))
        (add-message (str "You decend to level " (inc (:dungeon-level creature)) ".")))))

(defn exit-store-upstairs [game]
  (let [creature (get game :player)
        [ox oy] (:direction creature)]
    (-> game
        (remove-enemies)
        (remove-items)
        (merge (generate-level (dec (:dungeon-level creature)) (:x creature) (:y creature) (+ ox (:x creature)) (+ oy (:y creature)) :stairs-down :stairs-up))
        (update-in [:player :dungeon-level] dec)
        (update-in [:player] unpoison-creature-once)
        (update-in [:player :x] #(max (inc min-x) (min (+ % ox) (- (global :width-in-characters) 2))))
        (update-in [:player :y] #(max (inc min-y) (min (+ % oy) (- (global :height-in-characters) 2))))
        (add-message (str "You acend to level " (inc (:dungeon-level creature)) ".")))))

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

(defn open-door [game x y]
  (if (= :door (get-in game [:grid [x y]]))
    (-> game
        (assoc-in [:grid [x y]] :floor)
        (add-message "You punch the door off its hinges."))
    game))

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

(defn move-to [game id nx ny]
  (if (= 0 nx ny)
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
           (add-message "You free yourself from the web."))
       (and (not (nil? other-id)) (not= id other-id))
       (attack-creature game id other-id)
       (:walkable target)
       (-> game
           (update :tick inc)
           (open-door nx ny)
           (assoc-in [id :direction] direction)
           (assoc-in [id :x] nx)
           (assoc-in [id :y] ny)
           (acid-floor id nx ny)
           (use-stairs id)
           (pickup-item id))
       :else game))))

(defn move-by-path [game id]
  (let [step (first (get-in game [id :path]))]
    (-> game
        (move-to id (first step) (second step))
        (update-in [id :path] rest)
        (update-in [id] consume-step))))

(defn apply-replace-tiles-blast [game x y replacements]
  (let [neighborhood (for [xo (range -1 2)
                           yo (range -1 2)]
                       [(+ x xo) (+ y yo)])
        getter #(get replacements % %)
        replace-fn (fn [g xy] (assoc-in g [:grid xy] (getter (get-in g [:grid xy]))))]
    (ensure-walls (reduce replace-fn game neighborhood))))

(defn apply-knockback-blast [game x y amount]
  (let [neighborhood (for [xo (range -1 2)
                           yo (range -1 2)
                           :when (not= xo yo 0)]
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
                        g)))]
    (reduce affect-fn game neighborhood)))

(defn apply-embiggen-blast [game x y]
  (let [neighborhood (for [xo (range -1 2)
                           yo (range -1 2)]
                       [(+ x xo) (+ y yo)])
        affect-fn (fn [g xy]
                    (let [c (creature-at game xy)]
                      (if c
                        (update g (:id c) embiggen-creature)
                        g)))]
    (reduce affect-fn game neighborhood)))

(defn apply-poison-blast [game x y ]
  (let [neighborhood (for [xo (range -1 2)
                           yo (range -1 2)
                           :when (not= xo yo 0)]
                       [(+ x xo) (+ y yo)])
        affect-fn (fn [g xy]
                    (let [c (creature-at game xy)]
                      (if c
                        (update g (:id c) poison-creature)
                        g)))]
    (reduce affect-fn game neighborhood)))

(defn apply-damage-blast [game x y amount]
  (let [neighborhood (for [xo (range -1 2)
                           yo (range -1 2)]
                       [(+ x xo) (+ y yo)])
        affect-fn (fn [g xy]
                    (let [c (creature-at game xy)]
                      (if c
                        (update-in g [(:id c) :health] dec)
                        g)))]
    (reduce affect-fn game neighborhood)))

(defn apply-summon-blast [game x y]
  (let [neighborhood (for [xo (range -1 2)
                           yo (range -1 2)]
                       [(+ x xo) (+ y yo)])
        affect-fn (fn [g [x y]]
                    (if (and (get-in game [:grid [x y] :walkable]) (creature-at game [x y]))
                      g
                      (spawn-creature-at g x y)))]
    (reduce affect-fn game neighborhood)))

(defn apply-on-death [game id]
  (let [creature (get game id)
        x (:x creature)
        y (:y creature)]
    (case (first (:on-death creature))
      :replace-tiles
      (apply-replace-tiles-blast game x y (second (:on-death creature)))
      :knockback
      (apply-knockback-blast game x y (second (:on-death creature)))
      :embiggen
      (apply-embiggen-blast game x y)
      :poison
      (apply-poison-blast game x y)
      :damage
      (apply-damage-blast game x y (second (:on-death creature)))
      :summon-others
      (apply-summon-blast game x y)
      game)))

(defn remove-dead-creatures [game]
  (let [deads (for [[id e] game :when (and (:health e) (< (:health e) 1))] id)]
    (reduce dissoc (reduce apply-on-death game deads) deads)))

(defn move-enemy [game id]
  (let [c (get game id)
        occupied-by-ally (fn [xy] (any? #(= xy [(:x %) (:y %)]) (enemies game)))
        candidates (for [xo (range -1 2)
                         yo (range -1 2)
                         :when (not (= 0 xo yo))]
                     [(+ (:x c) xo) (+ (:y c) yo)])
        candidates (filter #(:walkable (get tiles (get-in game [:grid %]))) candidates)
        candidates (remove occupied-by-ally candidates)
        toward-player (second (bresenham (:x c) (:y c) (get-in game [:player :x]) (get-in game [:player :y])))
        [tx ty] (if (and (creature-can-see-creature game id :player) (not (occupied-by-ally toward-player)))
                  toward-player
                  (if (empty? candidates)
                    [0 0]
                    (rand-nth candidates)))]
    (move-to game id tx ty)))

(defn move-enemies [game]
  (let [game (remove-dead-creatures game)
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
           :target [5 9]
           :store-items []
           :exit-screen win-screen
           :store-screen store-screen
           :messages []
           :player (new-player [5 9])}]
    (-> g
        (merge (generate-level 1 4 9 5 9 :stairs-up :stairs-down))
        (add-message "You are RUNNER_PUNCHER."))))
