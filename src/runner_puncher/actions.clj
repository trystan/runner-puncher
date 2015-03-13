(ns runner_puncher.actions
  (:require [runner_puncher.framework :refer :all]
            [runner_puncher.worldgen :refer :all]))

(defn fix-store-prices [items]
  (vec (for [i (range 0 (count items))]
         (assoc (nth items i) :price (+ 5 (* 5 i))))))

(def store-atom (atom []))

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
          (update :player #(merge-with - % (:effect item)))))
    game))

(defn equip-item [game item]
  (let [set-slot (fn [g] (if (:slot item)
                           (assoc-in g [:player (:slot item)] item)
                           g))]
    (-> game
        (unequip-slot (:slot item))
        (set-slot)
        (update :player #(merge-with + % (:effect item))))))

(defn apply-purchace [game item]
  (-> game
      (update-in [:player :gold] #(- % (:price item)))
      (equip-item item)))

(defn restock-store-items [items]
  (fix-store-prices (take 9 (concat (drop 2 items) (repeatedly 20 (partial random-item true))))))

(defn enter-store [game]
  (swap! store-atom restock-store-items)
  (push-screen (:store-screen game)))

(defn creature-at [g xy]
  (first (for [[id e] g
               :when (and (:is-creature e) (= xy [(:x e) (:y e)]))]
           e)))

(defn enemies [game]
  (for [[k v] game :when (.startsWith (str k) ":enemy-")] v))

(defn item-at [game xy]
  (first (for [[id e] game
               :when (and (:is-item e) (= xy [(:x e) (:y e)]))]
           e)))

(defn items [game]
  (for [[k v] game :when (:is-item v)] v))

(defn add-message [game message]
  (update game :messages #(conj % {:text message :at (:tick game)})))

(defn nearby [x1 y1 x2 y2 minimum maximum]
  (let [distance-squared (+ (Math/pow (- x1 x2) 2) (Math/pow (- y1 y2) 2))]
    (<= (* minimum minimum) distance-squared (* maximum maximum))))

(defn spawn-creature-near [game x y]
  (let [candidates (filter (fn [[x2 y2]] (nearby x y x2 y2 4 9)) (find-tiles :floor (:grid game)))
        c (if (empty? candidates)
            nil
            (new-enemy (rand-nth candidates)))]
    (if c
      (into game [[(:id c) c]])
      game)))

(defn remove-items [game]
  (let [ids (map :id (items game))]
    (reduce dissoc game ids)))

(defn remove-enemies [game]
  (let [ids (map :id (enemies game))]
    (reduce dissoc game ids)))

(defn end-movement [creature]
  (-> creature
      (assoc :path [])
      (assoc :steps-remaining 0)
      (assoc :is-knocked-back false)))

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
      (update-in game [id] kb))))

(defn embiggen-creature [c]
  (if (= (clojure.string/upper-case (:char c)) (:char c))
    c
    (-> c
        (update :prefix #(str "Giant " (clojure.string/lower-case %)))
        (update :char clojure.string/upper-case)
        (update :health inc)
        (update :max-health inc)
        (assoc :immune-to-knockback true))))

(defn poison-creature [c]
  (-> c
      (assoc :poison-amount (inc (:poison-amount c 0)))
      (update :max-health dec)
      (update :health dec)
      (update :max-steps dec)
      (update :knockback-amount dec)))

(defn unpoison-creature-once [c]
  (if (> (:poison-amount c 0) 0)
    (-> c
      (update :poison-amount dec)
      (update :max-health inc)
      (update :health inc)
      (update :max-steps inc)
      (update :knockback-amount inc))
    c))

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
          (update-in [id-to :health] #(- % (:attack-damage attacker)))
          (knockback-creature id-to dx dy)))))

(defn move-downstairs [game k]
  (enter-store game)
  game)

(defn move-upstairs [game k]
  (let [creature (get game k)
        [ox oy] (:direction creature)]
    (if (= 1 (:dungeon-level creature))
      (swap-screen (:exit-screen game))
      (enter-store game)))
  game)

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

(defn use-stairs [game k]
  (if (not= :player k)
    game
    (let [creature (get game k)
          [x y] [(:x creature) (:y creature)]]
      (case (get-in game [:grid [x y]])
        :stairs-down
        (if (> (:going-up creature) 0)
          game
          (move-downstairs game k))
        :stairs-up
        (if (> (:going-up creature) 0)
          (move-upstairs game k)
          game)
        game))))

(defn open-door [game x y]
  (if (= :door (get-in game [:grid [x y]]))
    (-> game
        (assoc-in [:grid [x y]] :floor)
        (add-message "You punch the door off its hinges."))
    game))

(defn pickup-item [game k]
  (let [x (get-in game [k :x])
        y (get-in game [k :y])
        item (item-at game [x y])]
    (if (and (= :player k) item)
      (-> game
          (equip-item item)
          (dissoc (:id item))
          (update k end-movement)
          (spawn-creature-near (+ 2 x) (+ 2 y)))
      game)))

(defn move-to [game k nx ny]
  (let [x (get-in game [k :x])
        y (get-in game [k :y])
        direction [(- nx x) (- ny y)]
        target (get tiles (get-in game [:grid [nx ny]]) out-of-bounds)
        other-id (:id (creature-at game [nx ny]))]
    (cond
     (and (not (:is-knocked-back (get game k)))
          (= :web-floor (get-in game [:grid [x y]])))
     (-> game
         (assoc-in [:grid [x y]] :floor)
         (update k end-movement)
         (add-message "You free yourself from the web."))
     (and (not (nil? other-id)) (not= k other-id))
     (attack-creature game k other-id)
     (:walkable target)
     (-> game
         (update :tick inc)
         (open-door nx ny)
         (assoc-in [k :direction] direction)
         (assoc-in [k :x] nx)
         (assoc-in [k :y] ny)
         (use-stairs k)
         (pickup-item k))
     :else game)))

(defn consume-step [creature]
  (if (:is-knocked-back creature)
    (if (empty? (:path creature))
      (assoc creature :is-knocked-back false)
      creature)
    (let [creature (update creature :steps-remaining dec)]
      (if (< (:steps-remaining creature) 1)
        (end-movement creature)
        creature))))

(defn move-by-path [game k]
  (let [step (first (get-in game [k :path]))]
    (-> game
        (move-to k (first step) (second step))
        (update-in [k :path] rest)
        (update-in [k] consume-step))))

(defn apply-on-death [game id]
  (let [creature (get game id)
        x (:x creature)
        y (:y creature)]
    (case (first (:on-death creature))
      :replace-tiles
      (let [replacements (second (:on-death creature))
            neighborhood (for [xo (range -1 2)
                               yo (range -1 2)]
                           [(+ x xo) (+ y yo)])
            getter #(get replacements % %)
            replace-fn (fn [g xy]
                         (assoc-in g [:grid xy] (getter (get-in g [:grid xy]))))]
        (reduce replace-fn game neighborhood))
      :knockback
      (let [amount (second (:on-death creature))
            neighborhood (for [xo (range -1 2)
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
        (reduce affect-fn game neighborhood))
      :embiggen
      (let [neighborhood (for [xo (range -1 2)
                               yo (range -1 2)]
                           [(+ x xo) (+ y yo)])
            affect-fn (fn [g xy]
                        (let [c (creature-at game xy)]
                          (if c
                            (update g (:id c) embiggen-creature)
                            g)))]
        (reduce affect-fn game neighborhood))
      :poison
      (let [neighborhood (for [xo (range -1 2)
                               yo (range -1 2)
                               :when (not= xo yo 0)]
                           [(+ x xo) (+ y yo)])
            affect-fn (fn [g xy]
                        (let [c (creature-at game xy)]
                          (if c
                            (update g (:id c) poison-creature)
                            g)))]
        (reduce affect-fn game neighborhood))
      :damage
      (let [amount (second (:on-death creature))
            neighborhood (for [xo (range -1 2)
                               yo (range -1 2)
                               :when (not= xo yo 0)]
                           [(+ x xo) (+ y yo)])
            affect-fn (fn [g xy]
                        (let [c (creature-at game xy)]
                          (if c
                            (update-in g [(:id c) :health] dec)
                            g)))]
        (reduce affect-fn game neighborhood))
      game)))

(defn remove-dead-creatures [game]
  (let [deads (for [[id x] game :when (and (:health x) (< (:health x) 1))] id)]
    (reduce dissoc (reduce apply-on-death game deads) deads)))

(defn move-enemy [game k]
  (let [c (get game k)
        occupied-by-ally (fn [xy] (any? #(= xy [(:x %) (:y %)]) (enemies game)))
        candidates (for [xo (range -1 2)
                         yo (range -1 2)
                         :when (not (= 0 xo yo))]
                     [(+ (:x c) xo) (+ (:y c) yo)])
        candidates (filter #(:walkable (get tiles (get-in game [:grid %]))) candidates)
        candidates (remove occupied-by-ally candidates)
        toward-player (second (bresenham (:x c) (:y c) (get-in game [:player :x]) (get-in game [:player :y])))
        [tx ty] (if (and (creature-can-see-creature game k :player) (not (occupied-by-ally toward-player)))
                  toward-player
                  (rand-nth candidates))]
    (move-to game k tx ty)))

(defn move-enemies [game]
  (let [game (remove-dead-creatures game)
        ids (map :id (enemies game))]
    (reduce move-enemy game ids)))
