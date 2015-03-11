(ns runner_puncher.actions
  (:require [runner_puncher.framework :refer :all]
            [runner_puncher.worldgen :refer :all]))

(defn enemies [game]
  (for [[k v] game :when (.startsWith (str k) ":enemy-")] v))

(defn add-message [game message]
  (update game :messages #(conj % {:text message :at (:tick game)})))

(defn remove-enemies [game]
  (let [ids (map :id (enemies game))]
    (reduce dissoc game ids)))

(defn ensure-floor [game k]
  (let [creature (get game k)]
    (assoc-in game [:graph [(:x creature) (:y creature)]] :floor)))

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

(defn knockback-creature [game id dx dy]
  (let [kb (fn [c] (-> c
                       (assoc :is-knocked-back true)
                       (assoc :path (until-blocked game (bresenham (:x c)
                                                                   (:y c)
                                                                   (+ dx (:x c))
                                                                   (+ dy (:y c)))))))]
    (update-in game [id] kb)))

(defn attack-creature [game id-from id-to]
  (let [attacker (get game id-from)
        attacked (get game id-to)
        dx (- (:x attacked) (:x attacker))
        dy (- (:y attacked) (:y attacker))
        [dx dy] (mapv #(* (:knockback-amount attacker) %) [dx dy])]
    (-> game
        (assoc-in [id-from :path] [])
        (update-in [id-to :health] dec)
        (knockback-creature id-to dx dy))))

(defn maybe-allow-going-up-stairs [game k]
  (let [creature (get game k)]
    (if (= final-floor-depth (:dungeon-level creature))
      (assoc-in game [k :going-up] true)
      game)))

(defn move-downstairs [game k]
  (let [creature (get game k)
        [ox oy] (:direction creature)
        stairs-from (if (= final-floor-depth (:dungeon-level creature)) :floor :stairs-up)
        stairs-to (if (= final-floor-depth (:dungeon-level creature)) :stairs-up :stairs-down)]
    (-> (remove-enemies game)
        (merge (generate-level (:x creature) (:y creature) (+ ox (:x creature)) (+ oy (:y creature)) stairs-from stairs-to))
        (update-in [k :dungeon-level] inc)
        (update-in [k :x] #(max (inc min-x) (min (+ % ox) (- (global :width-in-characters) 2))))
        (update-in [k :y] #(max (inc min-y) (min (+ % oy) (- (global :height-in-characters) 2))))
        (add-message (str "You decend to level " (inc (:dungeon-level creature)) "."))
        (ensure-floor k)
        (maybe-allow-going-up-stairs k))))

(defn move-upstairs [game k]
  (let [creature (get game k)
        [ox oy] (:direction creature)]
    (if (= 1 (:dungeon-level creature))
      (swap-screen (:exit-screen game))
      (-> (remove-enemies game)
          (merge (generate-level (:x creature) (:y creature) (+ ox (:x creature)) (+ oy (:y creature)) :stairs-down :stairs-up))
          (update-in [k :dungeon-level] dec)
          (update-in [k :x] #(max (inc min-x) (min (+ % ox) (- (global :width-in-characters) 2))))
          (update-in [k :y] #(max (inc min-y) (min (+ % oy) (- (global :height-in-characters) 2))))
          (add-message (str "You acend to level " (inc (:dungeon-level creature)) "."))
          (ensure-floor k)))))

(defn use-stairs [game k]
  (if (not= :player k)
    game
    (let [creature (get game k)
          [x y] [(:x creature) (:y creature)]]
      (case (get-in game [:grid [x y]])
        :stairs-down
        (if (:going-up creature)
          game
          (move-downstairs game k))
        :stairs-up
        (if (:going-up creature)
          (move-upstairs game k)
          game)
        game))))

(defn open-door [game x y]
  (if (= :door (get-in game [:grid [x y]]))
    (-> game
        (assoc-in [:grid [x y]] :floor)
        (add-message "You punch the door off its hinges."))
    game))

(defn step-on-web [game k x y]
  (if (and (not (:is-knocked-back (get game k)))
           (= :web-floor (get-in game [:grid [x y]])))
    (-> game
        (assoc-in [:grid [x y]] :floor)
        (assoc-in [k :path] [])
        (assoc-in [k :steps-remaining] 0)
        (add-message "You get stuck in the web."))
    game))

(defn move-to [game k nx ny]
  (let [x (get-in game [k :x])
        y (get-in game [k :y])
        direction [(- nx x) (- ny y)]
        target (get tiles (get-in game [:grid [nx ny]]) out-of-bounds)
        other-id (first (for [[id e] game
                              :when (and (= nx (:x e)) (= ny (:y e)))]
                          id))]
    (if (and (not (nil? other-id)) (not= k other-id))
      (attack-creature game k other-id)
      (if (:walkable target)
        (-> game
            (update :tick inc)
            (assoc-in [k :direction] direction)
            (assoc-in [k :x] nx)
            (assoc-in [k :y] ny)
            (open-door nx ny)
            (step-on-web k nx ny)
            (use-stairs k))
        game))))

(defn consume-step [creature]
  (if (:is-knocked-back creature)
    (if (empty? (:path creature))
      (assoc creature :is-knocked-back false)
      creature)
    (let [creature (update creature :steps-remaining dec)]
      (if (< (:steps-remaining creature) 1)
        (assoc creature :path [])
        creature))))

(defn move-by-path [game k]
  (let [step (first (get-in game [k :path]))]
    (-> game
        (move-to k (first step) (second step))
        (update-in [k :path] rest)
        (update-in [k] consume-step))))

(defn move-enemy [game k]
  (let [c (get game k)
        occupied-by-ally (fn [xy] (any? #(= xy [(:x %) (:y %)]) (enemies game)))
        candidates (for [xo (range -1 2)
                         yo (range -1 2)
                         :when (not (= 0 xo yo))]
                     [(+ (:x c) xo) (+ (:y c) yo)])
        candidates (filter #(:walkable (get tiles (get-in game [:grid %]))) candidates)
        candidates (remove occupied-by-ally candidates)
        player-xy [(get-in game [:player :x]) (get-in game [:player :y])]
        [tx ty] (or (first (filter #(= player-xy %) candidates)) (rand-nth candidates))]
    (if (= player-xy [tx ty])
      (attack-creature game k :player)
      (move-to game k tx ty))))

(defn move-enemies [game]
  (let [ids (map :id (enemies game))]
    (reduce move-enemy game ids)))

(defn apply-on-death [game id]
  (let [creature (get game id)
        x (:x creature)
        y (:y creature)]
    (case (first (:on-death (get creatures (:type creature))))
      :replace-tiles
      (let [replacements (second (:on-death (get creatures (:type creature))))
            neighborhood (for [xo (range -1 2)
                               yo (range -1 2)]
                           [(+ x xo) (+ y yo)])
            getter #(get replacements % %)
            replace-fn (fn [g xy]
                         (assoc-in g [:grid xy] (getter (get-in g [:grid xy]))))]
        (reduce replace-fn game neighborhood))
      :knockback
      (let [amount (second (:on-death (get creatures (:type creature))))
            neighborhood (for [xo (range -1 2)
                               yo (range -1 2)
                               :when (not= xo yo 0)]
                           [(+ x xo) (+ y yo)])
            creature-at (fn [g xy] (first (for [[id e] g
                                                :when (and (:is-creature e) (= xy [(:x e) (:y e)]))]
                                            e)))
            affect-fn (fn [g xy]
                        (let [c (creature-at game xy)
                              _ (println amount x y c)
                              dx (if c (* amount (- (:x c) x)) 0)
                              dy (if c (* amount (- (:y c) y)) 0)]
                          (if c
                            (knockback-creature g (:id c) dx dy)
                            g)))]
        (reduce affect-fn game neighborhood))
      game)))

(defn remove-dead-creatures [game]
  (let [deads (for [[id x] game :when (and (:health x) (< (:health x) 1))] id)]
    (reduce dissoc (reduce apply-on-death game deads) deads)))

