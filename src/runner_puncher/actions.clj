(ns runner_puncher.actions
  (:require [runner_puncher.framework :refer :all]
            [runner_puncher.worldgen :refer :all]))

(defn enemies [game]
  (for [[k v] game :when (.startsWith (str k) ":enemy-")] v))

(defn add-message [game message]
  (update game :messages #(conj % {:text message :at (:tick game)})))

(defn open-door [game xy]
  (-> game
      (assoc-in [:grid xy] :floor)
      (add-message "You punch the door off its hinges.")))

(defn allow-going-up-stairs [game k]
  (let [creature (get game k)]
    (if (= final-floor-depth (:dungeon-level creature))
      (assoc-in game [k :going-up] true)
      game)))

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
  (let [kb (fn [c] (assoc c :path (until-blocked game (bresenham (:x c)
                                                                 (:y c)
                                                                 (+ (* 10 dx) (:x c))
                                                                 (+ (* 10 dy) (:y c))))))]
    (update-in game [id] kb)))

(defn attack-creature [game id-from id-to]
  (let [attacker (get game id-from)
        [dx dy] (:direction attacker)]
    (println id-from attacker dx dy)
    (-> game
        (assoc-in [id-from :path] [])
        (update-in [id-to :health] dec)
        (knockback-creature id-to dx dy))))

(defn move-creature-downstairs [game k]
  (let [creature (get game k)
        [ox oy] (:direction creature)
        stairs-from (if (= final-floor-depth (:dungeon-level creature)) :floor :stairs-up)
        stairs-to (if (= final-floor-depth (:dungeon-level creature)) :stairs-up :stairs-down)]
    (-> (remove-enemies game)
        (update :tick inc)
        (merge (generate-level (:x creature) (:y creature) (+ ox (:x creature)) (+ oy (:y creature)) stairs-from stairs-to))
        (update-in [k :dungeon-level] inc)
        (assoc-in [k :moved] false)
        (update-in [k :x] #(max (inc min-x) (min (+ % ox) (- width-in-characters 2))))
        (update-in [k :y] #(max (inc min-y) (min (+ % oy) (- height-in-characters 2))))
        (add-message (str "You decend to level " (inc (:dungeon-level creature)) "."))
        (ensure-floor k)
        (allow-going-up-stairs k))))

(defn move-creature-upstairs [game k exit-screen]
  (let [creature (get game k)
        [ox oy] (:direction creature)]
    (if (= 1 (:dungeon-level creature))
      (swap-screen exit-screen)
      (-> (remove-enemies game)
          (update :tick inc)
          (merge (generate-level (:x creature) (:y creature) (+ ox (:x creature)) (+ oy (:y creature)) :stairs-down :stairs-up))
          (update-in [k :dungeon-level] dec)
          (assoc-in [k :moved] false)
          (update-in [k :x] #(max (inc min-x) (min (+ % ox) (- width-in-characters 2))))
          (update-in [k :y] #(max (inc min-y) (min (+ % oy) (- height-in-characters 2))))
          (add-message (str "You acend to level " (inc (:dungeon-level creature)) "."))
          (ensure-floor k)))))

(defn terrain-message [game k]
  (let [creature (get game k)
        tile (get-in game [:grid [(:x creature) (:y creature)]])]
    (cond
     (not (:moved creature))
     game
     (and (= :stairs-up tile) (not (:going-up creature)))
     (add-message game "You can't go up until you get the TODO.")
     (and (= :stairs-down tile) (:going-up creature))
     (add-message game "You already have the TODO. Return to the surface.")
     :else
     game)))

(defn move-to [game k nx ny]
  (let [x (get-in game [k :x])
        y (get-in game [k :y])
        direction [(- nx x) (- ny y)]
        target (get tiles (get-in game [:grid [nx ny]]) out-of-bounds)
        game (if (= :door (get-in game [:grid [nx ny]]))
               (open-door game [nx ny])
               game)]
    (if (:walkable target)
      (-> game
          (update :tick inc)
          (assoc-in [k :moved] true)
          (assoc-in [k :direction] direction)
          (assoc-in [k :x] nx)
          (assoc-in [k :y] ny)
          (terrain-message k))
      game)))

(defn move-by [game k mx my]
  (let [x (get-in game [k :x])
        y (get-in game [k :y])]
    (move-to game k (+ x mx) (+ y my))))

(defn move-by-path [game k]
  (let [step (first (get-in game [k :path]))
        game (-> game
                 (move-to k (first step) (second step))
                 (update-in [k :path] rest))]
    (if (= :player k)
      (let [p (get game k)
            enemy (first (filter #(and (= (:x %) (:x p)) (= (:y %) (:y p))) (enemies game)))]
        (if enemy
          (attack-creature game k (:id enemy))
          game))
      game)))

(defn remove-dead-creatures [game]
  (let [deads (for [[id x] game :when (and (:health x) (< (:health x) 1))] id)]
    (reduce dissoc game deads)))
