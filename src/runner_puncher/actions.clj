(ns runner_puncher.actions
  (:require [runner_puncher.framework :refer :all]
            [runner_puncher.worldgen :refer :all]))

(defn open-door [game xy]
  (assoc-in game [:grid xy] :floor))

(defn allow-going-up-stairs [game k]
  (let [creature (get game k)]
    (if (= final-floor-depth (:dungeon-level creature))
      (assoc-in game [k :going-up] true)
      game)))

(defn move-creature-downstairs [game k]
  (let [creature (get game k)
        [ox oy] (:direction creature)
        stairs-from (if (= final-floor-depth (:dungeon-level creature)) :floor :stairs-up)
        stairs-to (if (= final-floor-depth (:dungeon-level creature)) :stairs-up :stairs-down)]
    (-> game
        (assoc :grid (generate-level (:x creature) (:y creature) (+ ox (:x creature)) (+ oy (:y creature)) stairs-from stairs-to))
        (update-in [k :dungeon-level] inc)
        (update-in [k :x] #(max (inc min-x) (min (+ % ox) (- width-in-characters 2))))
        (update-in [k :y] #(max (inc min-y) (min (+ % oy) (- height-in-characters 2))))
        (allow-going-up-stairs k))))

(defn move-creature-upstairs [game k exit-screen]
  (let [creature (get game k)
        [ox oy] (:direction creature)]
    (if (= 1 (:dungeon-level creature))
      (swap-screen exit-screen)
      (-> game
          (assoc :grid (generate-level (:x creature) (:y creature) (+ ox (:x creature)) (+ oy (:y creature)) :stairs-down :stairs-up))
          (update-in [k :dungeon-level] dec)
          (update-in [k :x] #(max (inc min-x) (min (+ % ox) (- width-in-characters 2))))
          (update-in [k :y] #(max (inc min-y) (min (+ % oy) (- height-in-characters 2))))))))

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
          (assoc-in [k :direction] direction)
          (assoc-in [k :x] nx)
          (assoc-in [k :y] ny))
      game)))

(defn move-by [game k mx my]
  (let [x (get-in game [k :x])
        y (get-in game [k :y])]
    (move-to game k (+ x mx) (+ y my))))
