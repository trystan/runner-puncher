(ns runner_puncher.actions
  (:require [runner_puncher.framework :refer :all]
            [runner_puncher.worldgen :refer :all]))

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

(defn ensure-floor [game k]
  (let [creature (get game k)]
      (assoc-in game [:graph [(:x creature) (:y creature)]] :floor)))

(defn move-creature-downstairs [game k]
  (let [creature (get game k)
        [ox oy] (:direction creature)
        stairs-from (if (= final-floor-depth (:dungeon-level creature)) :floor :stairs-up)
        stairs-to (if (= final-floor-depth (:dungeon-level creature)) :stairs-up :stairs-down)]
    (-> game
        (update :tick inc)
        (assoc :grid (generate-level (:x creature) (:y creature) (+ ox (:x creature)) (+ oy (:y creature)) stairs-from stairs-to))
        (update-in [k :dungeon-level] inc)
        (assoc-in [k :moved] false)
        (update-in [k :x] #(max (inc min-x) (min (+ % ox) (- width-in-characters 2))))
        (update-in [k :y] #(max (inc min-y) (min (+ % oy) (- height-in-characters 2))))
        (ensure-floor k)
        (allow-going-up-stairs k))))

(defn move-creature-upstairs [game k exit-screen]
  (let [creature (get game k)
        [ox oy] (:direction creature)]
    (if (= 1 (:dungeon-level creature))
      (swap-screen exit-screen)
      (-> game
          (update :tick inc)
          (assoc :grid (generate-level (:x creature) (:y creature) (+ ox (:x creature)) (+ oy (:y creature)) :stairs-down :stairs-up))
          (update-in [k :dungeon-level] dec)
          (assoc-in [k :moved] false)
          (update-in [k :x] #(max (inc min-x) (min (+ % ox) (- width-in-characters 2))))
          (update-in [k :y] #(max (inc min-y) (min (+ % oy) (- height-in-characters 2))))
          (ensure-floor k)))))

(defn terrain-message [game k]
  (let [creature (get game k)
        tile (get-in game [:grid [(:x creature) (:y creature)]])]
    (cond
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
