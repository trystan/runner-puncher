(ns runner_puncher.actions)

(defn move-player-target [mx my]
  (swap! player-target-atom #(map + % [mx my])))

(defn open-door [game xy]
  (assoc-in game [:grid xy] :floor))

(defn move-creature-downstairs [game k]
  (let [creature (get game k)
        [ox oy] (:direction creature)]
    (-> game
        (assoc :grid (generate-level (:x creature) (:y creature) (+ ox (:x creature)) (+ oy (:y creature))))
        (update-in [k :x] #(max 1 (min (+ % ox) (- width-in-characters 2))))
        (update-in [k :y] #(max 1 (min (+ % oy) (- height-in-characters 2)))))))

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
