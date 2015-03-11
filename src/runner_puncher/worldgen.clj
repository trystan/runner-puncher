(ns runner_puncher.worldgen
  (:require [runner_puncher.framework :refer :all]))

(def min-x 0)
(def min-y 1)
(def final-floor-depth 10)

(def out-of-bounds {:walkable false})
(def tiles
  {:floor {:char (str (char 250))
           :walkable true
           :fg (hsl 220 25 50)
           :bg (hsl 220 25  5)}
   :wall {:char (str (char 4))
          :fg (hsl 220 33 60)
          :bg (hsl 220 33 40)}
   :door {:char "+"
          :walkable true
          :fg (hsl 30 25 70)
          :bg (hsl 30 95 20)}
   :stairs-down {:char ">"
                 :walkable true
                 :fg (hsl 220 25 50)
                 :bg (hsl 220 25  5)}
   :stairs-up {:char "<"
                 :walkable true
                 :fg (hsl 220 25 50)
                 :bg (hsl 220 25  5)}
   :web-floor {:char "#"
               :walkable true
               :fg (hsl 220  5 60)
               :bg (hsl 220 25  5)}})

(defn room-to-tiles [room]
  (let [rows (seq (.split room "\n"))
        lookup {\# :wall
                \. :floor
                \+ :door
                \1 (rand-nth [:floor :floor :floor :wall])
                \2 (rand-nth [:floor :floor :floor :wall])
                \3 (rand-nth [:floor :floor :floor :wall])
                \4 (rand-nth [:floor :floor :floor :wall])}]
    (into {} (for [x (range (count (first rows)))
                   y (range (count rows))]
               [[x y] (lookup (nth (nth rows y) x))]))))


(defn room-list [] (mapv room-to-tiles
  [(str "######+######\n"
        "#1111...2222#\n"
        "#11.......22#\n"
        "#1.........2#\n"
        "#1.........2#\n"
        "#...........#\n"
        "+...........+\n"
        "#...........#\n"
        "#...........#\n"
        "#4.........3#\n"
        "#4.........3#\n"
        "#44.......33#\n"
        "#4444...3333#\n"
        "######+######")
   (str "####+####+####\n"
        "#222......222#\n"
        "#22...33...22#\n"
        "#2..........2#\n"
        "+...4....4...+\n"
        "#............#\n"
        "#.3...11...3.#\n"
        "#.3...11...3.#\n"
        "#............#\n"
        "+...4....4...+\n"
        "#2..........2#\n"
        "#22...33...22#\n"
        "#222......222#\n"
        "####+####+####")
   (str "####+####+####+####\n"
        "#1....2.....2....1#\n"
        "#.......333.......#\n"
        "+..44...333...44..+\n"
        "#.......333.......#\n"
        "#1....2.....2....1#\n"
        "####+####+####+####")
   (str "###+###\n"
        "#1...1#\n"
        "#.....#\n"
        "#..4..#\n"
        "+..4..+\n"
        "#.....#\n"
        "#2...2#\n"
        "#.....#\n"
        "#.333.#\n"
        "+.333.+\n"
        "#.333.#\n"
        "#.....#\n"
        "#2...2#\n"
        "#.....#\n"
        "+..4..+\n"
        "#..4..#\n"
        "#.....#\n"
        "#1...1#\n"
        "###+###")]))

(defn find-tiles [tile grid]
  (for [[xy t] grid :when (= t tile)] xy))

(defn all? [p coll]
  (= (count coll) (count (filter p coll))))

(defn any? [p coll]
  (> (count (filter p coll)) 0))

(defn map-keys [f m]
  (into {} (map (fn [[k v]] [(f k) v]) m)))

(defn position-room [room ox oy]
  (map-keys #(mapv + % [ox oy]) room))

(defn is-in-bounds? [x y]
  (and (<= min-x x (- (global :width-in-characters) 2)) (<= min-y y (- (global :height-in-characters) 2))))

(defn is-valid-placement [grid room]
  (and (any? nil? (for [[xy _] room] (get grid xy)))
       (all? identity (map second (merge-with (fn [a b] (or (= a b) (= :door b) (= :door a))) grid room)))
       (all? (fn [[x y]] (is-in-bounds? x y)) (map first room))))

(defn remove-extra-doors [grid dx dy]
  (let [doors (find-tiles :door grid)
        keep-door (fn [[x y]] (or (and (= dx x) (= dy y))
                                  (all? #(not (nil? %)) (for [ox [-1 0 1]
                                                              oy [-1 0 1]]
                                                          (get grid [(+ x ox) (+ y oy)])))))
        extras (remove keep-door doors)
        walls (into {} (for [xy extras] [xy :wall]))]
    (merge grid walls)))

(defn place-room [grid room gx gy rx ry]
  (let [[ox oy] (mapv - [gx gy] [rx ry])
        positioned (position-room room ox oy)
        placed (merge grid positioned)
        is-keeper (fn [[x y]] (any? nil? (for [ox [-1 0 1]
                                               oy [-1 0 1]]
                                           (get placed [(+ x ox) (+ y oy)]))))
        valids (filter is-keeper (find-tiles :door positioned))
        [door-x door-y] (if (empty? valids) [25 25] (rand-nth valids))]
    (if (is-valid-placement grid positioned)
      (remove-extra-doors placed door-x door-y)
      nil)))

(defn add-one-room [grid rooms]
  (remove nil?
          (for [[gx gy] (find-tiles :door grid)
                room rooms
                [rx ry] (find-tiles :door room)]
            (place-room grid room gx gy rx ry))))

(defn grow-levels [levels]
  (mapcat #(add-one-room % (room-list)) levels))

(defn position-start-room [room stairs-x stairs-y start-x start-y stairs]
  (let [w (- (apply max (map first (map first room))) 2)
        h (- (apply max (map second (map first room))) 2)
        ox (cond
            (< stairs-x start-x)
            stairs-x
            (> stairs-x start-x)
            (- stairs-x w 2)
            :else
            (- start-x (rand-int w) 1))
        oy (cond
            (< stairs-y start-y)
            stairs-y
            (> stairs-y start-y)
            (- stairs-y h 2)
            :else
            (- start-y (rand-int h) 1))]
    (loop [ox ox oy oy]
      (cond
       (< ox min-x)
       (recur (inc ox) oy)
       (< oy min-y)
       (recur ox (inc oy))
       (> (+ ox w 3) (global :width-in-characters))
       (recur (dec ox) oy)
       (> (+ oy h 3) (global :height-in-characters))
       (recur ox (dec oy))
       :else
       (merge (position-room room ox oy)
              {[start-x start-y] :floor}
              {[stairs-x stairs-y] stairs})))))

(defn add-end-stairs [grid tile]
  (let [is-candidate-door (fn [[x y]] (>= 3 (count (for [ox [-1 0 1]
                                                         oy [-1 0 1]
                                                         :let [tile (get grid [(+ x ox) (+ y oy)])]
                                                         :when (= tile :floor)]
                                                     tile))))
        positions (filter is-candidate-door (find-tiles :door grid))
        stairs (into {} (for [xy positions] [xy tile]))]
    (merge grid stairs)))

(defn fix-tiles [grid]
  (let [doors (concat (find-tiles :stairs-up grid) (find-tiles :stairs-down grid))
        fix-near-door (fn [grid [x y]]
                        (merge grid
                               (into {} (for [ox [-1 0 1]
                                              oy [-1 0 1]
                                              :let [tile (get grid [(+ x ox) (+ y oy)])]]
                                          [[(+ x ox) (+ y oy)] (or tile :wall)]))))
        fixed (reduce fix-near-door grid doors)
        walkable-neighbors (fn [x y] (count (for [ox [-1 0 1]
                                                  oy [-1 0 1]
                                                  :let [tile (get grid [(+ x ox) (+ y oy)])]
                                                  :when (:walkable (get tiles tile))]
                                       1)))]
    (into {} (for [[[x y] tile] fixed
                   :when (or (not= :wall tile) (< 0 (walkable-neighbors x y)))]
               [[x y] tile]))))

(defn generate-grid [stairs-x stairs-y start-x start-y stairs-from stairs-to]
  (loop [levels (map #(position-start-room % stairs-x stairs-y start-x start-y stairs-from) (room-list))
         rooms-remaining 4]
    (println (count levels))
    (cond
     (= 0 (count levels))
     (recur (map #(position-start-room % stairs-x stairs-y start-x start-y stairs-from) (room-list)) 5)
     (= 0 rooms-remaining)
     (fix-tiles (add-end-stairs (rand-nth levels) stairs-to))
     :else
     (recur (take 5 (shuffle (grow-levels levels))) (dec rooms-remaining)))))

(defn new-enemy [[x y]]
  (let [default {:is-creature true
                 :steps-remaining 1 :max-steps 1
                 :health 1 :max-health 1
                 :knockback-amount 0
                 :x x :y y :id (keyword "enemy-" (.toString (java.util.UUID/randomUUID)))}]
    (merge default (rand-nth [{:prefix "web" :char "w" :fg (hsl 0 66 66)
                :on-death [:replace-tiles {:floor :web-floor}]}
               {:prefix "knockback" :char "k" :fg (hsl 0 66 66)
                :on-death [:knockback 3]}
               {:prefix "growth" :char "g" :fg (hsl 0 66 66)
                :on-death [:growth]}]))))

(defn make-creatures [grid]
  (let [candidates (find-tiles :floor grid)
        positions (take 12 (shuffle candidates))]
    (into {} (for [c (map new-enemy positions)] [(:id c) c]))))

(defn generate-level [stairs-x stairs-y start-x start-y stairs-from stairs-to]
  (let [grid (generate-grid stairs-x stairs-y start-x start-y stairs-from stairs-to)]
    (merge {:grid grid} (make-creatures grid))))
