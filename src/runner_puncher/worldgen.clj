(ns runner_puncher.worldgen
  (:require [runner_puncher.framework :refer :all]))

(def out-of-bounds {:walkable false})
(def tiles
  {:floor {:char (str (char 250))
           :walkable true
           :fg (hsl 210 5 50)
           :bg (hsl 210 5  5)}
   :wall {:char "#"
          :fg (hsl 210 5 50)
          :bg (hsl 210 5 20)}
   :door {:char "+"
          :walkable true
          :fg (hsl 30 10 70)
          :bg (hsl 30 80 20)}
   :stairs-down {:char ">"
                 :walkable true
                 :fg (hsl 210 5 50)
                 :bg (hsl 210 5  5)}
   :stairs-up {:char "<"
                 :walkable true
                 :fg (hsl 210 5 50)
                 :bg (hsl 210 5  5)}})

(def creatures
  {:player {:char "@"
            :fg {:r 250 :g 250 :b 250}}})

(defn room-to-tiles [room]
  (let [rows (seq (.split room "\n"))
        lookup {\# :wall
                \. :floor
                \+ :door}]
    (into {} (for [x (range (count (first rows)))
                   y (range (count rows))]
               [[x y] (lookup (nth (nth rows y) x))]))))


(def room-list (mapv room-to-tiles
  [(str "###+###+###+###\n"
        "#.............#\n"
        "#.............#\n"
        "+.............+\n"
        "#.............#\n"
        "#.............#\n"
        "###+###+###+###")
   (str "###+###\n"
        "#.....#\n"
        "#.....#\n"
        "#.....#\n"
        "+.....+\n"
        "#.....#\n"
        "#.....#\n"
        "#.....#\n"
        "+.....+\n"
        "#.....#\n"
        "#.....#\n"
        "#.....#\n"
        "+.....+\n"
        "#.....#\n"
        "#.....#\n"
        "##+####")]))

(defn find-tile [tile grid]
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
  (and (<= 0 x (- width-in-characters 2)) (<= 0 y (- height-in-characters 2))))

(defn is-valid-placement [grid room]
  (and (all? identity (map second (merge-with = grid room)))
       (all? (fn [[x y]] (is-in-bounds? x y)) (map first room))))

(defn remove-extra-doors [grid dx dy]
  (let [doors (find-tile :door grid)
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
        valids (filter is-keeper (find-tile :door positioned))
        [door-x door-y] (if (empty? valids) [25 25] (rand-nth valids))]
    (if (is-valid-placement grid positioned)
      (remove-extra-doors placed door-x door-y)
      nil)))

(defn add-one-room [grid rooms]
  (remove nil?
          (for [[gx gy] (find-tile :door grid)
                room rooms
                [rx ry] (find-tile :door room)]
            (place-room grid room gx gy rx ry))))

(defn grow-levels [levels]
  (mapcat #(add-one-room % room-list) levels))

(defn position-start-room [room stairs-x stairs-y start-x start-y]
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
       (< ox 0)
       (recur (inc ox) oy)
       (< oy 0)
       (recur ox (inc oy))
       (> (+ ox w 3) width-in-characters)
       (recur (dec ox) oy)
       (> (+ oy h 3) height-in-characters)
       (recur ox (dec oy))
       :else
       (merge (position-room room ox oy)
              {[stairs-x stairs-y] :stairs-up})))))

(defn add-downstaris [grid]
  (let [is-candidate-door (fn [[x y]] (>= 3 (count (for [ox [-1 0 1]
                                                         oy [-1 0 1]
                                                         :let [tile (get grid [(+ x ox) (+ y oy)])]
                                                         :when (= tile :floor)]
                                                     tile))))
        positions (filter is-candidate-door (find-tile :door grid))
        stairs (into {} (for [xy positions] [xy :stairs-down]))]
    (merge grid stairs)))

(defn fix-tiles [grid]
  (let [doors (concat (find-tile :stairs-up grid) (find-tile :stairs-down grid))
        fix-near-door (fn [grid [x y]]
                        (merge grid
                               (into {} (for [ox [-1 0 1]
                                              oy [-1 0 1]
                                              :let [tile (get grid [(+ x ox) (+ y oy)])]]
                                          [[(+ x ox) (+ y oy)] (or tile :wall)]))))]
    (reduce fix-near-door grid doors)))

(defn generate-level [stairs-x stairs-y start-x start-y]
  (loop [levels (map #(position-start-room % stairs-x stairs-y start-x start-y) room-list)
         rooms-remaining 5]
    (println (count levels))
    (cond
     (= 0 (count levels))
     (recur (map #(position-start-room % stairs-x stairs-y start-x start-y) room-list) 5)
     (= 0 rooms-remaining)
     (fix-tiles (add-downstaris (rand-nth levels)))
     :else
     (recur (take 10 (shuffle (grow-levels levels))) (dec rooms-remaining)))))
