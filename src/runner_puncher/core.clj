(ns runner_puncher.core
  (:import [java.awt Canvas Graphics]
           [java.awt.event])
  (:require [runner_puncher.framework :refer :all]
            [runner_puncher.renderer :refer :all]))

(declare start-screen play-screen win-screen lose-screen)

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

(defn new-game[]
  {:grid (generate-level 4 9 5 9)
   :player {:type :player :path [] :x 5 :y 9 :direction [0 0]}})

(def game-atom (atom new-game))

(def player-target-atom (atom [5 3]))

(defn render-grid-tile [t [[x y] tile]]
  (add-string t (:char (tile tiles)) x y (:fg (tile tiles)) (:bg (tile tiles))))

(defn render-grid [t grid]
  (reduce render-grid-tile t grid))

(defn render-creature [t creature]
  (let [c ((:type creature) creatures)]
    (add-string t (:char c) (:x creature) (:y creature) (:fg c) nil)))

(defn render-player-path [t points]
  (let [add-one (fn [t [x y]] (add-string t nil x y nil (hsl 60 40 40)))]
    (reduce add-one t points)))

(defn render-target-line [t [x0 y0] [x1 y1]]
  (let [points (bresenham x0 y0 x1 y1)
        grid (:grid @game-atom)
        ok (all? (fn [xy] (:walkable ((get grid xy out-of-bounds) tiles))) points)
        add-one (fn [t [x y]] (add-string t nil x y nil (if ok (hsl 60 40 40) (hsl 0 40 40))))]
    (reduce add-one t points)))

(defn render-play-screen []
  (let [game @game-atom
        [mx my] @player-target-atom
        player (:player game)
        render-target-fn (if (empty? (:path player))
                           #(render-target-line % [(:x player) (:y player)] [mx my])
                           #(render-player-path % (:path player)))]
    (-> {}
        (render-grid (:grid game))
        (render-creature player)
        (render-target-fn))))

(defn open-door [game xy]
  (assoc-in game [:grid xy] :floor))

(defn move-creature-downstairs [game k]
  (let [creature (get game k)
        [ox oy] (:direction creature)]
    (-> game
        (assoc :grid (generate-level (:x creature) (:y creature) (+ ox (:x creature)) (+ oy (:y creature))))
        (update-in [k :x] #(max 1 (min (+ % ox) (- width-in-characters 2))))
        (update-in [k :y] #(max 1 (min (+ % oy) (- height-in-characters 2)))))))

(defn move-player-target [mx my]
  (swap! player-target-atom #(map + % [mx my])))

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

(defn move-player-to-target []
  (let [points (bresenham
                (get-in @game-atom [:player :x])
                (get-in @game-atom [:player :y])
                (first @player-target-atom)
                (second @player-target-atom))
        grid (:grid @game-atom)
        ok (all? (fn [xy] (:walkable ((get grid xy out-of-bounds) tiles))) points)]
    (when ok
      (swap! game-atom assoc-in [:player :path] points))))

(defn key-press-play-screen [e]
  (when (empty? (get-in @game-atom [:player :path]))
    (case (to-keyword e)
      :W (swap-screen win-screen)
      :L (swap-screen lose-screen)
      :up    (move-player-target  0 -1)
      :down  (move-player-target  0  1)
      :left  (move-player-target -1  0)
      :right (move-player-target  1  0)
      :left-click (move-player-to-target)
      :enter (move-player-to-target)
      (println e))))

(defn mouse-move-play-screen [e]
  (when (empty? (get-in @game-atom [:player :path]))
    (reset! player-target-atom [(int (/ (.getX e) 12)) (int (/ (.getY e) 12))])))

(defn update-play-screen [e]
  (let [player (get @game-atom :player)]
    (cond
     (= :stairs-down (get-in @game-atom [:grid [(:x player) (:y player)]]))
     (swap! game-atom move-creature-downstairs :player)
     (not (empty? (:path player)))
     (let [step (first (get-in @game-atom [:player :path]))]
       (swap! game-atom move-to :player (first step) (second step))
       (swap! game-atom update-in [:player :path] rest)))))




(def start-screen {:on-render (fn [] (-> {}
                                  (add-center-string "RUNNER_PUNCHER" 2)
                                  (add-center-string "A 2015 7DRL by Trystan Spangler" 3)
                                  (add-center-string "-- press Enter to start --" (- height-in-characters 2))))
                   :on-key-press (fn [e]
                                  (case (to-keyword e)
                                    :enter (do
                                             (reset! game-atom (new-game))
                                             (swap-screen play-screen))
                                    (println e)))})

(def play-screen {:on-render render-play-screen
                  :on-key-press key-press-play-screen
                  :on-mouse-move mouse-move-play-screen
                  :on-timer update-play-screen})

(def win-screen {:on-render (fn [] (-> {}
                                       (add-center-string "You won" 2)
                                       (add-center-string "-- press Enter to start again --" (- height-in-characters 2))))
                 :on-key-press (fn [e]
                                (case (to-keyword e)
                                  :enter (do
                                             (reset! game-atom (new-game))
                                             (swap-screen play-screen))
                                  (println e)))})

(def lose-screen {:on-render (fn [] (-> {}
                                        (add-center-string "You lost" 2)
                                        (add-center-string "-- press Enter to start again --" (- height-in-characters 2))))
                  :on-key-press (fn [e]
                                 (case (to-keyword e)
                                   :enter (do
                                             (reset! game-atom (new-game))
                                             (swap-screen play-screen))
                                   (println e)))})


(defn -main []
  (start-game start-screen))
