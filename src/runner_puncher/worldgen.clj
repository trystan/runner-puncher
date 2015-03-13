(ns runner_puncher.worldgen
  (:require [runner_puncher.framework :refer :all]))

(def min-x 0)
(def min-y 1)
(def final-floor-depth 10)

(def out-of-bounds {:walkable false :name "unknown"})
(def tiles
  {:floor {:char (str (char 250))
           :name "floor"
           :walkable true
           :fg (hsl 220 25 50)
           :bg (hsl 220 25  5)}
   :wall {:char (str (char 4))
          :name "wall"
          :fg (hsl 220 33 60)
          :bg (hsl 220 33 40)}
   :door {:char "+"
          :name "door"
          :walkable true
          :fg (hsl 30 25 70)
          :bg (hsl 30 95 20)}
   :stairs-down {:char ">"
                 :name "stairs down"
                 :walkable true
                 :fg (hsl 220 25 50)
                 :bg (hsl 220 25  5)}
   :stairs-up {:char "<"
               :name "stairs up"
               :walkable true
               :fg (hsl 220 25 50)
               :bg (hsl 220 25  5)}
   :web-floor {:char "#"
               :name "web covered floor"
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
                \4 (rand-nth [:floor :floor :floor :wall])
                \5 (rand-nth [:floor :floor :floor :wall])
                \8 (rand-nth [:door :wall])
                \9 (rand-nth [:door :wall])}]
    (into {} (for [x (range (count (first rows)))
                   y (range (count rows))]
               [[x y] (lookup (nth (nth rows y) x))]))))

(defn flip-v [r]
  (let [rows (clojure.string/split r #"\n")
        h (count rows)
        new-rows (for [y (range 0 h)]
                   (apply str (reverse (nth rows y))))]
    (clojure.string/join "\n" new-rows)))

(defn flip-h [r]
  (let [rows (clojure.string/split r #"\n")]
    (clojure.string/join "\n" (reverse rows))))

(defn rotate [r]
  (let [rows (clojure.string/split r #"\n")
        h (count rows)
        w (count (first rows))
        turned (for [x (range 0 w)]
                 (for [y (range 0 h)]
                   (.charAt (nth (reverse rows) y) x)))
        turned (map #(clojure.string/join "" %) turned)]
    (clojure.string/join "\n" turned)))

(def room-list-1
  (apply list (set (mapcat (juxt flip-v (comp rotate flip-v) (comp rotate rotate flip-v) (comp rotate rotate rotate flip-v)
                                 flip-h (comp rotate flip-h) (comp rotate rotate flip-h) (comp rotate rotate rotate flip-h)
                                 identity rotate (comp rotate rotate) (comp rotate rotate rotate))
  [(str "#######+#######\n"
        "#1111.....2222#\n"
        "#11.........22#\n"
        "#1...........2#\n"
        "#1...........2#\n"
        "#.............#\n"
        "#.............#\n"
        "+.............+\n"
        "#.............#\n"
        "#.............#\n"
        "#4...........3#\n"
        "#4...........3#\n"
        "#44.........33#\n"
        "#4444.....3333#\n"
        "#######+#######")
   (str "####8####+####\n"
        "#222......222#\n"
        "#22...33...22#\n"
        "#2..........2#\n"
        "+...4....4...+\n"
        "#.....11.....#\n"
        "#.3..1111..3.#\n"
        "#.3..1111..3.#\n"
        "#.....11.....#\n"
        "+...4....4...+\n"
        "#2..........2#\n"
        "#22...33...22#\n"
        "#222......222#\n"
        "####+####9####")
   (str "####8####+####9####\n"
        "#1....2.....2....1#\n"
        "#..44...333...44..#\n"
        "+.4444.53335.4444.+\n"
        "#..44...333...44..#\n"
        "#1....2.....2....1#\n"
        "####9####+####8####")
   (str "#####8####9####8#####\n"
        "#11...............11#\n"
        "#1......33333......1#\n"
        "8.......33333.......9\n"
        "#.......33333.......#\n"
        "#...................#\n"
        "#.....222###222.....#\n"
        "#.....2#######2.....#\n"
        "9.....2#######2.....8\n"
        "#.....#########.....#\n"
        "#4...4#########4...4#\n"
        "###+#############+###")
   (str "###+###8###9##\n"
        "#1...2...2..1#\n"
        "+............9\n"
        "#............#\n"
        "##33333333..2#\n"
        "###3333333...#\n"
        "####333333...8\n"
        "#####33333...#\n"
        "######3333..2#\n"
        "#######333...#\n"
        "########33...+\n"
        "#########3...#\n"
        "##########..1#\n"
        "###########+##")
   (str "###9####8########\n"
        "#..........######\n"
        "#..........######\n"
        "+..........######\n"
        "#..........1#####\n"
        "#..........11####\n"
        "####11..........#\n"
        "#####1..........#\n"
        "######..........+\n"
        "######..........#\n"
        "######..........#\n"
        "########8####9###")
   (str "##############+#########\n"
        "#############..#########\n"
        "#############....#######\n"
        "#############......#####\n"
        "#....................###\n"
        "#......................#\n"
        "+......................+\n"
        "#......................#\n"
        "#....................###\n"
        "#############......#####\n"
        "#############....#######\n"
        "#############..#########\n"
        "##############+#########")
   (str "########+########\n"
        "######3...3######\n"
        "######.....######\n"
        "#####1.....1#####\n"
        "####11.....11####\n"
        "#3.............3#\n"
        "#...............#\n"
        "+...............+\n"
        "#...............#\n"
        "#3.............3#\n"
        "####11.....11####\n"
        "#####1.....1#####\n"
        "######.....######\n"
        "######3...3######\n"
        "########+########")
   (str "#########+#########\n"
        "#3...............3#\n"
        "#.................#\n"
        "#.......222.......#\n"
        "+.......222.......+\n"
        "#.......222.......#\n"
        "#.................#\n"
        "#3...............3#\n"
        "####11.......11####\n"
        "#####1.......1#####\n"
        "######.......######\n"
        "######3.....3######\n"
        "#########+#########")
   (str "#######+#######\n"
        "######...######\n"
        "#####.....#####\n"
        "####.......####\n"
        "###....3....###\n"
        "##....121....##\n"
        "+....32223....+\n"
        "##....121....##\n"
        "###....3....###\n"
        "####.......####\n"
        "#####.....#####\n"
        "######...######\n"
        "#######+#######")]))))

(defn room-list []
  (mapv room-to-tiles room-list-1))

(defn get-random-room []
  (rand-nth (room-list)))

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
       (all? identity (map second (merge-with (fn [a b] (or (= a b) (= :door a))) grid room)))
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

(defn grow-levels [levels number]
  (mapcat #(add-one-room % (take number (shuffle (room-list)))) levels))

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
    (loop [room room ox ox oy oy]
      (cond
       (< ox min-x)
       (recur room (inc ox) oy)
       (< oy min-y)
       (recur room ox (inc oy))
       (> (+ ox w 3) (global :width-in-characters))
       (recur room (dec ox) oy)
       (> (+ oy h 3) (global :height-in-characters))
       (recur room ox (dec oy))
       (not (:walkable (get tiles (get-in (position-room room ox oy) [[start-x start-y]]))))
       (position-start-room (get-random-room) stairs-x stairs-y start-x start-y stairs)
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
  (loop [levels (map #(position-start-room % stairs-x stairs-y start-x start-y stairs-from) (take 2 (shuffle (room-list))))
         rooms-remaining 4]
    (cond
     (= 0 (count levels))
     (recur (map #(position-start-room % stairs-x stairs-y start-x start-y stairs-from) (take 2 (shuffle (room-list))))
            4)
     (= 0 rooms-remaining)
     (fix-tiles (add-end-stairs (rand-nth levels) stairs-to))
     :else
     (recur (take 2 (grow-levels levels 2))
            (dec rooms-remaining)))))

(defn new-enemy [[x y]]
  (let [s 75
        l 75
        default {:is-creature true
                 :steps-remaining 1 :max-steps 1
                 :health 1 :max-health 1 :attack-damage 1
                 :knockback-amount 0 :poison-amount 0
                 :x x :y y :id (keyword "enemy-" (.toString (java.util.UUID/randomUUID)))}]
    (merge default (rand-nth [{:prefix "Web" :type "monster" :char "w" :fg (hsl 0 s l)
                               :description "Leaves webs behind when it dies."
                               :on-death [:replace-tiles {:floor :web-floor}]
                               :on-attack [:replace-tiles {:floor :web-floor}]}
                              {:prefix "Knockback" :type "monster" :char "k" :fg (hsl 60 s l)
                               :description "Knocks others back when it dies or attacks."
                               :on-death [:knockback 3] :knockback-amount 3}
                              {:prefix "Poison" :type "monster" :char "p" :fg (hsl 120 s l)
                               :description "Poisons others when it dies or attacks."
                               :on-death [:poison]
                               :on-attack [:poison]
                               :attack-damage 0}
                              {:prefix "Deadly" :type "monster" :char "d" :fg (hsl 180 s l)
                               :description "Does extra damage to others when it dies or attacks."
                               :on-death [:damage 1]
                               :attack-damage 2}
                              {:prefix "Embiggening" :type "monster" :char "e" :fg (hsl 240 s l)
                               :description "Embiggens others when it dies."
                               :on-death [:embiggen]}]))))


(defn random-item [is-store-item]
  (let [prefix (rand-nth [{:name "heavy" :description "Reduce movement by 1." :effect {:max-steps -1}}
                          {:name "sturdy" :description "Immune to acid." :effect {:resist-acid 1}}])
        noun (rand-nth [{:char "[" :slot "footwear" :name "shoes"}
                        {:char "]" :slot "armor" :name "cape"}
                        {:char "(" :slot "headwear" :name "helm"}
                        {:char ")" :slot "weapon" :name "knuckles"}])
        postfix (rand-nth [{:name "of running" :description "Increase movement by 2."
                            :effect {:max-steps 2}}
                           {:name "of punching" :description "Increase knockback by 2."
                            :effect {:knockback-amount 2}}
                           {:name "of life" :description "Increase life by 1."
                            :effect {:max-health 1 :health 1}}
                           {:name "of standing" :description "Resist knockback 50%."
                            :effect {:resist-knockback 1}}
                           {:name "of vitality" :description "Immunity to poison."
                            :effect {:ignore-poison 1}}
                           {:name "of charming" :description "Lower shop item costs by $3."
                            :effect {:affect-prices -3}}
                           {:name "of protection" :description "Reduce damage by 1 to minimum of 1."
                            :effect {:resist-damage 1}}
                           {:name "of webwalking" :description "Immunity to webs."
                            :effect {:ignore-webs 1}}])
        [prefix postfix] (cond
                          is-store-item
                          [prefix postfix]
                          (< (rand) 0.66)
                          [prefix nil]
                          :else
                          [nil postfix])
        item-name (:name noun)
        item-name (if prefix (str (:name prefix) " " item-name) item-name)
        item-name (if postfix (str item-name " " (:name postfix)) item-name)
        description ""
        description (if postfix (str description " " (:description postfix)) description)
        description (if prefix (str description " " (:description prefix)) description)
        effect {}
        effect (if prefix (merge-with + effect (:effect prefix)) effect)
        effect (if postfix (merge-with + effect (:effect postfix)) effect)]
  {:price 0 :name item-name :slot (:slot noun) :description description :effect effect :char (:char noun) :fg light}))


(defn make-amulet [[x y]]
  {:is-item true :name "amulet" :char "*" :fg (hsl 80 99 99)
   :description "The reason you are down here."
   :effect {:going-up 1}
   :x x :y y :id (keyword "item-" (.toString (java.util.UUID/randomUUID)))})

(defn new-gold [[x y]]
  {:is-item true :name "gold" :char "$" :fg (hsl 60 50 50)
   :x x :y y :id (keyword "item-" (.toString (java.util.UUID/randomUUID)))
   :effect {:gold (inc (rand-int 5))}
   :description "Good for buying things."})

(defn new-item [[x y] is-store-item]
  (let [default {:is-item true
                 :x x :y y :id (keyword "item-" (.toString (java.util.UUID/randomUUID)))}]
    (merge default (random-item is-store-item))))

(defn make-creatures [grid depth]
  (let [candidates (find-tiles :floor grid)
        positions (take (+ 8 (* 4 depth)) (shuffle candidates))]
    (into {} (for [c (map new-enemy positions)] [(:id c) c]))))

(defn make-treasures [grid depth]
  (let [candidates (shuffle (find-tiles :floor grid))
        gold-positions (take (+ 10 (* 2 depth)) candidates)
        item-positions (take (+ 10 depth) (drop (count gold-positions) candidates))]
    (merge (into {} (for [t (map #(new-item % false) item-positions)] [(:id t) t]))
           (into {} (for [t (map new-gold gold-positions)] [(:id t) t])))))

(defn replace-down-stairs-with-amulet [grid]
  (let [candidates (find-tiles :stairs-down grid)
        target (rand-nth candidates)
        grid (reduce (fn [g xy] (assoc g xy :floor)) grid candidates)
        amulet (make-amulet target)]
    [grid {(:id amulet) amulet}]))

(defn generate-level [depth stairs-x stairs-y start-x start-y stairs-from stairs-to]
  (let [grid (generate-grid stairs-x stairs-y start-x start-y stairs-from stairs-to)
        [grid amulet] (if (= final-floor-depth depth)
                        (replace-down-stairs-with-amulet grid)
                        [grid {}])]
    (merge {:grid grid} (make-creatures grid depth) (make-treasures grid depth) amulet)))
