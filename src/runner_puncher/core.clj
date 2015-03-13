(ns runner_puncher.core
  (:import [java.awt Canvas Graphics Toolkit]
           [java.awt.event])
  (:require [runner_puncher.framework :refer :all]
            [runner_puncher.renderer :refer :all]
            [runner_puncher.worldgen :refer :all]
            [runner_puncher.actions :refer :all])
  (:gen-class))

(declare start-screen play-screen store-screen win-screen lose-screen)

(defn describe-creature [creature]
  (let [d (:description creature)
        d (str d (if (:immune-to-knockback creature) " Immune to knockback." ""))]
    d))

(defn new-game []
  (let [g {:tick 0
           :exit-screen win-screen
           :store-screen store-screen
           :messages []
           :player {:prefix "Player" :type "" :char "@" :fg {:r 250 :g 250 :b 250}
                    :description "Trying to find something on the 10th dungeon level."
                    :id :player :knockback-amount 5 :poison-amount 0 :attack 1 :defence 0
                    :is-creature true :going-up 0 :path [] :gold 0
                    :health 3 :max-health 3 :steps-remaining 5 :max-steps 5
                    :x 5 :y 9 :dungeon-level 1 :direction [0 0]}}]
    (-> g
        (merge (generate-level 1 4 9 5 9 :stairs-up :stairs-down))
        (add-message "You are RUNNER_PUNCHER."))))

(def game-atom (atom []))

(def player-target-atom (atom [5 9]))

(defn move-player-target [mx my]
  (swap! player-target-atom #(map + % [mx my])))

(defn render-grid-tile [t [[x y] tile]]
  (if (= 0 (mod (+ x y) 2))
    (add-string t (:char (tile tiles)) x y (:fg (tile tiles)) (:bg (tile tiles)))
    (add-string t (:char (tile tiles)) x y
                (merge-with + {:r 5 :g 5 :b 5} (:fg (tile tiles)))
                (merge-with + {:r 5 :g 5 :b 5} (:bg (tile tiles))))))

(defn render-grid [t grid]
  (reduce render-grid-tile t grid))

(defn render-item [t item]
  (add-string t (:char item) (:x item) (:y item) (:fg item) nil))

(defn render-items [t items]
  (reduce render-item t items))

(defn render-creature [t creature]
  (add-string t (:char creature) (:x creature) (:y creature) (:fg creature) nil))

(defn render-creatures [t creatures]
  (reduce render-creature t creatures))

(defn render-player-path [t points]
  (let [add-one (fn [t [x y]] (add-string t nil x y nil (hsl 60 40 40)))]
    (reduce add-one t points)))

(defn render-target-line [t points]
  (let [grid (:grid @game-atom)
        ok (all? (fn [xy] (:walkable ((get grid xy out-of-bounds) tiles))) points)
        add-one (fn [t [x y]] (add-string t nil x y nil (if ok (hsl 60 40 40) (hsl 0 40 40))))]
    (reduce add-one t points)))

(defn render-counters [t current maximum poison c color x y]
  (-> t
      (add-string (apply str (repeat current c))
                  x y color nil)
      (add-string (apply str (repeat (- maximum current) c))
                  (+ x current) y fg nil)
      (add-string (apply str (repeat poison c))
                  (+ x maximum) y green nil)))

(defn render-health [t current maximum poison x y]
  (render-counters t current maximum poison (char 3) red x y))

(defn render-hud [t player]
  (let [width-in-characters (global :width-in-characters)
        heart-start (- width-in-characters (:max-health player) (:poison-amount player) 5 1)
        puncher-start (- heart-start (:knockback-amount player) (:poison-amount player) 6 3)
        runner-start (- puncher-start (:max-steps player) (:poison-amount player) 4 3)]
    (-> t
        (add-string (apply str (repeat width-in-characters " ")) 0 0 fg (hsl 45 25 25))
        (add-string (str "Level " (:dungeon-level player)) 1 0 fg nil)

        (add-string (str "$" (:gold player)) (- runner-start 4) 0 (hsl 60 50 50) nil)

        (add-string "run" runner-start 0 fg nil)
        (render-counters (:steps-remaining player) (:max-steps player) (:poison-amount player)
                         (char 4) blue (+ runner-start 4) 0)

        (add-string "punch" puncher-start 0 fg nil)
        (render-counters (:knockback-amount player) (:knockback-amount player) (:poison-amount player)
                         (char 7) blue (+ puncher-start 6) 0)

        (add-string "live" heart-start 0 fg nil)
        (render-health (:health player) (:max-health player) (:poison-amount player) (+ heart-start 5) 0))))

(defn slot-description [creature slot]
  (if (get creature slot)
    (str (clojure.string/capitalize slot) ": " (get-in creature [slot :name]) " - " (get-in creature [slot :description]))
    (str (clojure.string/capitalize slot) ": none")))

(defn render-target [t game mx my]
  (let [c (creature-at game [mx my])
        creature-name (.trim (if c (str (:prefix c) " " (:type c)) ""))
        item (item-at game [mx my])
        tile (get tiles (get-in game [:grid [mx my]]))
        x 1
        y (- (global :height-in-characters) 9)
        y (if (> my y) 4 y)]
    (cond
     c
     (-> t
         (add-string creature-name x (+ y 0) fg bg)
         (render-health (:health c) (:max-health c) (:poison-amount c)
                        (+ (count creature-name) x 1) (+ y 0))
         (add-string (str " Attack " (:attack c)) (+ (count creature-name) (:max-health c) (:poison-amount c) 2) (+ y 0) fg bg)
         (add-string (str " Defence " (:defence c)) (+ (count (str " Attack " (:attack c))) (count creature-name) (:max-health c) (:poison-amount c) 2) (+ y 0) fg bg)
         (add-string (describe-creature c) x (+ y 1) fg bg)
         (add-string (slot-description c "headwear") x (+ y 2) fg bg)
         (add-string (slot-description c "armor") x (+ y 3) fg bg)
         (add-string (slot-description c "footwear") x (+ y 4) fg bg)
         (add-string (slot-description c "weapon") x (+ y 5) fg bg)
         (add-string (str "Standing on " (:name tile)) x (+ y 6) fg bg))
     item
     (-> t
         (add-string (:name item) x (+ y 0) fg bg)
         (add-string (:description item) x (+ y 1) fg bg)
         (add-string (str "On " (:name tile)) x (+ y 2) fg bg))
     :else
     (-> t
         (add-string (:name tile) 2 (+ y 0) fg bg)))))

(defn render-messages [t at-top messages]
  (let [most-recent (:at (last (sort-by :at messages)))]
    (loop [t t
           messages (filter #(= most-recent (:at %)) messages)
           y (if at-top
               min-y
               (- (global :height-in-characters) (count messages)))]
      (if (empty? messages)
        t
        (recur
         (add-center-string t (:text (first messages)) y)
         (rest messages)
         (inc y))))))

(defn render-play-screen []
  (let [game @game-atom
        [mx my] @player-target-atom
        player (:player game)
        line (take (:steps-remaining player) (drop 1 (bresenham (:x player) (:y player) mx my)))
        render-target-fn (if (empty? (:path player))
                           #(render-target-line % line)
                           #(render-player-path % (:path player)))]
    (-> {}
        (render-grid (:grid game))
        (render-items (items game))
        (render-creatures (enemies game))
        (render-creature player)
        (render-target-fn)
        (render-hud player)
        (render-target game mx my)
        (render-messages (> (:y player) (* 0.75 (global :height-in-characters))) (:messages game)))))



(defn move-player-to-target []
  (let [player (get @game-atom :player)
        points (take (:steps-remaining player) (rest (bresenham
                                                      (:x player)
                                                      (:y player)
                                                      (first @player-target-atom)
                                                      (second @player-target-atom))))
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
    (reset! player-target-atom [(int (/ (.getX e) (global :tile-width)))
                                (int (/ (.getY e) (global :tile-height)))])))

(defn update-play-screen [e]
  (let [player (get @game-atom :player)
        creatures (conj (enemies @game-atom) player)]
    (cond
     (not (all? empty? (map :path creatures)))
     (doseq [c creatures
             :when (and (seq? (:path c)) (> (count (:path c)) 0))]
       (swap! game-atom move-by-path (:id c)))
     (< (:steps-remaining player) 1)
     (do
       (swap! game-atom move-enemies)
       (swap! game-atom assoc-in [:player :steps-remaining] (:max-steps player)))
     :else
     (swap! game-atom remove-dead-creatures))
    (when (< (get-in @game-atom [:player :health]) 1)
      (swap-screen lose-screen))))




(def start-screen {:on-render (fn [] (-> {}
                                  (add-center-string "RUNNER_PUNCHER" 2)
                                  (add-center-string "A 2015 7DRL by Trystan Spangler" 3)
                                  (add-center-string "-- press Enter to start --" (- (global :height-in-characters) 2))))
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

(defn render-store-item [t [index item]]
  (if item
    (-> t
        (add-string (str (inc index) ". ($" (:price item) ") " (:name item)) 5 (+ (* 2 index) 7) white nil)
        (add-string (str "    " (:description item)) 5 (+ (* 2 index) 8) fg nil))
    (add-string t (str (inc index) ". -- out of stock --") 5 (+ (* 2 index) 7) fg nil)))

(defn render-store-items [t items]
  (let [x 5
        y 25
        player (get @game-atom :player)
        t (reduce render-store-item t (for [i (range 0 (count items))] [i (nth items i)]))]
    (-> t
        (add-string (str "You have $" (get-in @game-atom [:player :gold]) ". You are currently wearing:") x (+ y 1) white bg)
        (add-string (slot-description player "headwear") x (+ y 2) fg bg)
        (add-string (slot-description player "armor") x (+ y 3) fg bg)
        (add-string (slot-description player "footwear") x (+ y 4) fg bg)
        (add-string (slot-description player "weapon") x (+ y 5) fg bg))))

(defn maybe-buy-item [index]
  (let [player (get @game-atom :player)
        item (nth @store-atom index)]
    (when (<= (:price item) (:gold player))
      (swap! game-atom apply-purchace item)
      (swap! store-atom assoc index nil))))

(def store-screen {:on-render (fn [] (-> {}
                                       (add-center-string "Welcome to the store between levels!" 2)
                                       (render-store-items @store-atom)
                                       (add-center-string "Press 1 through 9 to buy something." 4)
                                       (add-center-string "-- press Enter to go to the next level --" (- (global :height-in-characters) 2))))
                 :on-key-press (fn [e]
                                (case (to-keyword e)
                                  :1 (maybe-buy-item 0)
                                  :2 (maybe-buy-item 1)
                                  :3 (maybe-buy-item 2)
                                  :4 (maybe-buy-item 3)
                                  :5 (maybe-buy-item 4)
                                  :6 (maybe-buy-item 5)
                                  :7 (maybe-buy-item 6)
                                  :8 (maybe-buy-item 7)
                                  :9 (maybe-buy-item 8)
                                  :enter (do
                                           (pop-screen)
                                           (if (> (get-in @game-atom [:player :going-up]) 0)
                                             (swap! game-atom exit-store-upstairs)
                                             (swap! game-atom exit-store-downstairs)))
                                  (println e)))})

(def win-screen {:on-render (fn [] (-> {}
                                       (add-center-string "You won" 2)
                                       (add-center-string "-- press Enter to start again --" (- (global :height-in-characters) 2))))
                 :on-key-press (fn [e]
                                (case (to-keyword e)
                                  :enter (do
                                             (reset! game-atom (new-game))
                                             (swap-screen play-screen))
                                  (println e)))})

(def lose-screen {:on-render (fn [] (-> {}
                                        (add-center-string "You lost" 2)
                                        (add-center-string "-- press Enter to start again --" (- (global :height-in-characters) 2))))
                  :on-key-press (fn [e]
                                 (case (to-keyword e)
                                   :enter (do
                                             (reset! game-atom (new-game))
                                             (swap-screen play-screen))
                                   (println e)))})

(defn round-down [x values]
  (last (filter #(<= % x) values)))

(defn to-int [string]
  (Integer/parseInt string))

(defn -main [& args]
  (let [options (apply hash-map args)
        screen-size (.getScreenSize (Toolkit/getDefaultToolkit))
        [window-width window-height] (if-let [option (get options "--window")]
                                       (mapv to-int (.split option "x"))
                                       [(round-down (int (.getWidth screen-size)) [640 800 1280])
                                        (round-down (int (.getHeight screen-size)) [480 600 720])])
        [font-width font-height] (mapv to-int (.split (get options "--font" "10x10") "x"))]
    (println "Using window size of" (str window-width "x" window-height) "and font size of" (str font-width "x" font-height))
    (println "Use window and font switches to override. Font can be 9x16, 8x8, 10x10, and 12x12. Theres no error handling or sanity checks so don't be dumb.")
    (println "Example: java -jar runner_puncher.jar --window 640x480 --font 12x12")
    (start-game start-screen
                {:window-width window-width
                 :window-height window-height
                 :tile-width font-width
                 :tile-height font-height})))
