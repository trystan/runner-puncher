(ns runner_puncher.core
  (:import [java.awt Canvas Graphics Toolkit]
           [java.awt.event])
  (:require [runner_puncher.framework :refer :all]
            [runner_puncher.renderer :refer :all]
            [runner_puncher.worldgen :refer :all]
            [runner_puncher.actions :refer :all])
  (:gen-class))

(declare start-screen play-screen win-screen lose-screen)

(defn describe-creature [creature]
  (let [d (:description creature)
        d (str d (if (:immune-to-knockback creature) " Immune to knockback." ""))]
    d))

(defn new-game []
  (let [g {:tick 0
           :exit-screen win-screen
           :messages []
           :player {:prefix "Player" :type "" :char "@" :fg {:r 250 :g 250 :b 250}
                    :id :player :knockback-amount 5
                    :is-creature true :going-up false :path []
                    :health 3 :max-health 3 :steps-remaining 5 :max-steps 5
                    :x 5 :y 9 :dungeon-level 1 :direction [0 0]}}]
    (-> g
        (merge (generate-level 4 9 5 9 :stairs-up :stairs-down))
        (add-message "You are RUNNER_PUNCHER."))))

(def game-atom (atom new-game))

(def player-target-atom (atom [5 3]))

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

(defn render-health [t current maximum x y]
  (-> t
      (add-string (apply str (repeat current (char 3)))
                  x y red nil)
      (add-string (apply str (repeat (- maximum current) (char 3)))
                  (+ x current) y fg nil)))

(defn render-hud [t player]
  (let [width-in-characters (global :width-in-characters)
        heart-start (- width-in-characters (:max-health player) 5 1)
        puncher-start (- heart-start (:knockback-amount player) 6 3)
        runner-start (- puncher-start (:max-steps player) 4 3)]
    (-> t
        (add-string (apply str (repeat width-in-characters " ")) 0 0 fg (hsl 45 25 25))
        (add-string (str "Level " (:dungeon-level player)) 1 0 fg nil)

        (add-string "run" runner-start 0 fg nil)
        (add-string (apply str (repeat (:steps-remaining player) (char 4)))
                    (+ runner-start 4) 0 blue nil)
        (add-string (apply str (repeat (- (:max-steps player) (:steps-remaining player)) (char 4)))
                    (+ runner-start 4 (:steps-remaining player)) 0 fg nil)

        (add-string "punch" puncher-start 0 fg nil)
        (add-string (apply str (repeat (:knockback-amount player) (char 7)))
                    (+ puncher-start 6) 0 blue nil)

        (add-string "live" heart-start 0 fg nil)
        (render-health (:health player) (:max-health player) (+ heart-start 5) 0))))

(defn render-target [t game mx my]
  (let [c (creature-at game [mx my])
        tile (get tiles (get-in game [:grid [mx my]]))
        x 1
        y (if (< my 5) 6 2)]
    (if c
      (-> t
          (add-string (str (:prefix c) " " (:type c)) x (+ y 0) fg bg)
          (render-health (:health c) (:max-health c) (+ (count (str (:prefix c) " " (:type c))) x 1) (+ y 0))
          (add-string (describe-creature c) x (+ y 1) fg bg)
          (add-string (str "Standing on " (:name tile)) x (+ y 2) fg bg))
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
