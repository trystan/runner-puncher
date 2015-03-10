(ns runner_puncher.core
  (:import [java.awt Canvas Graphics]
           [java.awt.event])
  (:require [runner_puncher.framework :refer :all]
            [runner_puncher.renderer :refer :all]
            [runner_puncher.worldgen :refer :all]
            [runner_puncher.actions :refer :all]))

(declare start-screen play-screen win-screen lose-screen)

(defn new-game []
  (let [g {:tick 0
           :messages []
           :player {:id :player
                    :type :player :going-up false :path []
                    :health 3 :max-health 3
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
  (let [c ((:type creature) creatures)]
    (add-string t (:char c) (:x creature) (:y creature) (:fg c) nil)))

(defn render-creatures [t creatures]
  (reduce render-creature t creatures))

(defn render-player-path [t points]
  (let [add-one (fn [t [x y]] (add-string t nil x y nil (hsl 60 40 40)))]
    (reduce add-one t points)))

(defn render-target-line [t [x0 y0] [x1 y1]]
  (let [points (bresenham x0 y0 x1 y1)
        grid (:grid @game-atom)
        ok (all? (fn [xy] (:walkable ((get grid xy out-of-bounds) tiles))) points)
        add-one (fn [t [x y]] (add-string t nil x y nil (if ok (hsl 60 40 40) (hsl 0 40 40))))]
    (reduce add-one t points)))

(defn render-hud [t player]
  (-> t
      (add-string (apply str (repeat width-in-characters " ")) 0 0 fg (hsl 45 25 25))
      (add-string (str "z: Laser cannon") 1 0 fg nil)
      (add-string (str "x: Rusty sword") 21 0 fg nil)
      (add-string (str "?: help") 41 0 fg nil)
      (add-string (str "@: ") (- width-in-characters 7) 0 fg nil)
      (add-string (apply str (repeat (:health player) (char 3))) (- width-in-characters 4) 0 red nil)
      (add-string (apply str (repeat (- (:max-health player) (:health player)) (char 3))) (+ (:health player) (- width-in-characters 4)) 0 fg nil)))

(defn render-messages [t at-top messages]
  (let [most-recent (:at (last (sort-by :at messages)))]
    (loop [t t
           messages (filter #(= most-recent (:at %)) messages)
           y (if at-top
               min-y
               (- height-in-characters (count messages)))]
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
        render-target-fn (if (empty? (:path player))
                           #(render-target-line % [(:x player) (:y player)] [mx my])
                           #(render-player-path % (:path player)))]
    (-> {}
        (render-grid (:grid game))
        (render-creatures (enemies game))
        (render-creature player)
        (render-target-fn)
        (render-hud player)
        (render-messages (> (:y player) (* 0.75 height-in-characters)) (:messages game)))))



(defn move-player-to-target []
  (let [points (rest (bresenham
                      (get-in @game-atom [:player :x])
                      (get-in @game-atom [:player :y])
                      (first @player-target-atom)
                      (second @player-target-atom)))
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
    (reset! player-target-atom [(int (/ (.getX e) tile-width)) (int (/ (.getY e) tile-height))])))

(defn update-play-screen [e]
  (let [player (get @game-atom :player)
        creatures (conj (enemies @game-atom) player)]
    (cond
     (not (all? empty? (map :path creatures)))
     (doseq [c creatures
             :when (and (seq? (:path c)) (> (count (:path c)) 0))]
       (swap! game-atom move-by-path (:id c)))
     ; TODO: enemy move and attack
     (and (not (:going-up player)) (= :stairs-down (get-in @game-atom [:grid [(:x player) (:y player)]])))
     (swap! game-atom move-creature-downstairs :player)
     (and (:going-up player) (= :stairs-up (get-in @game-atom [:grid [(:x player) (:y player)]])))
     (swap! game-atom move-creature-upstairs :player win-screen)
     :else
     (swap! game-atom remove-dead-creatures))))




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
