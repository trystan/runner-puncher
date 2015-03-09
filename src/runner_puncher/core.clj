(ns runner_puncher.core
  (:import [java.awt Canvas Graphics]
           [java.awt.event])
  (:require [runner_puncher.framework :refer :all]
            [runner_puncher.renderer :refer :all]
            [runner_puncher.worldgen :refer :all]
            [runner_puncher.actions :refer :all]))

(declare start-screen play-screen win-screen lose-screen)

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
