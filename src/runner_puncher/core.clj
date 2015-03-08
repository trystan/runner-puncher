(ns runner_puncher.core
  (:import [java.awt Canvas Graphics]
           [java.awt.event])
  (:require [runner_puncher.ssw :refer :all]
            [runner_puncher.renderer :refer :all]))

(set! *warn-on-reflection* true)
(set! *unchecked-math* true)

(def render-terminal (new-renderer 620 480 cp437-12x12))
(def width-in-characters (int (/ 620 12)))
(def height-in-characters (int (/ 480 12)))

(def fg {:r 191 :g 191 :b 191})
(def bg {:r 0 :g 0 :b 0})

(defn add-center-string [t s y]
  (let [x (int (/ (- width-in-characters (count s)) 2))]
    (add-string t s x y fg bg)))


(def screen-stack-atom (atom ()))

(defn push-screen [screen]
  (swap! screen-stack-atom conj screen))

(defn pop-screen []
  (swap! screen-stack-atom rest))

(defn swap-screen [screen]
  (pop-screen)
  (push-screen screen))


(defn to-keyword [e]
  (condp instance? e
    java.awt.event.ActionEvent
    :timer
    java.awt.event.KeyEvent
    (let [#^java.awt.event.KeyEvent e e
          string (java.awt.event.KeyEvent/getKeyText (.getKeyCode e))]
      (if (and (== 1 (count string)) (.isShiftDown e))
        (keyword (.toUpperCase string))
        (keyword (.toLowerCase string))))
    :unknown))


(defn on-render [^Graphics graphics]
  (let [terminal ((:on-render (first @screen-stack-atom) identity))]
    (render-terminal graphics terminal)))

(defn on-keypress [e]
  ((:on-keypress (first @screen-stack-atom) identity) e))

(defn on-timer [e]
  ((:on-timer (first @screen-stack-atom) identity) e))



(declare start-screen play-screen win-screen lose-screen)


(def start-screen {:on-timer identity
                   :on-render (fn [] (-> {}
                                  (add-center-string "RUNNER_PUNCHER" 2)
                                  (add-center-string "A 2015 7DRL by Trystan Spangler" 3)
                                  (add-center-string "-- press Enter to start --" (- height-in-characters 2))))
                   :on-keypress (fn [e]
                                  (case (to-keyword e)
                                    :enter (swap-screen play-screen)
                                    (println e)))})

(def play-screen {:on-timer identity
                  :on-render (fn [] (-> {}
                                 (add-center-string "So much fun" 2)
                                 (add-center-string "-- press W to win --" (- height-in-characters 4))
                                 (add-center-string "-- press L to lose --" (- height-in-characters 2))))
                  :on-keypress (fn [e]
                                 (case (to-keyword e)
                                   :W (swap-screen win-screen)
                                   :L (swap-screen lose-screen)
                                   (println e)))})

(def win-screen {:on-timer identity
                   :on-render (fn [] (-> {}
                                  (add-center-string "You won" 2)
                                  (add-center-string "-- press Enter to start again --" (- height-in-characters 2))))
                   :on-keypress (fn [e]
                                  (case (to-keyword e)
                                    :enter (swap-screen play-screen)
                                    (println e)))})

(def lose-screen {:on-timer identity
                   :on-render (fn [] (-> {}
                                  (add-center-string "You lost" 2)
                                  (add-center-string "-- press Enter to start again --" (- height-in-characters 2))))
                   :on-keypress (fn [e]
                                  (case (to-keyword e)
                                    :enter (swap-screen play-screen)
                                    (println e)))})




(defn -main []
  (push-screen start-screen)
  ; (swap-screen start-screen)
  (new-super-simple-window {:title "RUNNER_PUNCHER"
                            :height 480 :width 620
                            ;:height 720 :width 1280
                            :on-render on-render
                            :on-key-press on-keypress
                            :on-timer on-timer}))
