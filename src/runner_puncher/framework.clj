(ns runner_puncher.framework
  (:import [java.awt Canvas Graphics]
           [java.awt.event])
  (:require [runner_puncher.ssw :refer :all]
            [runner_puncher.renderer :refer :all]))

(set! *warn-on-reflection* true)
(set! *unchecked-math* true)

(def globals-atom (atom {}))

(defn set-render-options [options]
  (let [render-options { :file (str "cp437_" (:tile-width options) "x" (:tile-height options) ".png")
                         :char-width (:tile-width options)
                         :char-height (:tile-height options)}
        derived-options {:render-terminal (new-renderer (:window-width options) (:window-height options) render-options)
                         :width-in-characters (int (/ (:window-width options) (:tile-width options)))
                         :height-in-characters (int (/ (:window-height options) (:tile-height options)))}]
    (reset! globals-atom (merge derived-options
                                options))))

(defn global [k]
  (k @globals-atom))

(def red {:r 250 :g 0 :b 0})
(def green {:r 0 :g 250 :b 0})
(def blue {:r 0 :g 0 :b 250})
(def fg {:r 191 :g 191 :b 191})
(def bg {:r 0 :g 0 :b 0})

(defn add-center-string [t s y]
  (let [x (int (/ (- (global :width-in-characters) (count s)) 2))]
    (add-string t s x y fg nil)))


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
    java.awt.event.MouseEvent
    (let [#^java.awt.event.MouseEvent e e]
      (nth [:unknown :left-click :middle-click :right-click] (.getButton e)))
    :unknown))


(defn on-render [^Graphics graphics]
  (let [terminal ((:on-render (first @screen-stack-atom) identity))]
    ((global :render-terminal) graphics terminal)))

(defn on-key-press [e]
  ((:on-key-press (first @screen-stack-atom) identity) e))

(defn on-mouse-move [e]
  ((:on-mouse-move (first @screen-stack-atom) identity) e))

(defn on-timer [e]
  ((:on-timer (first @screen-stack-atom) identity) e))


(defn start-game [screen render-options]
  (set-render-options render-options)
  (reset! screen-stack-atom (list screen))
  (new-super-simple-window {:title "RUNNER_PUNCHER"
                            :width (global :window-width) :height (global :window-height)
                            :on-render on-render
                            :on-key-press on-key-press
                            :on-mouse-move on-mouse-move
                            :on-mouse-press on-key-press
                            :fps 30
                            :on-timer on-timer}))




(defn bresenham [x0 y0 x1 y1]
  (let [first-x x0
        first-y y0
        len-x (Math/abs (- x0 x1))
        len-y (Math/abs (- y0 y1))
        is-steep (> len-y len-x)
        [x0 y0 x1 y1] (if is-steep [y0 x0 y1 x1] [x0 y0 x1 y1])
        [x0 y0 x1 y1] (if (> x0 x1) [x1 y1 x0 y0] [x0 y0 x1 y1])
        delta-x (- x1 x0)
        delta-y (Math/abs (- y0 y1))
        y-step (if (< y0 y1) 1 -1)]
    (loop [x x0
           y y0
           error (Math/floor (/ delta-x 2))
           points (if is-steep [[y x]] [[x y]])]
      (if (> x x1)
        (let [points (rest points)]
          (if (= [first-x first-y] (first points))
            points
            (reverse points)))
        (if (< error delta-y)
          (recur (inc x)
                 (+ y y-step)
                 (+ error (- delta-x delta-y))
                 (if is-steep (conj points [y x]) (conj points [x y])))
          (recur (inc x)
                 y
                 (- error delta-y)
                 (if is-steep (conj points [y x]) (conj points [x y]))))))))




(defn hue-to-rgb [m1, m2, hue]
  (let* [h (cond
             (< hue 0) (inc hue)
             (> hue 1) (dec hue)
             :else hue)]
    (cond
      (< (* h 6) 1) (+ m1 (* (- m2 m1) h 6))
      (< (* h 2) 1) m2
      (< (* h 3) 2) (+ m1 (* (- m2 m1) (- (/ 2.0 3) h) 6))
      :else m1)))

(defn hsl-to-rgb
  [hue saturation lightness]
  (let* [h (/ hue 360.0)
         s (/ saturation 100.0)
         l (/ lightness 100.0)
         m2 (if (<= l 0.5) (* l (+ s 1))
                           (- (+ l s) (* l s)))
         m1 (- (* l 2) m2)]
    (into []
          (map #(Math/round (* 0xff %))
               [(hue-to-rgb m1 m2 (+ h (/ 1.0 3)))
                (hue-to-rgb m1 m2 h)
                (hue-to-rgb m1 m2 (- h (/ 1.0 3)))]))))

(defn hsl [h s l]
  (let [[r g b] (hsl-to-rgb h s l)]
    {:r r :g g :b b}))
