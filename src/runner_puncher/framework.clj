(ns runner_puncher.framework
  (:import [java.awt Canvas Graphics]
           [java.awt.event])
  (:require [runner_puncher.ssw :refer :all]
            [runner_puncher.renderer :refer :all]
            [runner_puncher.util :refer :all]))

(set! *warn-on-reflection* true)
(set! *unchecked-math* true)

(def game-atom (atom {}))

(def globals-atom (atom {}))

(defn set-render-options [options]
  (let [render-options { :file (str "cp437_" (:tile-width options) "x" (:tile-height options) ".png")
                         :char-width (:tile-width options)
                         :char-height (:tile-height options)}
        derived-options {:renderer (new-renderer (:window-width options) (:window-height options) render-options)
                         :width-in-characters (int (/ (:window-width options) (:tile-width options)))
                         :height-in-characters (int (/ (:window-height options) (:tile-height options)))}]
    (reset! globals-atom (merge derived-options
                                options))))

(defn global [k]
  (k @globals-atom))

(defn add-center-string [t s y]
  (let [x (int (/ (- (global :width-in-characters) (count s)) 2))]
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
    java.awt.event.MouseEvent
    (let [#^java.awt.event.MouseEvent e e]
      (nth [:unknown :left-click :middle-click :right-click] (.getButton e)))
    :unknown))


(defn on-render [^Graphics graphics]
  (let [terminal ((:on-render (first @screen-stack-atom) identity))]
    ((global :renderer) graphics terminal)))

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

