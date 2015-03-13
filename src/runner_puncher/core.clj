(ns runner_puncher.core
  (:import [java.awt Toolkit])
  (:require [runner_puncher.framework :refer :all]
            [runner_puncher.screens :refer :all]
            [runner_puncher.util :refer :all])
  (:gen-class))

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
