(ns runner_puncher.renderer
  (:import [javax.imageio ImageIO]
           [java.awt.image BufferedImage LookupOp ShortLookupTable]
           [java.awt Graphics]))

(set! *unchecked-math* true)

(def cp437-9x16 { :file "cp437_9x16.png" :char-width 9 :char-height 16 })
(def cp437-10x10 { :file "cp437_10x10.png" :char-width 10 :char-height 10 })
(def cp437-12x12 { :file "cp437_12x12.png" :char-width 12 :char-height 12 })

(defn load-glyphs
  "Load raster glyphs with a specific width and height from a file.
  You probably won't need to use this."
  [name w h]
  (let [image (ImageIO/read (clojure.java.io/resource name))
        per-row (int (/ (.getWidth image) w))
        images (for [index (range 256)]
                 (let [x (* (int (mod index per-row)) w)
                       y (* (int (/ index per-row)) h)
                       i (BufferedImage. w h BufferedImage/TYPE_INT_ARGB)
                       g (.getGraphics i)]
                   (.drawImage g image 0 0 w h x y (+ x w) (+ y h) nil)
                   i))]
    (vec images)))

(defn set-colors
  "Given a gliyph, create a new one where the black has been replaced by
  the bg color and everything else has been replaced by the fg color.
  You probably won't need to use this."
  [g { fgr :r fgg :g fgb :b :as foreground } { bgr :r bgg :g bgb :b :as background }]
  (let [table (make-array Short/TYPE 4 256)]
    (doseq [i (range 256)]
      (aset-short table 0 i (short (if (= 0 i) bgr fgr)))
      (aset-short table 1 i (short (if (= 0 i) bgg fgg)))
      (aset-short table 2 i (short (if (= 0 i) bgb fgb)))
      (aset-short table 3 i (short 255)))
    (.filter (LookupOp. (ShortLookupTable. 0 table) nil) g nil)))

(defn new-renderer
  "Return a new render function that takes a Graphics and tiles and renders the tiles to the Graphics object."
  [width height { :keys [file char-width char-height] }]
  (let [glyphs (load-glyphs file char-width char-height)
        glyph-cache-atom (atom {})
        get-glyph (fn [c fg bg]
                    (let [k (str c "-" fg "-" bg)]
                      (when-not (get @glyph-cache-atom k)
                        (swap! glyph-cache-atom assoc k (set-colors (get glyphs (int c)) fg bg)))
                      (get @glyph-cache-atom k)))
        render-cache-atom (atom {})
        #^BufferedImage offscreen-buffer (BufferedImage. width height BufferedImage/TYPE_INT_ARGB)
        #^Graphics offscreen-graphics (.getGraphics offscreen-buffer)
        empty-tile {:c 0 :fg {:r 0 :g 0 :b 0} :bg {:r 0 :g 0 :b 0}}
        empty-tiles (into {} (for [x (range (int (/ width char-width)))
                                   y (range (int (/ height char-height)))]
                               [[x y] empty-tile]))]
    (fn [#^Graphics graphics tiles]
      (doseq [[[x y] {:keys [c fg bg] :as tile}] (merge empty-tiles tiles)]
        (when (not= tile (get @render-cache-atom [x y]))
          (swap! render-cache-atom assoc [x y] tile)
          (.drawImage offscreen-graphics (get-glyph (int c) fg bg) (* x char-width) (* y char-height) nil)))
      (.drawImage graphics offscreen-buffer 0 0 width height nil))))

(defn blank-terminal
  "A convenience function for working with terminal inputs."
  []
  {})

(defn add-char
  "A convenience function for working with terminal inputs.
  Adds a character (char or int) to a terminal at a specific point.
  If fg or bg is nil, the existing fg or bg is used."
  [m c x y fg bg]
  (assoc m [x y] {:c c :fg (or fg (get-in m [[x y] :fg]) {:r 96 :g 96 :b 96}) :bg (or bg (get-in m [[x y] :bg]) {:r 0 :g 0 :b 0})}))

(defn add-string
  "A convenience function for working with terminal inputs.
  Adds a string to a terminal starting at a specific point.
  If fg or bg is nil, the existing fg or bg is used."
  [m s x y fg bg]
  (if (empty? s)
    m
    (add-string (add-char m (first s) x y fg bg) (rest s) (inc x) y fg bg)))

