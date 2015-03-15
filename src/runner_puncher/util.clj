(ns runner_puncher.util)

(def final-floor-depth 5)

(defn abs [^java.lang.Number number]
  (Math/abs number))

(defn round [^java.lang.Number number]
  (Math/round number))

(defn round-down [x values]
  (last (filter #(<= % x) values)))

(defn to-int [string]
  (Integer/parseInt string))

(defn nearby [x1 y1 x2 y2 minimum maximum]
  (let [distance-squared (+ (Math/pow (- x1 x2) 2) (Math/pow (- y1 y2) 2))]
    (<= (* minimum minimum) distance-squared (* maximum maximum))))

(defn all? [p coll]
  (= (count coll) (count (filter p coll))))

(defn any? [p coll]
  (> (count (filter p coll)) 0))

(defn map-keys [f m]
  (into {} (map (fn [[k v]] [(f k) v]) m)))

(defn bresenham [x0 y0 x1 y1]
  (let [first-x x0
        first-y y0
        len-x (abs (- x0 x1))
        len-y (abs (- y0 y1))
        is-steep (> len-y len-x)
        [x0 y0 x1 y1] (if is-steep [y0 x0 y1 x1] [x0 y0 x1 y1])
        [x0 y0 x1 y1] (if (> x0 x1) [x1 y1 x0 y0] [x0 y0 x1 y1])
        delta-x (- x1 x0)
        delta-y (abs (- y0 y1))
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
  (let [h (cond
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
  (let [h (/ hue 360.0)
        s (/ saturation 100.0)
        l (/ lightness 100.0)
        m2 (if (<= l 0.5) (* l (+ s 1))
             (- (+ l s) (* l s)))
        m1 (- (* l 2) m2)]
    (into []
          (map #(round (* 0xff %))
               [(hue-to-rgb m1 m2 (+ h (/ 1.0 3)))
                (hue-to-rgb m1 m2 h)
                (hue-to-rgb m1 m2 (- h (/ 1.0 3)))]))))

(defn hsl [h s l]
  (let [[r g b] (hsl-to-rgb h s l)]
    {:r r :g g :b b}))

(def red {:r 250 :g 0 :b 0})
(def green {:r 0 :g 250 :b 0})
(def blue {:r 0 :g 0 :b 250})
(def fg {:r 191 :g 191 :b 191})
(def light {:r 225 :g 225 :b 225})
(def white {:r 255 :g 255 :b 255})
(def bg {:r 0 :g 0 :b 0})
