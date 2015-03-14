(ns runner_puncher.util)

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
