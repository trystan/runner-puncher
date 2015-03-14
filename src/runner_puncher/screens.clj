(ns runner_puncher.screens
  (:import [java.awt Canvas Graphics Toolkit]
           [java.awt.event])
  (:require [runner_puncher.framework :refer :all]
            [runner_puncher.renderer :refer :all]
            [runner_puncher.worldgen :refer :all]
            [runner_puncher.creatures :refer :all]
            [runner_puncher.items :refer :all]
            [runner_puncher.actions :refer :all]
            [runner_puncher.util :refer :all])
  (:gen-class))


(declare start-screen play-screen store-screen win-screen lose-screen)

(defn trace [x]
  nil)

;; -------- play screen -------- ;;

(defn render-grid-tile [t [[x y] tile]]
  (if (= 0 (mod (+ x y) 2))
    (add-string t (:char (tile tiles)) x y (:fg (tile tiles)) (:bg (tile tiles)))
    (add-string t (:char (tile tiles)) x y
                (merge-with + {:r 5 :g 5 :b 5} (:fg (tile tiles)))
                (merge-with + {:r 5 :g 5 :b 5} (:bg (tile tiles))))))

(defn render-grid [t grid]
  (reduce render-grid-tile t grid))

(defn render-item [t item]
  (add-string t (:char item) (:x item) (:y item) (:fg item) nil))

(defn render-items [t items]
  (reduce render-item t items))

(defn render-creature [t creature]
  (add-string t (:char creature) (:x creature) (:y creature) (:fg creature) nil))

(defn render-creatures [t creatures]
  (reduce render-creature t creatures))

(defn render-effect [t e]
  (add-string t (:char e) (:x e) (:y e) (:fg e) (:bg e)))

(defn render-effects [t effects]
  (reduce render-effect t effects))

(defn render-player-path [t points]
  (let [add-ok (fn [t [x y]] (add-string t nil x y nil (hsl 60 40 40)))]
    (reduce add-ok t points)))

(defn render-target-line [t points]
  (let [grid (:grid @game-atom)
        walk-points (take (get-in @game-atom [:player :steps-remaining]) (drop 1 points))
        too-far-points (drop (+ (count walk-points) 1) points)
        ok (all? (fn [xy] (:walkable ((get grid xy out-of-bounds) tiles))) walk-points)
        add-walk-point (fn [t [x y]] (add-string t nil x y nil (if ok (hsl 60 33 33) (hsl 0 33 33))))
        add-too-far-point (fn [t [x y]] (add-string t nil x y nil (hsl 60 25 25)))
        t (reduce add-walk-point t walk-points)
        t (reduce add-too-far-point t too-far-points)]
    t))

(defn render-counters [t current maximum poison c color x y]
  (-> t
      (add-string (apply str (repeat current c)) x y color nil)
      (add-string (apply str (repeat (- maximum current) c)) (+ x current) y fg nil)
      (add-string (apply str (repeat poison c)) (+ x maximum) y green nil)))

(defn render-hud [t player]
  (let [width-in-characters (global :width-in-characters)
        heart-start (- width-in-characters (:max-health player) (:poison-amount player) 5 1)
        puncher-start (- heart-start (:knockback-amount player) (:poison-amount player) 6 3)
        runner-start (- puncher-start (:max-steps player) (:poison-amount player) 4 3)]
    (-> t
        (add-string (apply str (repeat width-in-characters " ")) 0 0 fg (hsl 45 25 25))
        (add-string (str "Level " (:dungeon-level player) " going " (if (= 1 (:going-up player)) "up" "down")) 1 0 fg nil)

        (add-string (str "$" (:gold player)) (- runner-start 4) 0 (hsl 60 50 50) nil)

        (add-string "run" runner-start 0 fg nil)
        (render-counters (:steps-remaining player) (:max-steps player) (:poison-amount player)
                         (char 4) blue (+ runner-start 4) 0)

        (add-string "punch" puncher-start 0 fg nil)
        (render-counters (:knockback-amount player) (:knockback-amount player) (:poison-amount player)
                         (char 7) blue (+ puncher-start 6) 0)

        (add-string "live" heart-start 0 fg nil)
        (render-counters (:health player) (:max-health player) (:poison-amount player)
                         (char 3) red (+ heart-start 5) 0))))

(defn render-target [t game mx my]
  (let [c (creature-at game [mx my])
        creature-name (.trim (if c (str (:prefix c) " " (:type c)) ""))
        item (item-at game [mx my])
        tile (get tiles (get-in game [:grid [mx my]]))
        x 1
        y (- (global :height-in-characters) 9)
        y (if (> my y) 4 y)]
    (cond
     c
     (-> t
         (add-string creature-name x (+ y 0) fg bg)
         (render-counters (:health c) (:max-health c) (:poison-amount c)
                          (char 3) red (+ (count creature-name) x 1) (+ y 0))
         (add-string (str " Attack " (:attack c)) (+ (count creature-name) (:max-health c) (:poison-amount c) 2) (+ y 0) fg bg)
         (add-string (str " Defence " (:defence c)) (+ (count (str " Attack " (:attack c))) (count creature-name) (:max-health c) (:poison-amount c) 2) (+ y 0) fg bg)
         (add-string (describe-creature c) x (+ y 1) fg bg)
         (add-string (describe-slot c "headwear") x (+ y 2) fg bg)
         (add-string (describe-slot c "armor") x (+ y 3) fg bg)
         (add-string (describe-slot c "footwear") x (+ y 4) fg bg)
         (add-string (describe-slot c "weapon") x (+ y 5) fg bg)
         (add-string (str "Standing on " (:name tile)) x (+ y 6) fg bg))
     item
     (-> t
         (add-string (:name item) x (+ y 0) fg bg)
         (add-string (:description item) x (+ y 1) fg bg)
         (add-string (str "On " (:name tile)) x (+ y 2) fg bg))
     :else
     (-> t
         (add-string (:name tile) 2 (+ y 0) fg bg)))))

(defn render-messages [t at-top messages]
  (let [most-recent (:at (last (sort-by :at messages)))]
    (loop [t t
           messages (take 5 (filter #(= most-recent (:at %)) messages))
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
        [mx my] (:target game)
        player (:player game)
        line (bresenham (:x player) (:y player) mx my)
        render-target-fn (if (empty? (:path player))
                           #(render-target-line % line)
                           #(render-player-path % (:path player)))]
    (-> {}
        (render-grid (:grid game))
        (render-items (items game))
        (render-creatures (enemies game))
        (render-creature player)
        (render-effects (filter :is-effect (map second game)))
        (render-target-fn)
        (render-target game mx my)
        (render-messages (> (:y player) (* 0.75 (global :height-in-characters))) (:messages game))
        (render-hud player))))

(defn key-press-play-screen [e]
  (when (empty? (get-in @game-atom [:player :path]))
    (case (to-keyword e)
      :up    (swap! game-atom move-player-target  0 -1)
      :down  (swap! game-atom move-player-target  0  1)
      :left  (swap! game-atom move-player-target -1  0)
      :right (swap! game-atom move-player-target  1  0)
      :k (swap! game-atom move-player-target  0 -1)
      :j (swap! game-atom move-player-target  0  1)
      :h (swap! game-atom move-player-target -1  0)
      :l (swap! game-atom move-player-target  1  0)
      :y (swap! game-atom move-player-target -1 -1)
      :u (swap! game-atom move-player-target  1 -1)
      :b (swap! game-atom move-player-target -1  1)
      :n (swap! game-atom move-player-target  1  1)
      :left-click (swap! game-atom move-player-to-target)
      :enter (swap! game-atom move-player-to-target)
      (trace e))))

(defn mouse-move-play-screen [e]
  (when (empty? (get-in @game-atom [:player :path]))
    (swap! game-atom assoc :target [(int (/ (.getX e) (global :tile-width)))
                                    (int (/ (.getY e) (global :tile-height)))])))

(defn update-play-screen [e]
  (let [player (get @game-atom :player)
        creatures (conj (enemies @game-atom) player)
        effects (filter :is-effect (map second @game-atom))]
    (cond
     (not (empty? effects))
     (swap! game-atom update-effects)
     (not (all? empty? (map :path creatures)))
     (doseq [c creatures
             :when (and (seq? (:path c)) (> (count (:path c)) 0))]
       (swap! game-atom move-by-path (:id c)))
     (< (:steps-remaining player) 1)
     (do
       (swap! game-atom move-enemies)
       (swap! game-atom assoc-in [:player :steps-remaining] (:max-steps player))
       (swap! game-atom update :tick inc))
     :else
     (if (< (get-in @game-atom [:player :health]) 1)
       (swap-screen lose-screen)
       (swap! game-atom remove-dead-enemies)))))

(def play-screen {:on-render render-play-screen
                  :on-key-press key-press-play-screen
                  :on-mouse-move mouse-move-play-screen
                  :on-timer update-play-screen})


;; -------- start screen -------- ;;

(defn render-start-screen []
  (-> {}
      (add-center-string "RUNNER_PUNCHER" 2)
      (add-center-string "A 2015 7DRL by Trystan Spangler" 4)
      (add-center-string (str "You must find the amulet on the " final-floor-depth "th floor and return to the surface.") 6)
      (add-center-string "-- Click or press Enter to start --" (- (global :height-in-characters) 2))))

(defn on-key-press-start-screen [e]
  (case (to-keyword e)
    :left-click (do
                  (swap! game-atom deref)
                  (swap-screen play-screen))
    :enter (do
             (swap! game-atom deref)
             (swap-screen play-screen))
    (trace e)))

(def start-screen {:on-render render-start-screen
                   :on-key-press on-key-press-start-screen})


;; -------- store screen -------- ;;

(defn render-store-item [t [index item]]
  (if item
    (-> t
        (add-string (str (inc index) ". ($" (:price item) ") " (:name item)) 5 (+ (* 2 index) 7) white nil)
        (add-string (str "    " (:description item)) 5 (+ (* 2 index) 8) fg nil))
    (add-string t (str (inc index) ". -- out of stock --") 5 (+ (* 2 index) 7) fg nil)))

(defn render-store-items [t items]
  (let [x 5
        y 25
        player (:player @game-atom)]
    (-> (reduce render-store-item t (for [i (range 0 (count items))] [i (nth items i)]))
        (add-string (str "You have $" (get-in @game-atom [:player :gold]) ". You are currently using:") x (+ y 1) white bg)
        (add-string (describe-slot player "headwear") x (+ y 2) fg bg)
        (add-string (describe-slot player "armor") x (+ y 3) fg bg)
        (add-string (describe-slot player "footwear") x (+ y 4) fg bg)
        (add-string (describe-slot player "weapon") x (+ y 5) fg bg))))

(defn render-store-screen []
  (-> {}
      (add-center-string "Welcome to the store between levels!" 2)
      (render-store-items (:store-items @game-atom))
      (add-center-string "Press 1 through 9 to buy something." 4)
      (add-center-string "-- press Enter to go to the next level --" (- (global :height-in-characters) 2))))

(defn on-key-press-store-screen [e]
  (case (to-keyword e)
    :1 (swap! game-atom maybe-buy-item 0)
    :2 (swap! game-atom maybe-buy-item 1)
    :3 (swap! game-atom maybe-buy-item 2)
    :4 (swap! game-atom maybe-buy-item 3)
    :5 (swap! game-atom maybe-buy-item 4)
    :6 (swap! game-atom maybe-buy-item 5)
    :7 (swap! game-atom maybe-buy-item 6)
    :8 (swap! game-atom maybe-buy-item 7)
    :9 (swap! game-atom maybe-buy-item 8)
    :enter (do
             (pop-screen)
             (reset! game-atom (deref (:next-level-future @game-atom))))
    (trace e)))

(def store-screen {:on-render render-store-screen
                   :on-key-press on-key-press-store-screen})


;; -------- win screen -------- ;;

(defn render-win-screen []
  (-> {}
      (add-center-string "You won" 2)
      (add-center-string "-- press Enter to start again --" (- (global :height-in-characters) 2))))

(defn on-key-press-win-screen [e]
  (case (to-keyword e)
    :enter (do
             (reset! game-atom (new-game win-screen store-screen))
             (swap-screen play-screen))
    (trace e)))

(def win-screen {:on-render render-win-screen
                 :on-key-press on-key-press-win-screen})


;; -------- lose screen -------- ;;

(defn render-lose-screen []
  (-> (render-play-screen)
      (add-center-string "You are dead" (int (- (/ (global :height-in-characters) 2) 1)))
      (add-center-string "-- press Enter to start again --" (int (+ (/ (global :height-in-characters) 2) 1)))))

(defn on-key-press-lose-screen [e]
  (case (to-keyword e)
    :enter (do
             (reset! game-atom (new-game win-screen store-screen))
             (swap-screen play-screen))
    (trace e)))

(def lose-screen {:on-render render-lose-screen
                  :on-key-press on-key-press-lose-screen})
