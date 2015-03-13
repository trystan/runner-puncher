(ns runner_puncher.creatures
  (:require [runner_puncher.framework :refer :all]))

(defn describe-creature [creature]
  (let [d (:description creature)
        d (str d (if (:immune-to-knockback creature) " Immune to knockback." ""))]
    d))

(defn describe-slot [creature slot]
  (if (get creature slot)
    (str (clojure.string/capitalize slot) ": " (get-in creature [slot :name]) " - " (get-in creature [slot :description]))
    (str (clojure.string/capitalize slot) ": none")))

(defn end-movement [creature]
  (-> creature
      (assoc :path [])
      (assoc :steps-remaining 0)
      (assoc :is-knocked-back false)))

(defn can-embiggen-creature [creature]
  (not= (clojure.string/upper-case (:char creature)) (:char creature)))

(defn embiggen-creature [creature]
  (if (can-embiggen-creature creature)
    (-> creature
        (update :prefix #(str "Giant " (clojure.string/lower-case %)))
        (update :char clojure.string/upper-case)
        (update :health inc)
        (update :max-health inc)
        (assoc :immune-to-knockback true))
    creature))

(defn poison-creature [creature]
  (if (> (:ignore-poison creature 0) 0)
    creature
    (-> creature
        (assoc :poison-amount (inc (:poison-amount creature 0)))
        (update :max-health dec)
        (update :health dec)
        (update :max-steps dec)
        (update :knockback-amount dec))))

(defn unpoison-creature-once [creature]
  (if (> (:poison-amount creature 0) 0)
    (-> creature
        (update :poison-amount dec)
        (update :max-health inc)
        (update :health inc)
        (update :max-steps inc)
        (update :knockback-amount inc))
    creature))

(defn consume-step [creature]
  (if (:is-knocked-back creature)
    (if (empty? (:path creature))
      (assoc creature :is-knocked-back false)
      creature)
    (let [creature (update creature :steps-remaining dec)]
      (if (< (:steps-remaining creature) 1)
        (end-movement creature)
        creature))))

(defn new-player [[x y]]
  {:prefix "Player" :type "" :char "@" :fg {:r 250 :g 250 :b 250}
   :description "Trying to find something on the 10th dungeon level."
   :id :player :knockback-amount 5 :poison-amount 0 :attack 1 :defence 0
   :is-creature true :going-up 0 :path [] :gold 0
   :health 3 :max-health 3 :steps-remaining 5 :max-steps 5
   :x 5 :y 9 :dungeon-level 1 :direction [0 0]})

(defn new-enemy [[x y]]
  (let [s 75
        l 75
        default {:is-creature true :steps-remaining 1 :max-steps 1
                 :health 1 :max-health 1 :attack 1 :defence 0
                 :knockback-amount 0 :poison-amount 0
                 :x x :y y :id (keyword "enemy-" (.toString (java.util.UUID/randomUUID)))}]
    (merge default (rand-nth [{:prefix "Web" :type "monster" :char "w" :fg (hsl 0 s l)
                               :description "Leaves webs behind when it dies."
                               :on-death [:replace-tiles {:floor :web-floor}]
                               :on-attack [:replace-tiles {:floor :web-floor}]}
                              {:prefix "Knockback" :type "monster" :char "k" :fg (hsl 30 s l)
                               :description "Knocks others back when it dies or attacks."
                               :on-death [:knockback 3] :knockback-amount 3}
                              {:prefix "Poison" :type "monster" :char "p" :fg (hsl 60 s l)
                               :description "Poisons others when it dies or attacks."
                               :on-death [:poison]
                               :on-attack [:poison]
                               :attack 0}
                              {:prefix "Deadly" :type "monster" :char "d" :fg (hsl 90 s l)
                               :description "Does extra damage to others when it dies or attacks."
                               :on-death [:damage 1]
                               :attack 2}
                              {:prefix "Embiggening" :type "monster" :char "e" :fg (hsl 120 s l)
                               :description "Embiggens others when it dies."
                               :on-death [:embiggen]}
                              {:prefix "Acid" :type "monster" :char "a" :fg (hsl 150 s l)
                               :description "Leaves acid pools when it dies."
                               :on-death [:replace-tiles {:floor :acid-floor}]}
                              {:prefix "Null" :type "monster" :char "n" :fg (hsl 180 s l)
                               :description "Nulifies nearby tiles when it dies."
                               :on-death [:replace-tiles {:wall :floor :door :floor
                                                          :web-floor :floor :acid-floor :floor}]}
                              {:prefix "Summoning" :type "monster" :char "s" :fg (hsl 210 s l)
                               :description "Summons others when it dies."
                               :on-death [:summon-others]}]))))


(defn make-creatures [grid depth candidate-positions]
  (let [positions (take (+ 8 (* 4 depth)) (shuffle candidate-positions))]
    (into {} (for [c (map new-enemy positions)] [(:id c) c]))))
