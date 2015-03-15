(ns runner_puncher.creatures
  (:require [runner_puncher.util :refer :all]))

(defn describe-creature [creature]
  (let [d (:description creature)
        d (str d (if (:immune-to-knockback creature) " Immune to knockback." ""))]
    d))

(defn creature-name [creature]
  (.trim (str (:prefix creature) " " (:type creature))))

(defn describe-slot [creature slot]
  (if (get creature slot)
    (str (clojure.string/capitalize slot) ": " (get-in creature [slot :name]) ". " (get-in creature [slot :description]))
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
  (if (or (< (:health creature) 1) (> (:ignore-poison creature 0) 0))
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

(defn unpoison-creature [creature]
  (if (> (:poison-amount creature 0) 0)
    (unpoison-creature (unpoison-creature-once creature))
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
   :description (str "Trying to find something on the " final-floor-depth "th dungeon level.")
   :id :player :knockback-amount 5 :poison-amount 0 :attack 1 :defence 0
   :is-creature true :going-up 0 :path [] :gold 0
   :health 3 :max-health 3 :steps-remaining 5 :max-steps 5
   :x 5 :y 9 :dungeon-level 1 :difficulty 1 :direction [0 0]})

(def enemy-prefixes [{:prefix "Web" :char "w"
                      :description "Leaves webs behind when it dies." :ignore-webs 1
                      :on-death [:replace-tiles {:floor :web-floor} 3]}
                     {:prefix "Knockback" :char "k"
                      :description "Knocks others back when it dies or attacks."
                      :on-death [:knockback 5] :knockback-amount 5}
                     {:prefix "Poison" :char "p"
                      :description "Poisons others when it dies or attacks."
                      :on-death [:poison]
                      :on-attack [:poison]
                      :attack 0}
                     {:prefix "Deadly" :char "d"
                      :description "Does extra damage to others when it dies or attacks."
                      :on-death [:damage 1]
                      :attack 2}
                     {:prefix "Embiggening" :char "e"
                      :description "Embiggens others when it dies."
                      :on-death [:embiggen]}
                     {:prefix "Acid" :char "a"
                      :description "Leaves acid pools when it dies." :ignore-acid-floor 1
                      :on-death [:replace-tiles {:floor :acid-floor} 1]}
                     {:prefix "Nullifier" :char "n"
                      :description "Nulifies nearby tiles when it dies."
                      :on-death [:replace-tiles {:wall :floor :door :floor
                                                 :web-floor :floor :acid-floor :floor
                                                 :spikes-n :floor :spikes-s :floor
                                                 :spikes-w :floor :spikes-e :floor} 1]}
                     {:prefix "Summoning" :char "s"
                      :description "Summons others when it dies."
                      :on-death [:summon-others]}])

(def enemy-types
  (let [s 75
        l 75]
    [{:type "noob"    :fg (hsl   0 s l) :description ""
      :on-death []}
     {:type "knight"  :fg (hsl  45 s l) :description "Strong armor and weapons."
      :on-death []
      :attack 2 :defence 2 :max-health 2 :health 2 :resist-knockback 1
      "weapon" {:name "Sword" :description "+1 attack."}
      "armor" {:name "Heavy armor" :description "+2 defence. Resist knockback 50%."}}
     {:type "archer" :fg (hsl  90 s l) :description "Ranged attacker."
      :on-death [] :ranged-attack 1
      "weapon" {:name "Bow" :description "Shoots arrows. Not accurate."}}
     {:type "wizard"  :fg (hsl 135 s l) :description "Teleports."
      :on-death []
      :teleportitis 1}
     {:type "zombie"  :fg (hsl 180 s l) :description "Keeps coming back."
      :on-death [:respawn]}
     {:type "thief"   :fg (hsl 225 s l) :description "Steals money."
      :on-death [] :on-attack [:steal-money]
      "weapon" {:name "Dagger" :description ""}}
     {:type "guard"   :fg (hsl 270 s l) :description "Hard to take down."
      :on-death []
      :defence 3 :health 3 :max-health 3 :resist-knockback 1
      "armor" {:name "Heavy armor" :description "+2 defence. Resist knockback 50%."}}
     {:type "baker"   :fg (hsl 315 s l) :description "Does not belong in dungeons."
      :on-death []
      "weapon" {:name "Baguette" :description "+0 attack."}
      "headwear" {:name "Chef's hat" :description "+0 defence."}}]))

(defn new-enemy-catalog []
  (let [pre (shuffle enemy-prefixes)
        main (shuffle enemy-types)
        merge-fn (fn [a b]
                   (cond
                    (integer? a)
                    (+ a  b)
                    (string? a)
                    (.trim (str a " " b))
                    (vector? a)
                    [a b]
                    :else
                    (println "berge-fn" a b)))]
    (map (partial merge-with merge-fn) pre main)))

(defn new-enemy [difficulty enemy-catalog [x y]]
  (let [valid-enemies (take (+ 1 difficulty) enemy-catalog)
        default {:is-creature true :steps-remaining 1 :max-steps 1
                 :health 1 :max-health 1 :attack 1 :defence 0
                 :knockback-amount 0 :poison-amount 0
                 :x x :y y :id (keyword "enemy-" (.toString (java.util.UUID/randomUUID)))}]
    (merge default (rand-nth valid-enemies))))


(defn make-creatures [grid difficulty enemy-catalog candidate-positions]
  (let [positions (take (+ 10 difficulty) (shuffle candidate-positions))]
    (into {} (for [c (map #(new-enemy difficulty enemy-catalog %) positions)]
               (if (< (rand-int 100) 1000)
                 [(:id c) (embiggen-creature c)]
                 [(:id c) c])))))
