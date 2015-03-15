(ns runner_puncher.items
  (:require [runner_puncher.util :refer :all]))

(defn random-item [is-store-item]
  (let [data (rand-nth [{:base {:char "[" :slot "footwear" :name "shoes"}
                         :prefixes [{:name "heavy" :description "-1 movement" :effect {:max-steps -1}}
                                    {:name "uncomfortable" :description "-1 knockback" :effect {:knockback-amount -1}}
                                    {:name "smelly" :description "increase shop prices by $5" :effect {:affect-prices 5}}
                                    {:name "fine" :description "" :effect {}}]
                         :postfixes [{:name "of webwalking" :description "ignore webs"
                                      :effect {:ignore-webs 1}}
                                     {:name "of acidwalking" :description "ignore acid pools"
                                      :effect {:ignore-acid-floor 1}}
                                     {:name "of running" :description "+2 movement"
                                      :effect {:max-steps 2}}
                                     {:name "of standing" :description "resist knockback 50%"
                                      :effect {:resist-knockback 1}}]}
                        {:base {:char "]" :slot "armor" :name "cape"}
                         :prefixes [{:name "heavy" :description "-1 movement" :effect {:max-steps -1}}
                                    {:name "uncomfortable" :description "-1 knockback" :effect {:knockback-amount -1}}
                                    {:name "fine" :description "" :effect {}}
                                    {:name "cumbersome" :description "-1 movement, -1 knockback" :effect {:max-steps -1 :knockback-amount -1}}]
                         :postfixes [{:name "of life" :description "+1 life"
                                      :effect {:max-health 1 :health 1}}
                                     {:name "of anti-poison" :description "ignore poison attacks"
                                      :effect {:ignore-poison 1}}
                                     {:name "of defense" :description "+1 defense"
                                      :effect {:defense 1}}
                                     {:name "of standing" :description "resist knockback 50%"
                                      :effect {:resist-knockback 1}}]}
                        {:base {:char "(" :slot "headwear" :name "helm"}
                         :prefixes [{:name "heavy" :description "-1 movement" :effect {:max-steps -1}}
                                    {:name "uncomfortable" :description "-1 knockback" :effect {:knockback-amount -1}}
                                    {:name "fine" :description "" :effect {}}
                                    {:name "cumbersome" :description "-1 movement, -1 knockback" :effect {:max-steps -1 :knockback-amount -1}}]
                         :postfixes [{:name "of life" :description "+1 life"
                                      :effect {:max-health 1 :health 1}}
                                     {:name "of standing" :description "resist knockback 50%"
                                      :effect {:resist-knockback 1}}
                                     {:name "of charming" :description "decrease shop prices by $5"
                                      :effect {:affect-prices -5}}
                                     {:name "of defense" :description "+1 defense"
                                      :effect {:defense 1}}]}
                        {:base {:char ")" :slot "weapon" :name "knuckles"}
                         :prefixes [{:name "fine" :description "" :effect {}}
                                    {:name "crude" :description "-1 attack" :effect {:attack -1}}
                                    {:name "bulky" :description "-1 defense" :effect {:defense -1}}
                                    {:name "soft" :description "-1 knockback" :effect {:knockback-amount -1}}]
                         :postfixes [{:name "of punching" :description "+2 knockback"
                                      :effect {:knockback-amount 2}}
                                     {:name "of defense" :description "+1 defense"
                                      :effect {:defense 1}}
                                     {:name "of attack" :description "+1 attack"
                                      :effect {:attack 1}}
                                     {:name "of charming" :description "decrease shop prices by $5"
                                      :effect {:affect-prices -5}}]}])
        noun (:base data)
        prefix (rand-nth (:prefixes data))
        postfix (rand-nth (:postfixes data))
        [prefix postfix] (cond
                          is-store-item
                          [prefix postfix]
                          (< (rand) 0.66)
                          [prefix nil]
                          :else
                          [nil postfix])
        item-name [(:name noun)]
        item-name (if prefix (concat [(:name prefix)] item-name) item-name)
        item-name (if postfix (concat item-name [(:name postfix)]) item-name)
        item-name (clojure.string/capitalize (clojure.string/join " " (remove empty? item-name)))
        description []
        description (if prefix (concat description [(:description prefix)]) description)
        description (if postfix (concat description [(:description postfix)]) description)
        description (clojure.string/capitalize (str (clojure.string/join ", " (remove empty? description)) "."))
        description (if (= 1 (count description)) "" description)
        effect {}
        effect (if prefix (merge-with + effect (:effect prefix)) effect)
        effect (if postfix (merge-with + effect (:effect postfix)) effect)]
  {:price 0 :name item-name :slot (:slot noun) :description description
   :effect effect :char (:char noun) :fg light}))

(defn make-health-potion [[x y]]
  {:is-item true :name "Health potion" :char "!" :fg white
   :description "Instantly recover one health."
   :effect {:health 1}
   :x x :y y :id (keyword "item-" (.toString (java.util.UUID/randomUUID)))})

(defn make-amulet [[x y]]
  {:is-item true :name "The amulet" :char "*" :fg white
   :description "The reason you are down here."
   :effect {:going-up 1}
   :x x :y y :id (keyword "item-" (.toString (java.util.UUID/randomUUID)))})

(defn new-gold [[x y]]
  {:is-item true :name "Gold" :char "$" :fg (hsl 60 50 50)
   :x x :y y :id (keyword "item-" (.toString (java.util.UUID/randomUUID)))
   :effect {:gold 1}
   :description "Good for buying things."})

(defn new-item [[x y] is-store-item]
  (let [default {:is-item true
                 :x x :y y :id (keyword "item-" (.toString (java.util.UUID/randomUUID)))}]
    (merge default (random-item is-store-item))))

(defn make-treasures [grid difficulty candidate-positions]
  (let [candidates (shuffle candidate-positions)
        gold-positions (take (+ 34 (* 3 difficulty)) candidates)
        item-positions (take (+ 14 (* 3 difficulty)) (drop (count gold-positions) candidates))
        potion-positions (take 1 (drop (+ (count gold-positions) (count item-positions)) candidates))]
    (merge (into {} (for [t (map #(new-item % false) item-positions)] [(:id t) t]))
           (into {} (for [t (map new-gold gold-positions)] [(:id t) t]))
           (into {} (for [t (map make-health-potion potion-positions)] [(:id t) t])))))
