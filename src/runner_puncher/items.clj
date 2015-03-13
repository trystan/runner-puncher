(ns runner_puncher.items
  (:require [runner_puncher.framework :refer :all]))


(defn random-item [is-store-item]
  (let [prefix (rand-nth [{:name "heavy" :description "-1 movement." :effect {:max-steps -1}}
                          {:name "uncomfortable" :description "-1 knockback." :effect {:knockback-amount -1}}
                          {:name "smelly" :description "Increase shop prices by $5." :effect {:affect-prices 5}}
                          {:name "decent" :description "" :effect {}}])
        noun (rand-nth [{:char "[" :slot "footwear" :name "shoes"}
                        {:char "]" :slot "armor" :name "cape"}
                        {:char "(" :slot "headwear" :name "helm"}
                        {:char ")" :slot "weapon" :name "knuckles"}])
        postfix (rand-nth [{:name "of running" :description "+2 movement."
                            :effect {:max-steps 2}}
                           {:name "of punching" :description "+2 knockback."
                            :effect {:knockback-amount 2}}
                           {:name "of life" :description "+1 life."
                            :effect {:max-health 1 :health 1}}
                           {:name "of standing" :description "Resist knockback 50%."
                            :effect {:resist-knockback 1}}
                           {:name "of anti-poison" :description "Ignore poison attacks."
                            :effect {:ignore-poison 1}}
                           {:name "of charming" :description "Decrease shop prices by $5."
                            :effect {:affect-prices -3}}
                           {:name "of defense" :description "+1 defence."
                            :effect {:defence 1}}
                           {:name "of attack" :description "+1 attack."
                            :effect {:attack 1}}
                           {:name "of webwalking" :description "Ignore webs."
                            :effect {:ignore-webs 1}}
                           {:name "of acidwalking" :description "Ignore acid pools."
                            :effect {:ignore-acid-floor 1}}])
        [prefix postfix] (cond
                          is-store-item
                          [prefix postfix]
                          (< (rand) 0.66)
                          [prefix nil]
                          :else
                          [nil postfix])
        item-name (:name noun)
        item-name (if prefix (str (:name prefix) " " item-name) item-name)
        item-name (if postfix (str item-name " " (:name postfix)) item-name)
        item-name (clojure.string/capitalize item-name)
        description ""
        description (if postfix (str description " " (:description postfix)) description)
        description (if prefix (str description " " (:description prefix)) description)
        description (.trim description)
        effect {}
        effect (if prefix (merge-with + effect (:effect prefix)) effect)
        effect (if postfix (merge-with + effect (:effect postfix)) effect)]
  {:price 0 :name item-name :slot (:slot noun) :description description
   :effect effect :char (:char noun) :fg light}))

(defn make-amulet [[x y]]
  {:is-item true :name "The amulet" :char "*" :fg (hsl 80 99 99)
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

(defn make-treasures [grid depth candidate-positions]
  (let [candidates (shuffle candidate-positions)
        gold-positions (take (+ 29 depth) candidates)
        item-positions (take (+ 9 depth) (drop (count gold-positions) candidates))]
    (merge (into {} (for [t (map #(new-item % false) item-positions)] [(:id t) t]))
           (into {} (for [t (map new-gold gold-positions)] [(:id t) t])))))
