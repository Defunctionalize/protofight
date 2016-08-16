(ns state-system.fact-state)

(ns state-system.fact-state
  (:require [clojure.data :refer [diff]]))

(defn reverse-fact [fact]
  (into [] (concat (take 2 fact) (vector (reverse (nth fact 2))) (vector (last fact)))))

(defn advance-state
  ([instant next-transition]
   (let [[id attribute [old-value new-value] value time] next-transition]
     (assoc-in instant [id attribute] new-value))))

(let [sample-fact [1 :position [nil 0] 1]
      sample-fact2 [1 :position [0 4] 2]]
  (-> {}
      (advance-state sample-fact)
      (advance-state sample-fact2)
      ))

(defn ->facts [time [only-a only-b both]]
  (apply concat (for [[id attributes] only-b]
                  (for [[attribute value] attributes]
                    [id attribute value time]))))

(defn fact->reversible-fact [previous-values fact]
  (let [path-to-value (vec (take 2 fact))]
    (vec (assoc fact 2 [(get-in previous-values path-to-value) (nth fact 2)]))))

(fact->reversible-fact {1 {:weight 4}} [1 :weight 5 2])

(defn ->reversible-facts [timestamp diff]
  (let [[only-a] diff
        facts (->facts timestamp diff)]
    (vec (map (partial fact->reversible-fact only-a) facts))))

(defn ->initial-facts [state]
  (->reversible-facts 0 (diff {} state)))
;
;
;(let [time 2
;      state-diff [{1 {:weight 4}} {1 {:weight 5, :height 3}} {1 {:position 0}}] ]
;  (->> state-diff
;       (->facts time)
;       (diff [[1 :weight 5 time] [1 :height 3 time]])
;       (take 2) (= [nil nil]) is
;       )
;  (->> state-diff
;       (->reversible-facts time)
;       (diff [[1 :weight [4 5] time] [1 :height [nil 3] time]])
;       (take 2) (= [nil nil]) is
;       ))
