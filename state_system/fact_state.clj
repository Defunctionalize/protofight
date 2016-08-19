(ns state-system.fact-state
  (:require [clojure.set :as s]))
;
;(defn reverse-fact [fact]
;  (into [] (concat (take 2 fact) (vector (reverse (nth fact 2))) (vector (last fact)))))
;
;(defn advance-state
;  ([instant next-transition]
;   (let [[id attribute [old-value new-value] value time] next-transition]
;     (assoc-in instant [id attribute] new-value))))
;
;(let [sample-fact [1 :position [nil 0] 1]
;      sample-fact2 [1 :position [0 4] 2]]
;  (-> {}
;      (advance-state sample-fact)
;      (advance-state sample-fact2)
;      ))

(defn ->entattrs [state]
  (set (for [[entity attributes] state
             [attribute value] attributes]
         [entity attribute])))

(defn ->reversible-fact [timestamp entattr oldval newval]
  (into entattr [[oldval newval] timestamp]))

(defn new-entattr-facts [timestamp b a-entattrs]
  (for [created-entattr (s/difference (->entattrs b) a-entattrs)
        :let [new-val (get-in b created-entattr)]]
    (->reversible-fact timestamp created-entattr nil new-val)))

(defn modified-and-deleted [equal? timestamp a b entattr]
  (let [old-val (get-in a entattr)
        new-val (get-in b entattr)]
    (when-not (equal? old-val new-val) (->reversible-fact timestamp entattr old-val new-val))))

(defn ->new-facts [equal? timestamp a b]
  (let [entattrs (->entattrs a)
        new (new-entattr-facts timestamp b entattrs)
        modified-and-deleted (keep identity (map (partial modified-and-deleted equal? timestamp a b) entattrs))]
    (into new modified-and-deleted))
  )


(let [timestamp 10
      state-a {:player {:position [0 0] :update-type :player}
               :config {:walk-speed 5 :dash-speed 50}
               :input  {:vertical 0.0 :horizontal 0.0}}
      state-b {:player {:position [0 0] :update-type :player}
               :config {:walk-speed 5 :dash-speed 50}
               :input  {:vertical 1.0 :horizontal 0.5}}
      state-c {:player {:position [0 0] :update-type :player}
               :config {:walk-speed 5 :dash-speed 50}
               :input  {:vertical 0.0 :horizontal 0.0 :dash 30}}
      state-d {:player {:position [0 0] :update-type :player}
               :config {:walk-speed 5 :dash-speed 50}
               :input  {:vertical 0.0}}
      state-e {:player {:position [1 0]}
               :config {:walk-speed 5 :dash-speed 40 :outlook {:disposition :sunny}}}
      ]
  ;(->entattrvals state-a)
  ;(->reversible-fact time [:player :position [0 0]] [:player :position [1 0]])

  (= (->new-facts = timestamp state-a state-c) [[:input :dash [nil 30] timestamp]])
  (= (->new-facts = timestamp state-a state-d) [[:input :horizontal [0.0 nil] timestamp]])
  (= (->new-facts = timestamp state-a state-b) [[:input :vertical [0.0 1.0] timestamp] [:input :horizontal [0.0 0.5] timestamp]])
  (= (set (->new-facts = timestamp state-a state-e))
     (set [[:player :position [[0 0] [1 0]] timestamp]
           [:player :update-type [:player nil] timestamp]
           [:config :dash-speed [50 40] timestamp]
           [:config :outlook [nil {:disposition :sunny}] timestamp]
           [:input :horizontal [0.0 nil] timestamp]
           [:input :vertical [0.0 nil] timestamp]
           ]))
  )

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
