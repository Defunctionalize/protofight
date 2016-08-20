(ns game-state.state-system
  (:require [clojure.set :as s]))



(defn input-engine [input-map instant accretive input event]
  (let [[time event-type & args] event]
    (apply (event-type input-map) time instant accretive input args)))

(defn time-of-last-event [event-type input-view-state] (or (->> input-view-state reverse (drop-while #(-> % second (not= event-type))) first first) 0))
(def last-update (partial time-of-last-event :update))

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
      state-a {:player {:position [0 0] :type :player}
               :config {:walk-speed 5 :dash-speed 50}
               :input  {:vertical 0.0 :horizontal 0.0}}
      state-b {:player {:position [0 0] :type :player}
               :config {:walk-speed 5 :dash-speed 50}
               :input  {:vertical 1.0 :horizontal 0.5}}
      state-c {:player {:position [0 0] :type :player}
               :config {:walk-speed 5 :dash-speed 50}
               :input  {:vertical 0.0 :horizontal 0.0 :dash 30}}
      state-d {:player {:position [0 0] :type :player}
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
           [:player :type [:player nil] timestamp]
           [:config :dash-speed [50 40] timestamp]
           [:config :outlook [nil {:disposition :sunny}] timestamp]
           [:input :horizontal [0.0 nil] timestamp]
           [:input :vertical [0.0 nil] timestamp]
           ]))
  )