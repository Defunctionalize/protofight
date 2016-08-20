(ns game-state.core
  (:require [state-system.core :as ss]
            [utils :refer :all]))

(set! *warn-on-reflection* true)

(defmacro when-> [value predicate & expressions]
  `(if (~predicate ~value) (-> ~value ~@expressions) ~value))

(defn handle-walk [player speed h v application-speed-mod]
  (let [[player-x player-y] (:position player)
        horiz (* h speed application-speed-mod)
        vert (* v speed application-speed-mod)]
    (assoc player :position [(+ player-x horiz) (+ player-y vert)])))

(defn handle-dash [player dash-speed]
  (update player :side-effects (comp vec conj) [:add-impulse dash-speed]))


(defn deconstruct [entity-key entity-value]
  ;(>logl> "key" entity-key)
  ;(>logl> "value" entity-value)
  (let [entity (-> entity-value
                   (dissoc :side-effects)
                   (dissoc :state-manipulations)
                   )]
    [entity-key entity (:side-effects entity-value) (:state-manipulations entity-value)]))

(def update-map
  {:player
   (fn [player tt state _ _ app-speed-mod]
     (let [{{:keys [walk-speed dash-speed]}    :config
            {:keys [dash horizontal vertical]} :input} state]
       (-> player
           (handle-walk walk-speed horizontal vertical app-speed-mod)
           (when-> (constantly (pos? dash))
                   (handle-dash dash-speed)))))})

(defn updated-entity [[entity-key entity] & args]
  (deconstruct entity-key (if-let [update-func (-> entity :type update-map)]
                            (apply update-func entity args)
                            entity)))

(defn deep-merge
  "Recursively merges maps. If vals are not maps, the last value wins."
  [& vals]
  (if (every? map? vals)
    (apply merge-with deep-merge vals)
    (last vals)))

(def RECIPES
  {:bullet
   (fn [state x y]
     {:position [x y]
      :type :bullet})})

(def EFFECTS
  {:create
   (fn [state entity-type & args]
     (assoc state (gensym entity-type) (apply (RECIPES entity-type) state args)))})

(defn apply-effect [state [effect-type & effect-args]]
  (apply (EFFECTS effect-type) state effect-args))

(def game-engine
  (partial ss/input-engine
           {:fixed-update
            (fn [tt instant accretive input fixed-delta-time new-observations]
              (->> (deep-merge instant new-observations)
                   ;(>logl> "observed entities")
                   (map #(updated-entity % tt instant accretive input fixed-delta-time))
                   (apply map vector)
                   (let->> [entity-keys entity-values side-effects state-manipulations]
                           (-> (zipmap entity-keys entity-values)
                               (#(reduce apply-effect % (filter identity side-effects)))
                               (assoc :side-effects (into {} (filter second (map list entity-keys side-effects))))
                               ))))

            :input
            (fn [tt state _ _ input-key value]
              (assoc-in state [:input input-key] value))

            :effects-performed
            (fn [tt state _ _ & entity-keys]
              ;(let [entity-specified? (if entity-keys (set entity-keys) (constantly true))]
              ;  (->> state
              ;       (map #(if (entity-specified? (key %))
              ;              [(key %) (dissoc (val %) :side-effects)]
              ;              %))
              ;       (into {}))
              ;  )
              (dissoc state :side-effects))

            :initialize-state
            (fn [tt state _ _]
              ;(>log> state)
              {:player {:position [0 0] :type :player}
               :config {:walk-speed 5 :dash-speed 50}
               :input  {:vertical 0.0 :horizontal 0.0}})}))