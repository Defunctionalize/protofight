(ns game-state.core
  (:require [state-system.core :as ss]
            [utils :refer :all]))

(defmacro when-> [value predicate & expressions]
  `(if (~predicate ~value) (-> ~value ~@expressions) ~value))

(defn handle-walk [player speed h v application-speed-mod]
  (let [[player-x player-y] (:position player)
        horiz (* h speed application-speed-mod)
        vert (* v speed application-speed-mod)]
    (assoc player :position [(+ player-x horiz) (+ player-y vert)])))

(defn handle-dash [player dash-speed]
  (update player :side-effects (comp vec conj) [:add-impulse dash-speed]))


(def update-map
  {:player
   (fn [player tt state _ _ app-speed-mod]
     (let [{{:keys [walk-speed dash-speed]} :config
            {:keys [dash horizontal vertical]} :input} state]
       ;(>log> dash)
       (-> player
           (handle-walk walk-speed horizontal vertical app-speed-mod)
           ;(when-> (constantly (pos? dash))
           ;        (handle-dash dash-speed))
           )))})

(defn updated-entity [[entity-key entity] & args]
  {entity-key (if-let [update-func (-> entity :update-type update-map)]
                (apply update-func entity args)
                entity)})


(defn deep-merge
  "Recursively merges maps. If vals are not maps, the last value wins."
  [& vals]
  (if (every? map? vals)
    (apply merge-with deep-merge vals)
    (last vals)))

(def game-engine
  (partial ss/input-engine
           {:fixed-update
            (fn [tt instant accretive input fixed-delta-time new-observations]
              ;(>log> instant)
              (->> (deep-merge instant new-observations)
                   (map #(updated-entity % tt instant accretive input fixed-delta-time))
                   (into {})
                   ;>log>
                   ))

            :input
            (fn [tt state _ _ input-key value]
              (assoc-in state [:input input-key] value))

            :effects-performed
            (fn [tt state _ _ & entity-keys]
              (let [entity-specified? (if entity-keys (set entity-keys) (constantly true))]
                (->> state
                     (map #(if (entity-specified? (key %))
                            [(key %) (dissoc (val %) :side-effects)]
                            %))
                     (into {}))))

            :initialize-state
            (fn [tt state _ _]
              ;(>log> state)
              {:player {:position [0 0] :update-type :player}
               :config {:walk-speed 5 :dash-speed 50}
               :input  {:vertical 0.0 :horizontal 0.0}})}))