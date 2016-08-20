(ns game-state.core
  (:require [state-system.core :as ss]
            [utils :refer :all]))

(defmacro when-> [value predicate & expressions]
  `(if (~predicate ~value) (-> ~value ~@expressions) ~value))

(defn handle-movement [agent speed [h v] application-speed-mod]
  (let [[agent-x agent-y] (:position agent)
        horiz (* h speed application-speed-mod)
        vert (* v speed application-speed-mod)]
    (assoc agent :position [(+ agent-x horiz) (+ agent-y vert)])))

(defn handle-dash [player dash-speed]
  (update player :side-effects (comp vec conj) [:add-impulse dash-speed]))


(defn deconstruct [entity-key entity-value]
  (let [entity (-> entity-value
                   (dissoc :side-effects)
                   (dissoc :state-manipulations)
                   )]
    [entity-key entity (:side-effects entity-value) (:state-manipulations entity-value)]))

(defn fire-bullet [player bullet-speed]
  (update player :state-manipulations (comp vec conj) [:create :bullet bullet-speed (:position player) [1 0]]))

(def pressed? #(constantly (pos? %)))

(def update-map
  {:player
   (fn [player tt state _ _ app-speed-mod]
     (let [{{:keys [walk-speed dash-speed bullet-speed]} :config
            {:keys [dash horizontal vertical fire]}      :input} state]
       (-> player
           (handle-movement walk-speed [horizontal vertical] app-speed-mod)
           (when-> (pressed? dash)
                   (handle-dash dash-speed))
           (when-> (pressed? fire)
                   (fire-bullet bullet-speed))
           )))
   :bullet
   (fn [bullet _ _ _ _ app-speed-mod]
     (handle-movement bullet (:speed bullet) (:direction bullet) app-speed-mod))})

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
   (fn [state speed pos direction]
     {:position pos
      :speed speed
      :direction direction
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
                   (map #(updated-entity % tt instant accretive input fixed-delta-time))
                   (apply map vector)
                   (let->> [entity-keys entity-values side-effects state-manipulations]
                           (let [mapped-side-effects (into {} (filter val (zipmap entity-keys side-effects)))]
                             (-> (zipmap entity-keys entity-values)
                                 (#(reduce apply-effect % (apply concat (filter identity state-manipulations))))
                                 (update :side-effects (partial merge-with into) mapped-side-effects))))))
            :input
            (fn [tt state _ _ input-key value]
              (assoc-in state [:input input-key] value))

            :effects-performed
            (fn [tt state _ _ & entity-keys]
              (dissoc state :side-effects))

            :initialize-state
            (fn [tt state _ _]
              {:player {:position [0 0] :type :player}
               :config {:walk-speed 5 :dash-speed 50 :bullet-speed 50}
               :input  {:vertical 0.0 :horizontal 0.0 :dash 0 :fire 0}})}))