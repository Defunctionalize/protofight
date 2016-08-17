(ns game-state.core
  (:require [state-system.core :as ss]
            [utils :refer :all]))

(defn handle-walk [state application-speed-mod]
  (let [speed (get-in state [:config :speed])
        [player-x player-y] (get-in state [:player :position])
        horiz (-> state :input :horizontal (* speed application-speed-mod))
        vert (-> state :input :vertical (* speed application-speed-mod))]
    ;(>log> fixed-delta-time)
    (-> state
        (assoc-in [:player :position] [(+ player-x horiz)
                                       (+ player-y vert)]))))

(defn handle-dash [state]
  (let [dash-speed (get-in state [:config :dash-speed])
        dashing-pressed? (pos? (get-in state [:input :dash]))]
    (if dashing-pressed?
      (update-in state [:side-effects :player] (comp vec conj) [:add-impulse dash-speed])
      state)))

(def game-engine
  (partial ss/input-engine
           {:fixed-update
            (fn [tt state _ _ fixed-delta-time new-observations]
              (-> state
                  (merge new-observations)
                  (handle-walk fixed-delta-time)
                  handle-dash))

            :input
            (fn [tt state _ _ input-key value]
              (assoc-in state [:input input-key] value))

            :effects-performed
            (fn [tt state _ _ entity-keys]
              (assoc state :side-effects
                           (if (= :all entity-keys)
                             {}
                             (filter (-> entity-keys set complement) (:side-effects state)))))

            :initialize-state
            (fn [tt state _ _]
              {:player {:position [0 0]}
               :config {:speed 5 :dash-speed 10}
               :input  {:vertical 0.0 :horizontal 0.0}})}))