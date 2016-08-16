


;; ---------------------------------------------------------------------------------
;; ------------------ ----------- --------------------------------------------------
;; ------------------     CORE    --------------------------------------------------
;; ------------------     CODE    --------------------------------------------------
;; ------------------ ----------- --------------------------------------------------
;; ---------------------------------------------------------------------------------


(ns state-system.core
  (:require
    [state-system.fact-state :refer [->reversible-facts ->initial-facts]]
    [state-system.utils :refer :all]
    [utils :refer :all]
    [clojure.data :refer [diff]]))

;(def ->console identity)

;; engine assumption - game state is always the first element of a state log

;; ---------------------------------------------------------------------------------
;; ------------------ UTILS -------------------------------------------------------
;; ---------------------------------------------------------------------------------



;; ---------------------------------------------------------------------------------
;; ------------------ ENGINE -------------------------------------------------------
;; ---------------------------------------------------------------------------------

(defn update-state [transitioner [instant-state accretive-state input-state] event]
  (let [event-time (first event)
        new-instant-state (transitioner instant-state accretive-state input-state event)
        new-facts (->reversible-facts event-time (diff instant-state new-instant-state))
        new-input-state (conj input-state event)
        new-accretive-state (into accretive-state new-facts)]
    [new-instant-state new-accretive-state new-input-state]))

(defn engine [transitioner game-data new-events]
  "applies the events to the initial state to produce a renderable view of the data at a given time"
  (reduce (partial update-state transitioner) game-data new-events))



(defn input-engine [input-map instant accretive input event]
  (let [[time event-type & args] event]
    (apply (event-type input-map) time instant accretive input args)))

;; ---------------------------------------------------------------------------------
;; ------------------ EVENT STREAM -------------------------------------------------
;; ---------------------------------------------------------------------------------

;; event stream thoughts - event streams could add randomgenerator seeds to each input.
;; core system would remain referentially transparent,
;; but game would be nondeterministic.

(defn time-of-last-event [event-type input-view-state] (or (->> input-view-state reverse (drop-while #(-> % second (not= event-type))) first first) 0))
(def last-update (partial time-of-last-event :update))

(defn clock-updater [tick-rate input-view-state current-time]
  (let [time-since-last-update (- current-time (last-update input-view-state))]
    (when (<= tick-rate time-since-last-update)
      [[current-time :update]])))

(defn terminate-at [termination-time _ curr-time] (when (< termination-time curr-time) [[curr-time :finished]]))

;; ---------------------------------------------------------------------------------
;; ------------------ MAIN SYSTEM --------------------------------------------------
;; ---------------------------------------------------------------------------------

;; main system thoughts.
;; should be able to capture data and dump it into interactive eden viewer application
;; should be able to set start and stop record points for capturing data
;; should be able to auto generate unit tests from the captured data


(defn game-update
  ([game instant-state] (game-update game (current-time-millis) instant-state (->reversible-facts 0 (diff {} instant-state)) [instant-state] []))
  ([game curr-time instant-state accretive-state input-view-state events]
   (let [{:keys [output-streams input-streams transitioner]} game
         [new-instant-state new-accretive-state new-input-state] (engine transitioner instant-state accretive-state input-view-state events)
         new-events (vec (mapcat #(% new-input-state (current-time-millis)) input-streams))]
     (do (doall (map #(% instant-state curr-time) output-streams))
         [(current-time-millis) new-instant-state new-accretive-state new-input-state new-events]))))

(defn finished? [game-data]
  (->> game-data
       last
       (map last)
       (some #(= % :finished))))

(defn play [game instant & {:keys [until-finished print-iterations] :or {until-finished true print-iterations false}}]
  (let [game-sequence (iterate #(apply game-update game %) [instant])]
    (if until-finished
      (->> game-sequence
           (drop 1)
           (drop-while #(not (finished? %)))
           first)
      game-sequence)))

(def DEMO-STATE {:player {:position 0}})
(def DEMO-FACTS (->initial-facts DEMO-STATE))
(def DEMO-TRANSITIONER
  (partial input-engine
           {:update (fn [t state _ _]
                      (assoc-in state [:player :position] (inc (get-in state [:player :position]))))}))


(defn test-state-system []
  (println "beginning test")
  (let [
        start-time (current-time-millis)
        demo-state DEMO-STATE
        demo-facts DEMO-FACTS
        demo-transitioner DEMO-TRANSITIONER
        demo-game {:transitioner  demo-transitioner
                   :input-streams [(partial terminate-at (+ start-time 500))
                                   (partial clock-updater 50)]}
        ]
    (demo-transitioner demo-state demo-facts [demo-state] [1 :update])
    (binary-split [demo-state [1 :update]] 1 :key-fn first :compare-fn <=)
    (engine demo-transitioner demo-state demo-facts [demo-state] [[1 :update] [2 :update]])
    (game-update {:transitioner demo-transitioner :input-streams [(fn [_ curr-time] [[curr-time :update]])]} demo-state)
    (->> [demo-state]
         (iterate #(apply game-update {:transitioner demo-transitioner :input-streams [(fn [_ curr-time] [[curr-time :update]])]} %))
         (take 5)
         last)
    (terminate-at (+ start-time 5) nil (+ start-time 10))
    (->> [demo-state]
         (iterate #(apply game-update demo-game %))
         (drop-while #(not (finished? %)))
         first
         )
    (finished? [{:player {:position 6}} [[:player :position [nil 0] 0] [:player :position [0 1] 1469344640462] [:player :position [1 2] 1469344640463] [:player :position [2 3] 1469344640464] [:player :position [3 4] 1469344640465] [:player :position [4 5] 1469344640466] [:player :position [5 6] 1469344640467]] [{:player {:position 0}} [1469344640462 :update] [1469344640462 :update] [1469344640463 :update] [1469344640464 :update] [1469344640465 :update] [1469344640466 :update]] [[1469344640467 :finished] [1469344640467 :update]]])
    (play demo-game demo-state)
    ))
;; ---------------------------------------------------------------------------------
;; ------------------ ----------- --------------------------------------------------
;; ------------------     CORE    --------------------------------------------------
;; ------------------     CODE    --------------------------------------------------
;; ------------------ ----------- --------------------------------------------------
;; ---------------------------------------------------------------------------------


; state representations -->
; input-view: contains the entire history of a game, including all changes to values
; instantaneous: this is just a shortcut for rendering, it creates a renderable view of a game at a particular time index.
;
; time is the key. we dont need to consider "new events".  an engine should simple generate a view of an input-view
; value for a given time index.



; today's refactor challenge.  Engine needs to be simplified to a thing that takes a state and an event and produces(ns state-system.core)
