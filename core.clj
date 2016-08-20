(ns core
  (:require
    [arcadia.core :as a]
    [arcadia.linear :as al]
    [utils :refer :all]
    [game-state.state-system :refer [->new-facts]]
    [game-state.core :as gs])
  (:import [UnityEngine Debug GameObject Transform SpriteRenderer
            Sprite Rect Vector2 Input Time Transform Resources
            Rigidbody2D ForceMode BoxCollider2D Vector3]
           ArcadiaState))

(def components
  {:rigid     Rigidbody2D
   :transform Transform})

(defn make-sprite
  ([path] (make-sprite path path))
  ([path name]
   (let [sprite (Sprite/Create (Resources/Load path) (Rect. 0 0 32 32) (al/v2 0 0))]
     (do (set! (.name sprite) name)
         sprite))))

(defn ensure-from-state [entity key value]
  "returns the the value of the (a/state entity key) or stores and returns value"
  (or (a/state entity key) (val (a/set-state! entity key value))))

(defn add-inputs! [master-game-obj & input-events]
  (let [^Atom state-atom (.state ^ArcadiaState (a/cmpt master-game-obj ArcadiaState))
        current-time (current-time-millis)]
    (swap! state-atom update :new-events into (map #(into [current-time] %) input-events))))


(defn consume-event! [transitioner master-game-obj new-event]
  (let [^Atom state-atom (.state ^ArcadiaState (a/cmpt master-game-obj ArcadiaState))
        {:keys [instant accretive input]} (a/state master-game-obj)
        new-instant (transitioner instant accretive input new-event)
        new-facts (->new-facts = (first new-event) instant new-instant)]
    (swap! state-atom #(-> %
                           (assoc :instant new-instant)
                           (update :accretive into new-facts)
                           (update :input conj new-event)))))

(defn consume-queued-events! [transitioner master-game-obj]
  (doall (map #(consume-event! transitioner master-game-obj %) (a/state master-game-obj :new-events)))
  (a/set-state! master-game-obj :new-events []))

(defn advance-engine! [master-game-obj event]
  (add-inputs! master-game-obj event)
  (consume-queued-events! gs/game-engine master-game-obj))

(defn get-stored-component [unity-entity component-key]
  (let [component-type (or (component-key components)
                           (throw (Exception. (str "Invalid Component Access: " component-key " maps to no components"))))]
    (ensure-from-state unity-entity component-key (a/cmpt unity-entity component-type))))

(def recipes
  {:player
   (fn [entities] (-> (GameObject.)
                      (<!> (-> (a/cmpt+ Rigidbody2D)
                               (<!> (-> .gravityScale (set! 0)))))
                      (<!> (a/cmpt+ BoxCollider2D))
                      (<!> (-> (a/cmpt+ SpriteRenderer)
                               .sprite
                               (set! (make-sprite "Sprites/Player"))))))})

(defn be-created! [recipes master-game-obj-entities object-recipe-key]
  (if-let [existing-entity (object-recipe-key master-game-obj-entities)]
    existing-entity
    ((object-recipe-key recipes) master-game-obj-entities)))

(defn build-world! [master-game-obj]
  (a/set-state! master-game-obj :instant {})
  (a/set-state! master-game-obj :accretive [])
  (a/set-state! master-game-obj :input [{}])
  (a/set-state! master-game-obj :new-events [[(current-time-millis) :initialize-state]])
  (consume-queued-events! gs/game-engine master-game-obj)
  (a/set-state! master-game-obj :entities {:player (be-created! recipes (a/state master-game-obj :entities) :player)}))

(def side-effect-map
  {:add-impulse
   (fn [unity-entity impulse-magnitude]
     (let [^Rigidbody2D rb (get-stored-component unity-entity :rigid)]
       (.AddForce rb (al/v2* (al/v2 1 0) impulse-magnitude) ForceMode/VelocityChange)))})

(defn destroy-world! [master-game-obj]
  (doall (map #(a/destroy-immediate (val %)) (a/state master-game-obj :entities)))
  (a/set-state! master-game-obj :entities {})
  (a/set-state! master-game-obj :instant nil)
  (a/set-state! master-game-obj :accretive [])
  (a/set-state! master-game-obj :input [])
  (a/set-state! master-game-obj :new-events []))

(def get-game-state #(a/state % :instant))

(defn log-debug! [master-game-obj]
  (a/log "INSTANT STATE: " (a/state master-game-obj :instant))
  (a/log "INPUT STATE: " (a/state master-game-obj :input))
  (a/log "ACCRETIVE STATE: " (a/state master-game-obj :accretive))
  )

(defn handle-input [master-game-obj]
  ((cond
     (pos? (Input/GetAxisRaw "submit")) build-world!
     (pos? (Input/GetAxisRaw "cancel")) destroy-world!
     (pos? (Input/GetAxisRaw "log-debug")) log-debug!
     :else (constantly nil)) master-game-obj))

(defn add-input-axes-to-game-state [master-game-obj]
  (apply add-inputs! master-game-obj [[:input :horizontal (Input/GetAxisRaw "horizontal")]
                             [:input :vertical (Input/GetAxisRaw "vertical")]
                             [:input :dash (Input/GetAxis "dash")]
                             [:input :fire (Input/GetAxis "fire")]]))

(defn match-position [master-game-obj]
  (let [state-player (:player (get-game-state master-game-obj))
        unity-player (:player (a/state master-game-obj :entities))
        ^Rigidbody2D rb (get-stored-component unity-player :rigid)
        [x y] (:position state-player)]
    (.MovePosition rb (al/v2 x y))))

(defn perform-side-effect [unity-entity [side-effect-type & side-effect-args]]
  (apply (side-effect-map side-effect-type) unity-entity side-effect-args))

(defn perform-entity-side-effects [master-game-obj entity-key side-effects]
  (let [unity-entity (entity-key (a/state master-game-obj :entities))]
    (doall (map (partial perform-side-effect unity-entity) side-effects))))

(defn perform-all-side-effects [master-game-obj]
  (doall (map #(apply perform-entity-side-effects master-game-obj %) (-> master-game-obj get-game-state :side-effects)))
  (add-inputs! master-game-obj [:effects-performed]))

(defn adjust-player [master-game-obj]
  (match-position master-game-obj)
  (perform-all-side-effects master-game-obj))

(defn update-master-game-obj [master-game-obj]
  (handle-input master-game-obj)
  (when (a/state master-game-obj :instant)
    (add-input-axes-to-game-state master-game-obj)))

(defn ->state-entity [[entity-key unity-entity]]
  (let [current-pos (.position ^Transform (get-stored-component unity-entity :transform))]
    {entity-key {:position [(.x current-pos) (.y current-pos)]}}))

(defn fixed-update-master-game-obj [master-game-obj]
  (when (a/state master-game-obj :instant)
    (let [new-observations (into {} (map ->state-entity (a/state master-game-obj :entities)))]
      (advance-engine! master-game-obj [:fixed-update Time/fixedDeltaTime new-observations]))
    (adjust-player master-game-obj)))

(defn in-the-beginning [master-game-obj]
  (a/hook+ master-game-obj :update #'update-master-game-obj)
  (a/hook+ master-game-obj :fixed-update #'fixed-update-master-game-obj))


;----------------
;- Common debugging operations:
;1 - NullReferenceException:
;    you probably changed a game state related keyword and its causing map lookups to fail.  grep for uses of old key
;2 - cant cast Coll -> Key
;    could be you're filtering a dict and forgot to cast it back into a dict