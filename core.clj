(ns core
  (:require
    [arcadia.core :as a]
    [arcadia.linear :as al]
    [utils :refer :all]
    [state-system.fact-state :refer [->new-facts]]
    [game-state.core :as gs])
  (:import [UnityEngine Debug GameObject Transform SpriteRenderer
            Sprite Rect Vector2 Input Time Transform Resources
            Rigidbody2D ForceMode BoxCollider2D Vector3]
           ArcadiaState))

(set! *warn-on-reflection* true)

(def components
  {:rigid     Rigidbody2D
   :transform Transform})

(defn make-sprite
  ([path] (make-sprite path path))
  ([path name]
   (let [sprite (Sprite/Create (Resources/Load path) (Rect. 0 0 32 32) (al/v2 0 0))]
     (do (set! (.name sprite) name)
         sprite))))

(defn into-state [entity key value]
  "returns the the value of the (a/state entity key) or stores and returns value"
  (or (a/state entity key) (val (a/set-state! entity key value))))

(defn add-inputs! [helios & input-events]
  (let [^Atom state-atom (.state ^ArcadiaState (a/cmpt helios ArcadiaState))
        current-time (current-time-millis)]
    (swap! state-atom update :new-events into (map #(into [current-time] %) input-events))))


(defn consume-event! [transitioner helios new-event]
  (let [^Atom state-atom (.state ^ArcadiaState (a/cmpt helios ArcadiaState))
        {:keys [instant accretive input]} (a/state helios)
        new-instant (transitioner instant accretive input new-event)
        new-facts (->new-facts = (first new-event) instant new-instant)]
    (swap! state-atom #(-> %
                           (assoc :instant new-instant)
                           (update :accretive into new-facts)
                           (update :input conj new-event)
                           ;(<!> (a/log))
                           ))))

(defn consume-queued-events! [transitioner helios]
  ;(a/log "new events: " (a/state helios :new-events))
  (doall (map #(consume-event! transitioner helios %) (a/state helios :new-events)))
  (a/set-state! helios :new-events []))

(defn advance-engine! [helios event]
  (add-inputs! helios event)
  (consume-queued-events! gs/game-engine helios))

(defn get-stored-component [unity-entity component-key]
  (let [component-type (or (component-key components)
                           (throw (Exception. (str "Invalid Component Access: " component-key " maps to no components"))))]
    (into-state unity-entity component-key (a/cmpt unity-entity component-type))))

(def recipes
  {:player
   (fn [entities] (-> (GameObject.)
                      (<!> (-> (a/cmpt+ Rigidbody2D)
                               (<!> (-> .gravityScale (set! 0)))))
                      (<!> (a/cmpt+ BoxCollider2D))
                      (<!> (-> (a/cmpt+ SpriteRenderer)
                               .sprite
                               (set! (make-sprite "Sprites/Player"))))))})

(defn be-created! [recipes helios-entities object-recipe-key]
  (if-let [existing-entity (object-recipe-key helios-entities)]
    existing-entity
    ((object-recipe-key recipes) helios-entities)))

(defn build-world! [helios]
  (a/set-state! helios :instant {})
  (a/set-state! helios :accretive [])
  (a/set-state! helios :input [{}])
  (a/set-state! helios :new-events [[(current-time-millis) :initialize-state]])
  (consume-queued-events! gs/game-engine helios)
  (a/set-state! helios :entities {:player (be-created! recipes (a/state helios :entities) :player)}))

(def side-effect-map
  {:add-impulse
   (fn [unity-entity impulse-magnitude]
     (let [^Rigidbody2D rb (get-stored-component unity-entity :rigid)]
       (.AddForce rb (al/v2* (al/v2 1 0) impulse-magnitude) ForceMode/VelocityChange)))})

(defn destroy-world! [helios]
  (doall (map #(a/destroy-immediate (val %)) (a/state helios :entities)))
  (a/set-state! helios :entities {})
  (a/set-state! helios :instant nil)
  (a/set-state! helios :accretive [])
  (a/set-state! helios :input [])
  (a/set-state! helios :new-events []))

(def get-game-state #(a/state % :instant))

(defn floor [divisor target]
  (-> target (quot divisor) (* divisor)))


(defn log-debug! [helios]
  ;(a/log (type (.state (a/cmpt helios ArcadiaState))))
  (a/log (floor 0.1 12.54))
  ;(add-inputs! helios [:fixed-update 0.2 {}])
  ;(consume-queued-events! gs/game-engine helios)
  )

(defn handle-input [helios]
  ((cond
     (pos? (Input/GetAxisRaw "Submit")) build-world!
     (pos? (Input/GetAxisRaw "Cancel")) destroy-world!
     (pos? (Input/GetAxisRaw "log-debug")) log-debug!
     :else (constantly nil)) helios))


(defn add-input-axes-to-game-state [helios]
  (apply add-inputs! helios [[:input :horizontal (Input/GetAxisRaw "horizontal")]
                             [:input :vertical (Input/GetAxisRaw "vertical")]
                             [:input :dash (Input/GetAxis "dash")]]))

(defn match-position [helios]
  (let [state-player (:player (get-game-state helios))
        unity-player (:player (a/state helios :entities))
        ^Rigidbody2D rb (get-stored-component unity-player :rigid)
        [x y] (:position state-player)]
    (.MovePosition rb (al/v2 x y))))

(defn perform-side-effect [unity-entity [side-effect-type & side-effect-args]]
  (apply (side-effect-map side-effect-type) unity-entity side-effect-args))

(defn perform-entity-side-effects [helios entity-key side-effects]
  (let [unity-entity (entity-key (a/state helios :entities))]
    (doall (map (partial perform-side-effect unity-entity) side-effects))))

(defn get-side-effects [helios]
  (let [gs (get-game-state helios)]
    (zipmap (keys gs) (map :side-effects (vals gs)))))

(defn perform-all-side-effects [helios]
  (doall (map #(apply perform-entity-side-effects helios %) (get-side-effects helios)))
  (add-inputs! helios [:effects-performed]))

(defn adjust-player [helios]
  (match-position helios)
  (perform-all-side-effects helios)
  )

(defn update-helios [helios]
  (handle-input helios)
  (when (a/state helios :instant)
    (add-input-axes-to-game-state helios)
    )
  )

(defn ->state-entity [[entity-key unity-entity]]
  (let [current-pos (.position ^Transform (get-stored-component unity-entity :transform))]
    {entity-key {:position [(.x current-pos) (.y current-pos)]}}))

;(defn observations [helios]
;  (let [old-state (get-game-state helios)
;        current-unity-state (into {} (map ->state-entity (a/state helios :entities)))]
;    current-unity-state))

(defn fixed-update-helios [helios]
  (when (a/state helios :instant)
    (let [new-observations (into {} (map ->state-entity (a/state helios :entities)))]
      (advance-engine! helios [:fixed-update Time/fixedDeltaTime new-observations])
      )
    (adjust-player helios)
    )
)

(defn in-the-beginning [helios]
  (a/hook+ helios :update #'update-helios)
  (a/hook+ helios :fixed-update #'fixed-update-helios))
