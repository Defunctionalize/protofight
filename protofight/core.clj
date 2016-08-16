(ns protofight.core
  (:require
    [arcadia.core :as a]
            [arcadia.linear :as al]
            [utils :refer :all]
            [state-system.core :as ss]
            [game-state.core :as gs]
            )
  (:import [UnityEngine Debug GameObject Transform SpriteRenderer Sprite Rect Vector2 Input Time Transform Resources Rigidbody2D ForceMode BoxCollider2D Vector3])
  )

(def regardless #(fn [& _] (%)))
(defmacro <> [o [f & xs]] `(let [o# ~o] (~f o# ~@xs) o#))   ; god dammit josh parker.  fuck your variable names.

(def reload-deps!
  (regardless (fn []
                (>log> "pressed reload")
                (use '[utils] :reload)
                (require '[arcadia.core :as a] :reload)
                (require '[state-system.core :as ss] :reload)
                (require '[game-state.core :as gs] :reload)
                (require '[arcadia.linear :as al] :reload))))

(defn make-sprite
  ([path] (make-sprite path path))
  ([path name]
   (let [sprite (Sprite/Create (Resources/Load path) (Rect. 0 0 32 32) (al/v2 0 0))]
     (do (set! (.name sprite) name)
         sprite))))

(defn into-state [entity key value]
  "returns the the value of the (a/state entity key) or stores and returns value"
  (or (a/state entity key) (val (a/set-state! entity key value))))

(defn advance-engine [helios event]
  (let [game-data (a/state helios :state)
        latest-state-data (ss/engine gs/game-engine
                                     game-data
                                     (conj (a/state helios :input)
                                           (vec (cons (current-time-millis)) event)))]
    (a/set-state! helios :input [])
    (a/set-state! helios :state latest-state-data)))

(def recipes
  {:player
   (fn [entities] (-> (GameObject.)
                      (<> (-> (a/cmpt+ Rigidbody2D)
                              (<> (-> .gravityScale (set! 0)))))
                      (<> (a/cmpt+ BoxCollider2D))
                      (<> (-> (a/cmpt+ SpriteRenderer)
                              .sprite
                              (set! (make-sprite "Sprites/Player"))))))})

(defn be-created! [recipes helios-entities object-recipe-key]
  (if-let [existing-entity (object-recipe-key helios-entities)]
    existing-entity
    ((object-recipe-key recipes) helios-entities)))

(defn build-world! [helios]
  (a/set-state! helios :state [{} [] [{}]])
  (a/set-state! helios :input [[(current-time-millis) :initialize-state]])
  (a/set-state! helios :entities {:player (be-created! recipes (a/state helios :entities) :player)})
  )
(def side-effect-map
  {:add-impulse
   (fn [helios unity-entity impulse-magnitude]
     (let [rb (into-state unity-entity :rigid Rigidbody2D)]
       (a/log "Adding Force: " impulse-magnitude "!!")
       (.AddForce rb impulse-magnitude ForceMode/VelocityChange)))})

(defn destroy-world! [helios]
  (doall (map #(a/destroy-immediate (val %)) (a/state helios :entities)))
  (a/set-state! helios :entities {})
  (a/set-state! helios :state nil))

(defn handle-input [helios]
  ((cond
     (pos? (Input/GetAxisRaw "Submit")) build-world!
     (pos? (Input/GetAxisRaw "Cancel")) destroy-world!
     (pos? (Input/GetAxisRaw "Reload")) reload-deps!
     :else (constantly nil)) helios))

(defn add-input [helios input-event]
  (a/set-state! helios :input (conj (a/state helios :input) (into [(current-time-millis)] input-event))))

(defn add-input-axes-to-game-state [helios]
  (add-input helios [:input :horizontal (Input/GetAxis "horizontal")])
  (add-input helios [:input :vertical (Input/GetAxis "vertical")]))

(def get-game-state #(-> % (a/state :state) first))

(defn match-position [helios]
  (let [state-player (:player (get-game-state helios))
        unity-player (:player (a/state helios :entities))
        rb (into-state unity-player :rigid (a/cmpt unity-player Rigidbody2D))
        [x y] (:position state-player)]
    ;(>log> state-player)
    (.MovePosition rb (al/v2 x y))
    ))
(defn perform-entity-side-effect [helios entity-key side-effects]
  (let [unity-entity (entity-key (a/state helios :entities))]
    (for [[side-effect-type & side-effect-args] side-effects]
      (apply (side-effect-map side-effect-type) unity-entity side-effect-args))))

(defn perform-side-effects [helios]
  (doall (map #(apply perform-entity-side-effect helios %) (-> helios get-game-state :side-effects)))
  (add-input helios [:effects-performed :all]))

(defn adjust-player [helios]
  (do (match-position helios)
      ;(perform-side-effects helios)
      ))

(defn update-helios [helios]
  (handle-input helios)
  (when (a/state helios :state)
    (add-input-axes-to-game-state helios)))

(defn fixed-update-helios [helios]
  (when (a/state helios :state)
    (advance-engine helios [:fixed-update Time/fixedDeltaTime])
    (adjust-player helios)))