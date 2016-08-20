 (ns utils
   (:require [arcadia.core :as a]))

(def current-time-millis #(long (.TotalMilliseconds (.Subtract (System.DateTime/UtcNow) (System.DateTime. 1970 1 1 0 0 0 System.DateTimeKind/Utc)))))
(def >log> #(do (spit "arcadia-log.log" (str (System.DateTime/Now) ": " % "\n") :file-mode System.IO.FileMode/Append) %))
(def >logl> #(do (spit "arcadia-log.log" (str (System.DateTime/Now) ": " %2 " " %1 "\n") :file-mode System.IO.FileMode/Append) %1))
(def >>logl> #(do (spit "arcadia-log.log" (str (System.DateTime/Now) ": " %1 " " %2 "\n") :file-mode System.IO.FileMode/Append) %2))
(def regardless #(fn [& _] (%)))
(defmacro let->> [binding body value] `(let [~binding ~value] ~body))
(defmacro <!> [o [f & xs]] `(let [o# ~o] (~f o# ~@xs) o#))