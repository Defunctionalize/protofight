 (ns utils)

(def current-time-millis #(long (.TotalMilliseconds (.Subtract (System.DateTime/UtcNow) (System.DateTime. 1970 1 1 0 0 0 System.DateTimeKind/Utc)))))
(def >log> #(do (spit "arcadia-log.log" (str (System.DateTime/Now) ": " % "\n") :file-mode System.IO.FileMode/Append) %))
