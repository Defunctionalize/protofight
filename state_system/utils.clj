(ns state-system.utils)

(defn iterate-until [pred fn data]
  (first (drop-while (complement pred) (iterate fn data))))

(defn lazy-seq-assoc [lazy n val]
  (concat (take n lazy) [val] (drop (inc n) lazy)))

(defn idx->coord [width index]
  [(mod index width) (quot index width)])
(defn coord->idx [width x y]
  (-> y (* width) (+ x)))

(defn bisect-search-area [key-fn compare-fn target coll too-low too-high]
  (let [middle (-> coll count (quot 2))
        middle-value (key-fn (nth coll middle))]
    (if (compare-fn middle-value target)
      [(subvec coll (inc middle)) (concat too-low (subvec coll 0 (inc middle))) too-high]
      [(subvec coll 0 middle) too-low (concat (subvec coll middle) too-high)])))

(defn binary-split [coll target & {:keys [key-fn compare-fn] :or {key-fn identity compare-fn <}}]
  (->> [coll [] []]
       (iterate #(apply (partial bisect-search-area key-fn compare-fn target) %))
       (drop-while #(-> % first empty? not)) first rest))

(defn binary-insert [coll val  & {:keys [key-fn compare-fn subvec?] :or {key-fn identity compare-fn < subvec? false}}]
  (let [anchor-getter (if subvec? first identity)
        insertion-wrapper (if subvec? identity #(vector %))
        [before after] (binary-split coll (anchor-getter val) :key-fn key-fn :compare-fn compare-fn)]
    (concat before (insertion-wrapper val) after)))