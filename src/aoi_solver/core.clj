(ns aoi-solver.core
  (:gen-class)
  (require [clojure.math.combinatorics :as combinatorics]))

;; Data
;; ({ :percentage-of-users INT :vis-seq [] }...)

;; Result
;; ({ :sub-seq [] :occurrences INT :weight INT }...)


(def visitation-sequences [
                           {:percentage-of-users 30  :vis-seq [4 1 1 1 2 4 7]}
                           {:percentage-of-users 15  :vis-seq [6 1 6 6 6 6]}
                           {:percentage-of-users 15  :vis-seq [5 8 1 5 7]}
                           {:percentage-of-users 10  :vis-seq [5 8 1 5 7]}
                           {:percentage-of-users 10  :vis-seq [4 1 2 3 4]}
                           {:percentage-of-users 5  :vis-seq [4 1 2 1]}
                           {:percentage-of-users 5  :vis-seq [5 6 5 6 4 1 5]}
                           {:percentage-of-users 3  :vis-seq [1 2 1 5 6 1]}
                           {:percentage-of-users 3  :vis-seq [4 1 2 5 6 5 6]}
                           {:percentage-of-users 3  :vis-seq [3 5 6 1 5 5]}
                           {:percentage-of-users 1  :vis-seq [3 5 6 1 5 5]}])

(def number-specific-results {:1 0 :2 0 :3 0 :4 0 :5 0 :6 0 :7 0 :8 0 :9 0})

(defn calculate-weight
  ;; Calculate weight for subset. Now only percentage value, in future maybe more
  ;; sophisticated calculations
  ;; POU = Percentage of users
  [subset pou]
  pou)

(defn update-result-if-exists
  ;; Update result if subset exists
  [result-sub sub-set pou]
  (if (= (:sub-seq result-sub) sub-set)
    (assoc result-sub
      :occurrences (inc (:occurrences result-sub))
      :weight (+ (:weight result-sub) (calculate-weight sub-set pou)))
    result-sub))

(defn handle-subset
  ;; Handle single subset of sequence
  [subset pou result]
  (let [new-result (map #(update-result-if-exists % subset pou) result)]
    (if (= new-result result)
      (conj result (assoc {} :sub-seq subset :weight pou :occurrences 1))
      new-result)))

(defn calculate-propability-for-number-helper
  ;; Iterate subset
  [number number-weight result]
  (let [keyw (keyword (str number))]
    (assoc result keyw (+ (keyw result) number-weight))))

(defn calculate-propability-for-number
  ;; Number-specific weight
  [subset result]
  (reduce #(calculate-propability-for-number-helper %2 (:weight subset) %1) result (:sub-seq subset)))


(defn calculate-aoi-for-site
  ;; Iterate all possible subsets and calculate weight
  [data result]
  (->> (:vis-seq data)
    (combinatorics/subsets)
    (filter #(> (count %) 1))
    (reduce #(handle-subset %2 (:percentage-of-users data) %1) result)
    (flatten)))


(defn -main
  [& args]
  (->> visitation-sequences
      (reduce #(calculate-aoi-for-site %2 %1) '())
      (reduce #(calculate-propability-for-number %2 %1) number-specific-results)
      (sort-by second)
      (reverse)
      (println)))




