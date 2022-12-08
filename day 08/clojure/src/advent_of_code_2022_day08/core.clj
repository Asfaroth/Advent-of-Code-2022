(ns advent-of-code-2022-day08.core)
(require '[clojure.string :as str])

(defn treeInVisible?
  "Checks whether a tree is invisible for a given line of sight."
  [line height]
  (if (empty? line)
    false
    (if (<= height (first line))
      true
      (recur (drop 1 line) height))))

(defn getScencicScoreLine
  "Calculates the scenic score for a given line of sight."
  ([line height] (getScencicScoreLine line height 1))
  ([line height value]
   ;;(println line height value)
   (if (empty? line)
     (dec value)
     (if (>= (first line) height)
       value
       (recur (drop 1 line) height (inc value))))))

(defn -main
  "Main function."
  []
  (let [input (map #(str/split (str/replace % #"\p{C}" "") #"") (str/split-lines (slurp "../input.txt")))
        matrix (map #(map (fn [char] (Integer/parseInt char)) %) input)
        visibleTrees (map-indexed (fn [y row]
                                    (map-indexed (fn [x col]
                                                   (if (or (= x 0) (= y 0) (= x (dec (count row))) (= y (dec (count matrix)))
                                                           (not (treeInVisible? (take x row) col))
                                                           (not (treeInVisible? (drop (inc x) row) col))
                                                           (not (treeInVisible? (take y (map #(nth % x) matrix)) col))
                                                           (not (treeInVisible? (drop (inc y) (map #(nth % x) matrix)) col)))
                                                     1
                                                     0))
                                                 row))
                                  matrix)
        scenicScores (map-indexed (fn [y row] (map-indexed (fn [x col] (if (or (= x 0) (= y 0) (= x (dec (count row))) (= y (dec (count matrix))))
                                                                         0
                                                                         (* (getScencicScoreLine (reverse (take x row)) col)
                                                                            (getScencicScoreLine (drop (inc x) row) col)
                                                                            (getScencicScoreLine (reverse (take y (map #(nth % x) matrix))) col)
                                                                            (getScencicScoreLine (drop (inc y) (map #(nth % x) matrix)) col)))) row)) matrix)]
    (println "# of visible trees: " (reduce #(+ (reduce + %2) %1) 0 visibleTrees))
    (println "Max scenic score:   " (apply max (map #(apply max %) scenicScores)))))
