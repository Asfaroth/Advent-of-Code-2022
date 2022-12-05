(ns advent-of-code-2022-day05.core)
(require '[clojure.string :as str])

(defn -main
  "Main function."
  []
  (let [partedInput (str/split (slurp "../input.txt") #"\n\n")
        movements (map #(let [splitLine (str/split % #" ")]
                          {:from (- (Integer/parseInt (nth splitLine 3)) 1)
                           :to (- (Integer/parseInt (nth splitLine 5)) 1)
                           :amount (Integer/parseInt (nth splitLine 1))})
                       (str/split-lines (second partedInput)))
        splitContainerInput (str/split-lines (first partedInput))
        containers (map #(filter (fn [place] (not= (int place) (int \ )))
                                 (map (fn [row] (get row %)) (drop-last splitContainerInput)))
                        (map #(str/index-of (last splitContainerInput) (str/replace % #" " "")) (str/split (last splitContainerInput) #"   ")))
        firstResult (reduce #(map-indexed (fn [index stack] (if (= index (:from %2))
                                                              (drop (:amount %2) stack)
                                                              (if (= index (:to %2))
                                                                (flatten (conj stack (reverse (take (:amount %2) (nth %1 (:from %2))))))
                                                                stack))) %1) containers movements)
        secondResult (reduce #(map-indexed (fn [index stack] (if (= index (:from %2))
                                                               (drop (:amount %2) stack)
                                                               (if (= index (:to %2))
                                                                 (flatten (conj stack (take (:amount %2) (nth %1 (:from %2)))))
                                                                 stack))) %1) containers movements)]
    (println "1st Top Containers: " (reduce #(str %1 (first %2)) "" firstResult))
    (println "2nd Top Containers: " (reduce #(str %1 (first %2)) "" secondResult))))
