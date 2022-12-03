(ns advent-of-code-2022-day03.core)
(require '[clojure.string :as str])

(defn getDuplicates
  "Gets duplicates out of two given lists and returns them in a sequence."
  [l1 l2]
  (let [countedChars (frequencies l2)]
    (distinct (filter (fn [char] (and (contains? countedChars char) (< 0 (get countedChars char)))) l1))))

(defn getValues
  "Calculates the challenge values for the given metric."
  [list]
  (map (fn [char] (let [asciiValue (int char)] (if (< 90 asciiValue) (- asciiValue 96) (- asciiValue 38)))) list))

(defn -main
  "Main function."
  []
  (let [rawText (slurp "../input.txt")
        splitLines (map #(split-at (/ (count %) 2) %) (map (fn [x] (str/replace x #"\p{C}" "")) (str/split-lines rawText)))
        duplicateValues (getValues (flatten (map #(distinct (getDuplicates (first %) (second %))) splitLines)))
        groupedLines (map #(map (fn [x] (str/replace x #"\p{C}" "")) %) (map #(str/split-lines %) (re-seq #"(?:[a-zA-Z]+\n){3}" rawText)))
        priorityValues (getValues (flatten (map #(getDuplicates (first %) (getDuplicates (second %) (nth % 2))) groupedLines)))]
    (println "Duplicates: " (reduce + duplicateValues))
    (println "Priorities: " (reduce + priorityValues))))
