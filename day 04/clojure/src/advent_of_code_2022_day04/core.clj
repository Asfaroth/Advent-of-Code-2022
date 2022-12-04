(ns advent-of-code-2022-day04.core)
(require '[clojure.string :as str])

(defn -main
  "Main function."
  []
  (let [rawText (slurp "../input.txt")
        splitLines (map #(map (fn [range] (str/split range #"-")) %) (map #(str/split % #",") (map (fn [x] (str/replace x #"\p{C}" "")) (str/split-lines rawText))))
        parsedInput (map #(map (fn [range] (map (fn [number] (Integer/parseInt number)) range)) %) splitLines)]
    (println "Contained: " (reduce #(if (or (<= (first (first %2)) (first (second %2)) (second (second %2)) (second (first %2)))
                                            (<= (first (second %2)) (first (first %2)) (second (first %2)) (second (second %2))))
                                      (inc %1) %1) 0 parsedInput))
    (println "Overlaped: " (reduce #(if (or (<= (first (first %2)) (first (second %2)) (second (first %2)))
                                            (<= (first (first %2)) (second (second %2)) (second (first %2)))
                                            (<= (first (second %2)) (first (first %2)) (second (second %2)))
                                            (<= (first (second %2)) (second (first %2)) (second (second %2))))
                                      (inc %1) %1) 0 parsedInput))))
