(ns advent-of-code-2022-day14.core)
(require '[clojure.string :as str])

(defn drawLinesBetweenPoints
  "Creates the coordinates for rocks on a given line definition."
  [points rocks]
  (if (= (count points) 1)
    rocks
    (let [x1 (first (first points))
          x2 (first (second points))
          y1 (second (first points))
          y2 (second (second points))]
      (if (= y1 y2)
        ;; horizontal line
        (recur (drop 1 points)
               (assoc rocks y1 (reduce-kv #(assoc %1 (if (<= x1 x2)
                                                       (+ x1 %2)
                                                       (- x1 %2)) %3) (get rocks y1) (into [] (repeat (inc (Math/abs (- x1 x2))) "#")))))
        ;; vertical line
        (recur (drop 1 points)
               (reduce-kv #(let [currentRow (if (< y1 y2)
                                              (+ y1 %2)
                                              (- y1 %2))]
                             (assoc %1 currentRow (assoc (get rocks currentRow) x1 %3))) rocks (into [] (repeat (inc (Math/abs (- y1 y2))) "#"))))))))

(defn simulateSand
  "Simulates the dropping of sand until sand falls off the cliif or the source is blocked by already fallen sand."
  ([cave source bottomFull?] (simulateSand cave source bottomFull? 0 source 0))
  ([cave source bottomFull? sandAmount newX newY]
   (cond (= newX 0) (recur (mapv #(into ["."] %) cave) (inc source) bottomFull? sandAmount (inc newX) newY)
         (= newX (dec (count (first cave)))) (recur (mapv #(conj % ".") cave) source bottomFull? sandAmount newX newY)
         (< -1 newY (dec (count cave))) (let [relevantRow (get cave (inc newY))]
                                          (if (= (get relevantRow newX) ".")
                                            (recur cave source bottomFull? sandAmount newX (inc newY))
                                            (cond (= (get relevantRow (dec newX)) ".") (recur cave source bottomFull? sandAmount (dec newX) (inc newY))
                                                  (= (get relevantRow (inc newX)) ".") (recur cave source bottomFull? sandAmount (inc newX) (inc newY))
                                                  (and (= newY 0) (= newX source)) (do (run! println cave) (inc sandAmount))
                                                  :else (recur (assoc cave newY (assoc (nth cave newY) newX "o")) source bottomFull? (inc sandAmount) source 0))))
         bottomFull? (recur (assoc cave newY (assoc (nth cave newY) newX "o")) source bottomFull? (inc sandAmount) source 0)
         :else (do (run! println cave) sandAmount))))

(defn -main
  "Main function."
  []
  (let [input  (map #(map (fn [coords] (map (fn [coord] (Integer/parseInt coord)) (str/split coords #","))) (str/split % #" -> "))
                    (str/split-lines (slurp "../input.txt")))
        rocks (reduce #(drawLinesBetweenPoints %2 %1) (sorted-map) input)
        rows (inc (apply max (keys rocks)))
        minCol (apply min (map #(apply min (keys (val %))) rocks))
        maxCol (apply max (map #(apply max (keys (val %))) rocks))
        cave (reduce-kv (fn [outerCave row rowValues]
                          (assoc outerCave row (reduce-kv (fn [innerRow col val] (assoc innerRow (inc (- col minCol)) val))
                                                          (nth outerCave row) rowValues)))
                        (into [] (repeat (inc rows) (into [] (repeat (+ (- maxCol minCol) 3) ".")))) rocks)]
    (println "Sand needed without floor: " (simulateSand cave (- 501 minCol) false))
    (println "Sand needed with floor:    " (simulateSand cave (- 501 minCol) true))))
