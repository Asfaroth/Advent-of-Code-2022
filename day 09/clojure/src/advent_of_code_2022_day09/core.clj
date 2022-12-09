(ns advent-of-code-2022-day09.core)
(require '[clojure.string :as str])

(defn calcPositions
  "Calculates the rope's new positions according to the exercise requirements."
  [positions newH]
  (let [oldT (first positions)
        newT (if (and (<= (dec (:x newH)) (:x (first positions)) (inc (:x newH)))
                      (<= (dec (:y newH)) (:y (first positions)) (inc (:y newH))))
               oldT
               (cond (= (:x newH) (:x oldT)) (if (< (:y newH) (:y oldT))
                                               {:x (:x oldT) :y (dec (:y oldT))} {:x (:x oldT) :y (inc (:y oldT))})
                     (= (:y newH) (:y oldT)) (if (< (:x newH) (:x oldT))
                                               {:x (dec (:x oldT)) :y (:y oldT)} {:x (inc (:x oldT)) :y (:y oldT)})
                     (< (:x newH) (:x oldT)) (if (< (:y newH) (:y oldT))
                                               {:x (dec (:x oldT)) :y (dec (:y oldT))} {:x (dec (:x oldT)) :y (inc (:y oldT))})
                     :else (if (< (:y newH) (:y oldT))
                             {:x (inc (:x oldT)) :y (dec (:y oldT))} {:x (inc (:x oldT)) :y (inc (:y oldT))})))]
    (if (= (count positions) 1)
      (list newH newT)
      (conj (calcPositions (drop 1 positions) newT) newH))))

(defn step
  "Simulates one step in the given direction. If the amount drop to 0, a new direction is chosen from the remainingSteps list."
  [direction amount remainingSteps positions]
  (if (= amount 0)
    (if (empty? remainingSteps)
      positions
      (recur (first (first remainingSteps)) (Integer/parseInt (second (first remainingSteps))) (drop 1 remainingSteps) positions))
    (let [currentH (first (first positions))
          newH (case direction
                 "U" {:x (:x currentH) :y (inc (:y currentH))}
                 "D" {:x (:x currentH) :y (dec (:y currentH))}
                 "R" {:x (inc (:x currentH)) :y (:y currentH)}
                 "L" {:x (dec (:x currentH)) :y (:y currentH)}
                 currentH)]
      (recur direction (dec amount) remainingSteps (conj positions (calcPositions (drop 1 (first positions)) newH))))))

(defn -main
  "Main function."
  []
  (let [input (map #(str/split (str/replace % #"\p{C}" "") #" ") (str/split-lines (slurp "../input.txt")))
        positionsTwoLinks (step (first (first input)) (Integer/parseInt (second (first input))) (drop 1 input) (list (repeat 2 {:x 0 :y 0})))
        positionsTenLinks (step (first (first input)) (Integer/parseInt (second (first input))) (drop 1 input) (list (repeat 10 {:x 0 :y 0})))]
    (println "Visited tiles (02): " (count (distinct (map #(last %) positionsTwoLinks))))
    (println "Visited tiles (10): " (count (distinct (map #(last %) positionsTenLinks))))))
