(ns advent-of-code-2022-day15.core)
(require '[clojure.string :as str])

(defn getDistance
  ""
  [p1 p2]
  (+ (Math/abs (- (first p1) (first p2)))
     (Math/abs (- (second p1) (second p2)))))

(defn interpreteData
  ""
  [sensorData]
  {:s (first sensorData)
   :b (second sensorData)
   :r (getDistance (first sensorData) (second sensorData))})

(defn findBlockedSpots
  ""
  [row sensorData]
  (let [concernedSensors (filter #(or (<= (second (:s %)) row (+ (second (:s %)) (:r %)))
                                      (<= (- (second (:s %)) (:r %)) row (second (:s %))))
                                 sensorData)
        searchedRow (map #(let [width (inc (Math/abs (- (* 2 (:r %))
                                                        (* 2 (Math/abs (- row (second (:s %))))))))]
                            (if (= width 1)
                              (list (first (:s %)))
                              (range (- (first (:s %)) (quot width 2)) (inc (+ (first (:s %)) (quot width 2)))))) concernedSensors)]
    (sort < (distinct (flatten searchedRow)))))

(defn getDiagonalLine
  ""
  [x1 y1 x2 y2]
  (if (< x1 x2)
    (if (< y1 y2)
      ;; bottom to right
      (map list (range x1 (inc x2)) (range y1 (inc y2)))
      ;; top to right
      (map list (range x1 (inc x2)) (reverse (range y2 (inc y1)))))
    (if (< y1 y2)
      ;; bottom to left
      (map list (reverse (range x2 (inc x1))) (range y1 (inc y2)))
      ;; top to left
      (map list (reverse (range x2 (inc x1))) (reverse (range y2 (inc y1)))))))

(defn checkPointIsInDiamond
  ""
  [point sensorData]
  (cond (empty? sensorData) true
        (<= (getDistance (:s (first sensorData)) point)
            (:r (first sensorData))) false
        :else (recur point (drop 1 sensorData))))

(defn checkPoint
  ""
  [point sensorData minX maxX minY maxY]
  (and (<= minX (first point) maxX)
       (<= minY (second point) maxY)
       (checkPointIsInDiamond point sensorData)))

(defn getPerimeters
  ""
  [currentSensor]
  (let [topRight (future (getDiagonalLine (first (:s currentSensor))
                                          (+ (second (:s currentSensor)) (:r currentSensor) 1)
                                          (+ (first (:s currentSensor)) (:r currentSensor) 1)
                                          (second (:s currentSensor))))
        rightBottom (future (getDiagonalLine (+ (first (:s currentSensor)) (:r currentSensor) 1)
                                             (second (:s currentSensor))
                                             (first (:s currentSensor))
                                             (- (second (:s currentSensor)) (:r currentSensor) 1)))
        bottomLeft (future (getDiagonalLine (first (:s currentSensor))
                                            (- (second (:s currentSensor)) (:r currentSensor) 1)
                                            (- (first (:s currentSensor)) (:r currentSensor) 1)
                                            (second (:s currentSensor))))
        leftTop (future (getDiagonalLine (- (first (:s currentSensor)) (:r currentSensor) 1)
                                         (+ (second (:s currentSensor)) (:r currentSensor) 1)
                                         (first (:s currentSensor))
                                         (+ (second (:s currentSensor)) (:r currentSensor) 1)))]
    (concat (deref topRight)
            (deref rightBottom)
            (deref bottomLeft)
            (deref leftTop))))

(defn -main
  "Main function."
  []
  (let [input (map
               #(map (fn [rawSides]
                       (map (fn [splitSides] (Integer/parseInt (str/replace splitSides #"Sensor at x=|closest beacon is at x=|y=" "")))
                            (str/split rawSides #", ")))
                     (str/split % #": "))
               (str/split-lines (slurp "../input.txt")))
        interpretedData (map interpreteData input)
        blockedSpacesAtRow (future (findBlockedSpots 2000000 interpretedData))
        beaconsAtRow (map second (distinct (map #(:b %) (filter #(= (second (:b %)) 2000000) interpretedData))))
        hole (first (distinct (remove nil? (pmap #(first (filter (fn [point]
                                                                   (checkPoint point interpretedData 0 4000000 0 4000000))
                                                                 (distinct (getPerimeters %))))
                                                 interpretedData))))]
    (println "# of blocked spots at y=2.000.000:          " (count (remove (set beaconsAtRow) (deref blockedSpacesAtRow))))
    (println "Tuning Frequency (this may take some time): " (+ (* (first hole) 4000000) (second hole)))
    (shutdown-agents)))
