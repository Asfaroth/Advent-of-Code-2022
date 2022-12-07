(ns advent-of-code-2022-day07.core)
(require '[clojure.string :as str])

;; Filestructure saved as:
;; {:dirName "/"
;;  :subDir ({:dirName "test" :subDir () :files ()})
;;  :files ({:name "test.txt" :size 42} {:name "test2.txt" :size 1337})}

;; Path saved as:
;; ("/" "some" "path")

(defn addDirectory
  "Adds a directory with a given name to the given path and returns the altered file structure."
  [dir path name]
  (if (empty? path)
    (assoc dir :subDir (conj (:subDir dir) {:dirName name :subDir (list) :files (list)}))
    (assoc dir :subDir (map #(if (= (:dirName %) (first path))
                               (addDirectory % (into [] (drop 1 path)) name)
                               %) (:subDir dir)))))

(defn addFile
  "Adds a file with a given name to the given path and returns the altered file structure."
  [dir path file]
  (if (empty? path)
    (assoc dir :files (conj (:files dir) file))
    (assoc dir :subDir (map #(if (= (:dirName %) (first path))
                               (addFile % (into [] (drop 1 path)) file)
                               %) (:subDir dir)))))

(defn interpreteLines
  "Interpretes the given input lines and returns the interpreted file structure."
  [files path input]
  (if (empty? input)
    files
    (let [currentLine (first input)]
      (if (= (first currentLine) "$")
        (if (= (second currentLine) "cd")
          (if (= (nth currentLine 2) "/")
            (recur files (vector) (drop 1 input))
            (if (= (nth currentLine 2) "..")
              (recur files (into [] (drop-last path)) (drop 1 input))
              (recur files (conj path (nth currentLine 2)) (drop 1 input))))
          (recur files path (drop 1 input)));; ls command
        (if (= (first currentLine) "dir")
          (recur (addDirectory
                  files
                  path
                  (second currentLine))
                 path (drop 1 input))
          (recur (addFile
                  files
                  path
                  {:name (second currentLine) :size (Integer/parseInt (first currentLine))})
                 path (drop 1 input)))))))

(defn calcSizes
  "Calculates the directory sizes for a given structure."
  [file]
  (let [subDirWithSize (map calcSizes (:subDir file))]
    (assoc file
           :size (+ (reduce #(+ %1 (:size %2)) 0 (:files file))
                    (reduce #(+ %1 (:size %2)) 0 subDirWithSize))
           :subDir subDirWithSize)))

(defn sumFirstDirs
  "Sums up all directories below a size of 100000."
  [file]
  (let [addAmount (if (<= (:size file) 100000) (:size file) 0)]
    (if (empty? (:subDir file))
      addAmount
      (+ addAmount
         (reduce + (map sumFirstDirs (:subDir file)))))))

(defn getDirSizesAboveThreshold
  "Filters out all directories bigger than a certain threshold and returns their sizes."
  [file threshold]
  (let [toAdd (if (<= threshold (:size file)) (list (:size file)) (list))]
    (if (empty? (:subDir file))
      toAdd
      (concat toAdd (flatten (map #(getDirSizesAboveThreshold % threshold) (:subDir file)))))))

(defn -main
  "Main function."
  []
  (let [input (map #(str/replace % #"\p{C}" "") (str/split-lines (slurp "../input.txt")))
        splitInput (map #(str/split % #" ") input)
        files (calcSizes (interpreteLines {:dirName "/" :subDir (list) :files (list)} (vector) splitInput))]
    (println "Sum of Directories under 100k: " (sumFirstDirs files))
    (println "Smallest Directory to delete:  " (apply min (getDirSizesAboveThreshold files (- 30000000 (- 70000000 (:size files))))))))
