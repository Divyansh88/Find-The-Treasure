(def map_string (slurp "map.txt"))
(def data (clojure.string/split-lines map_string))

(def rows (count data))
(def columns (count (str (get-in data [0]))))
(def starting_point (str (get-in data [0 0])))

(def data1 (-> (mapv vec (clojure.string/split-lines (slurp "map.txt")))) )

(doseq [n data]
  (cond
    (not= (count n) columns) (do
                               (println "Oops, something wrong with the map ;-)")
                               (System/exit 0)
                              ))
  )

;(println starting_point rows columns)
;(println data)

(defn checkBoundary [r c]
  (if (or (and (< r rows) (< c columns)) (and (>= r 0) (>= c 0)))
    "true"
    "false")
  )

(defn checkPath [map_data1 r c]
  (def map_data map_data1)
  (cond
    (or (= (str (get-in map_data [r c])) "+") (= (str (get-in map_data [r c])) "!"))  "false"
    (= (str (get-in map_data [r c])) "@") "true"
    (= (str (get-in map_data [r c])) "#") "false"
    (and (= (checkBoundary r c) "true") (= (str (get-in map_data [r c])) "-")) (do
                                                                                 ;(println (get-in map_data [r c]) r c)
                                                                                 ;assign value "+" to the map
                                                                                 (def  updated_map_data(-> map_data (assoc-in [r c] \+)))
                                                                                 ;(println updated_map_data)

                                                                                 (cond
                                                                                   (= (checkPath updated_map_data (inc r) c) "true") "true"
                                                                                   (= (checkPath updated_map_data r (inc c)) "true") "true"
                                                                                   (= (checkPath updated_map_data (dec r) c) "true") "true"
                                                                                   (= (checkPath updated_map_data r (dec c)) "true") "true"

                                                                                   :else (do
                                                                                           ;assign value "!" to the map
                                                                                           (def  updated_map_data(-> updated_map_data (assoc-in [r c] \!)))
                                                                                           ;(= (checkPath updated_map_data r c) "true") "true"
                                                                                           ;(println updated_map_data)
                                                                                           ;(println (get-in map_data [r c]) r c)

                                                                                           "false"
                                                                                           )
                                                                                   )
                                                                                 )

    :else "error")
  )

(defn printMsg
  [value]
  (println map_string)
  (println "")


  (cond
    (= value 1) (do
                  (def print_map (clojure.string/join "\n" map_data))
                  (def print_map (clojure.string/replace print_map " " ""))
                  (def print_map (clojure.string/replace print_map "\\" ""))
                  (def print_map (clojure.string/replace print_map "[" ""))
                  (def print_map (clojure.string/replace print_map "]" ""))
                  (println "Woo hoo, I found the treasure :-)")
                  (println "")
                  (println print_map)
                  )
    (= value 2) (do
                  (def print_map (clojure.string/join "\n" updated_map_data))
                  (def print_map (clojure.string/replace print_map " " ""))
                  (def print_map (clojure.string/replace print_map "\\" ""))
                  (def print_map (clojure.string/replace print_map "[" ""))
                  (def print_map (clojure.string/replace print_map "]" ""))
                  (println "Uh oh, I could not find the treasure :-(")
                  (println "")
                  (println print_map)
                  )
    )
  )



;(println (checkPath data1 0 0))                                         ;returns "true" if path exist
(cond
  (= (checkPath data1 0 0) "true") (printMsg 1)
  (= (checkPath data1 0 0) "false") (printMsg 2)
  (= (checkPath data1 0 0) "error") (println "Oops, something wrong with the map ;-)")
  )
;(if (= (checkPath data1 0 0) "true")
;  (printMsg 1)
;  (printMsg 2)
;  )


