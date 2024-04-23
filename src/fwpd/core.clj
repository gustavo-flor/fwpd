(ns fwpd.core)

(def filename "suspects.csv")

(def vamp-keys [:name :glitter-index])

(defn str->int
  [str]
  (Integer/parseInt str))

(def conversions {:name identity
                  :glitter-index str->int})

(defn convert
  [vamp-key value]
  ((get conversions vamp-key) value))

(defn parse
  "Convert a CSV into rows of columns"
  [str]
  (map #(clojure.string/split % #",")
       (clojure.string/split str #"\n")))

(defn mapify
  "Return a seq of maps like {:name \"Edward Cullen\" :glitter-index 10}"
  [rows]
  (map (fn [unmapped-row]
         (reduce (fn [row-map [vamp-key value]]
                   (assoc row-map vamp-key (convert vamp-key value)))
                 {}
                 (map vector vamp-keys unmapped-row)))
       rows))

(defn glitter-filter
  [records minimum-glitter]
  (filter #(>= (:glitter-index %) minimum-glitter) records))

(defn get-vamp-names
  [vamps]
  (map #(:name %) vamps))

(defn is-valid-glitter-index?
  [value]
  (and ((complement nil?) value) (integer? value) (>= value 0) (<= value 10)))

(def not-empty? (complement empty?))

(def validations {:name not-empty?
                  :glitter-index is-valid-glitter-index?})

(defn is-valid?
  [vamp-key value]
  ((get validations vamp-key) value))

(defn is-valid-vamp?
  [vamp]
  (and (is-valid? :name (:name vamp))
       (is-valid? :glitter-index (:glitter-index vamp))))

(defn append
  [vamps vamp]
  (if (is-valid-vamp? vamp)
    (conj vamps vamp)
    (identity vamps)))

(defn vamp-keys-to-string
  [vamp]
  (str (:name vamp) "," (:glitter-index vamp)))

(defn write-vamps-csv
  [vamps]
  (spit "vamps.csv" (clojure.string/join "\n" (map vamp-keys-to-string vamps))))

(defn -main
  "I don't do a lot ... yet."
  [& _]
  (let [vamps (-> (slurp filename)
                  parse
                  mapify
                  (append {:name "Gustavo Flôr" :glitter-index 0})
                  (glitter-filter 3))]
    (write-vamps-csv vamps)
    (doseq [name (get-vamp-names vamps)]
      (println name))))
