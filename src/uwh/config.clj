(ns uwh.config
  "Library for resolving and formatting config. A config file is identified by a URL and resolves into a clojure resource"
  (:import [java.net URI URL])
  (:use [clojure.java.io :only [as-url]]
        [clojure.walk :only [postwalk]]
        [clojure.string :only [split]]))

(declare retrieve)

(defprotocol URIConversions
  (as-uri [self]))

(extend-protocol URIConversions
  String
  (as-uri [s] (URI. s))

  URL
  (as-uri [u] (.toURI u))

  java.io.File
  (as-uri [f] (.toURI f))

  URI
  (as-uri [u] u))


(defn merge-recursive [& maps]
  (apply merge-with 
         (fn [& vs] (if (every? map? vs) (apply merge-recursive vs) (last vs)))
         maps))

(defn- resolve-config-function [tag f data]
  (postwalk
   (fn [e]
     (if (and (list? e) (= (first e) tag))
       (apply f (rest e))
       e))
   data))

(defn- resolve-local-references 
  ([paths data] (resolve-local-references paths data data))
  ([paths full-data data]
     (resolve-config-function 
      ::local-ref
      (fn [path] 
        (let [replacement (get-in full-data path)]
          (when (contains? (set paths) path)
            (throw (Exception. (str "Cyclical local references: " paths))))
          (resolve-local-references (conj paths path) full-data replacement)))
      data)))

(defn- resolve-references [parent-uri data]
  (resolve-config-function ::reference #(retrieve (.resolve parent-uri (as-uri %))) data))

(defn- resolve-merges [data]
  (resolve-config-function ::merge #(apply merge-recursive %&) data))

(defn post-process [uri data]
  (->> data
       (resolve-references uri)
       (resolve-local-references [])
       (resolve-merges)))


;; same as clojure assoc-in but handles the case where one of the elements on the 
;; path is not associative by dropping the nested element
(defn- safe-assoc-in
  [m [k & ks] v]
  (if (or (nil? m) (instance? clojure.lang.Associative m))
    (if ks
      (assoc m k (safe-assoc-in (get m k) ks v))
      (assoc m k v))
    m))

(def system-property-uri (URI. "system:properties"))
(def system-property-config 
  (->> (System/getProperties)
       (into {})
       (map (fn [[k v]]
              [(vec (map keyword (split k #"\."))) v]))
       ;; use safe-assoc-in because some paths like file.encoding and file.encoding.pkg clash ...
       (reduce (fn [res [path v]] (safe-assoc-in res path v)) {})))

(defn- read-clj [s]
  (binding [*read-eval* false] (read-string s)))

(defn- retrieve-config [uri]
  (if (= uri system-property-uri)
    system-property-config
    (-> uri as-url slurp read-clj)))

(defn retrieve [input]
  (let [uri (as-uri input)]
    (->> uri retrieve-config (post-process uri))))

