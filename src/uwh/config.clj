(ns uwh.config
  "Library for resolving and formatting config. A config file is identified by a URL and resolves into a clojure resource"
  (:import [java.net URI URL])
  (:require [clojure.data.json :as json]
            [clojure.java.io :as io]
            [clojure.edn :as edn]
            [clojure.string :as s]
            [clojure.walk :as walk]))

;; Bijections are relative to Clojure EDN
;; TODO how to deal with aspects that are not truly bijective
;; but there is limited back and forth conversion, so ... what to call them
(defprotocol Bijection
  (inject [self value])
  (surject [self value]))

(defn bijection [inject-fn surject-fn]
  (reify Bijection
    (inject [_ v] (inject-fn v))
    (surject [_ v] (surject-fn v))))

(def identity-bijection (bijection identity identity))

(defn json-bijection []
  (bijection json/write-str #(json/read-str % :key-fn keyword)))

(defn- name++ [v]
  (if (instance? clojure.lang.Named v) (name v) (str v)))

(defn- array-like-map? [form]
  (let [ks (sort (keys form))]
    (= ks (take (count ks) (map str (range))))))

;; same as clojure assoc-in but handles the case where one of the elements on the 
;; path is not associative by dropping the nested element
(defn- safe-assoc-in
  [m [k & ks] v]
  (if (or (nil? m) (instance? clojure.lang.Associative m))
    (if ks
      (assoc m k (safe-assoc-in (get m k) ks v))
      (assoc m k v))
    m))

(defn properties-bijection
  [& {:keys [separator key-bijection]
      :or {separator "." key-bijection identity-bijection}}]
  (let [sep-regex (re-pattern (java.util.regex.Pattern/quote separator))]
    (reify
      Bijection
      (inject [self v]
        (cond
         (map? v)
         (into {}
               (mapcat
                (fn [[k v]]
                  (let [k (name++ k)
                        v (inject self v)]
                    (if (map? v)
                      (map (fn [[k# v#]] [(str k separator k#) v#]) v)
                      [[k v]])))
                v))

         (sequential? v) (inject self (zipmap (range) v))

         :else (str v)))
      
      (surject [_ v]
        (->> v
             (map (fn [[k v]] [(s/split k sep-regex) v]))
             (remove #(or (nil? (seq (first %))) (some s/blank? (first %))))
             (reduce (fn [res [k v]] (safe-assoc-in res (map #(surject key-bijection %) k) v)) {})
             (walk/postwalk
              (fn [form]
                (if (and (map? form) (array-like-map? form))
                  (vec (map val (sort-by #(Long/parseLong (key %)) form)))
                  form)))
             (walk/keywordize-keys))))))



(defprotocol URIConversions
  (as-uri ^URI [self]))

(extend-protocol URIConversions
  String       (as-uri [s] (URI. s))
  URL          (as-uri [u] (.toURI u))
  java.io.File (as-uri [f] (.toURI f))
  URI          (as-uri [u] u))



(declare retrieve)

(defn merge-recursive [& maps]
  (apply merge-with 
         (fn [& vs] (if (every? map? vs) (apply merge-recursive vs) (last vs)))
         maps))

(defn- resolve-config-function [tag f data]
  (walk/postwalk
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

(defn- resolve-references [^URI parent-uri data]
  (resolve-config-function ::reference #(retrieve (.resolve parent-uri (as-uri %))) data))

(defn- resolve-merges [data]
  (resolve-config-function ::merge #(apply merge-recursive %&) data))

(defn post-process [uri data]
  (->> data
       (resolve-references uri)
       (resolve-local-references [])
       (resolve-merges)))


(def system-property-uri (URI. "system:properties"))
(def system-env-uri (URI. "system:env"))

(def system-property-config 
  (surject (properties-bijection) (into {} (System/getProperties))))

(def system-env-config
  (->> (System/getenv)
       (into {})
       (surject (properties-bijection
                 :separator "_"
                 :key-bijection
                 (bijection 
                  (comp s/upper-case name)
                  (comp keyword s/lower-case))))))


(defn- retrieve-config [uri]
  (cond
   (= uri system-property-uri) system-property-config
   (= uri system-env-uri) system-env-config
   :else (-> uri io/as-url slurp edn/read-string)))

(defn retrieve [input]
  (let [uri (as-uri input)]
    (->> uri retrieve-config (post-process uri))))
