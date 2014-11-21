(ns uwh.config-test
  (:require [uwh.config :as cfg]
            [clojure.data.json :as json])
  (:use [clojure.test :only [deftest is testing]]
        [clojure.java.io :only [file]]))

;; << Conversions >>

(deftest test-json-conversion
  (let [sut (cfg/json-bijection)]
    (is (= (json/json-str {:a [1 "string"]})
           (cfg/inject sut {:a [1 "string"]})))
    (is (= {:a [1 "string"]}
           (cfg/surject sut (json/json-str {:a [1 "string"]}))))
    (is (= {:a 1}
           (cfg/surject sut (json/json-str {"a" 1}))))))

(deftest test-properties-conversion
  (let [sut (cfg/properties-bijection)]
    (is (= {"a" "1"
            "b.0" "a" "b.1" "b"
            "c.key" "value"}
           (cfg/inject sut {:a 1 :b ["a" "b"] :c {:key "value"}})))
    (is (= {:a "1" :b ["a" "b"] :c {:key "value"}}
           (cfg/surject sut
                        {"a" "1"
                         "b.0" "a" "b.1" "b"
                         "c.key" "value"}))))
  (let [sut (cfg/properties-bijection :separator "_")]
    (is (= {"a" "1"
            "b_0" "a" "b_1" "b"
            "c_key" "value"}
           (cfg/inject sut {:a 1 :b ["a" "b"] :c {:key "value"}})))
    (is (= {:a "1" :b ["a" "b"] :c {:key "value"}}
           (cfg/surject sut
                        {"a" "1"
                         "b_0" "a" "b_1" "b"
                         "c_key" "value"})))))


;; << Config acquisition >>

(deftest test-simple-config
  (is (= {:string "value"
          :integer 1
          :double 3.2}
         (cfg/retrieve (file "test/uwh/data/simple.clj")))))

(deftest test-self-reference
  (is (= {:server {:host "127.0.0.0"
                   :port 8080}
          :appserver {:server {:host "127.0.0.0"
                               :port 8080}}
          :topology {:server1 {:server {:host "127.0.0.0"
                                        :port 8080}}}}
         (cfg/retrieve (file "test/uwh/data/selfreference.clj"))))

  (is (thrown? Exception
               (cfg/retrieve (file "test/uwh/data/cycle.clj")))))

(deftest test-nested-config
  (is (= {:nested {:string "value"
                   :integer 1
                   :double 3.2}}
         (cfg/retrieve (file "test/uwh/data/nested.clj"))))

  (is (= {:second {:nested {:string "value"
                            :integer 1
                            :double 3.2}}}
         (cfg/retrieve (file "test/uwh/data/doublenested.clj")))))

(deftest test-merged-config
  (is (= {:string "overridden"
          :integer 1
          :double 3.2
          :nested {:string "overridden2"
                   :integer 1
                   :double 3.2}}
         (cfg/retrieve (file "test/uwh/data/merged.clj")))))

(deftest test-system-properties
  (let [props (cfg/retrieve "system:properties")]
    (is (not= nil (get-in props [:user :name]))))

  (testing "system:properties via ::reference element"
    (let [props (cfg/retrieve (file "test/uwh/data/sys.clj"))]
      (is (not= nil (get-in props [:user :name]))))))

(deftest test-system-env
  (let [props (cfg/retrieve "system:env")]
    (is (not= nil (get-in props [:lein :home])))))
