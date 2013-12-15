(ns uwh.config-test
  (:require [uwh.config :as cfg])
  (:use [clojure.test :only [deftest is testing]]
        [clojure.java.io :only [file]]))

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