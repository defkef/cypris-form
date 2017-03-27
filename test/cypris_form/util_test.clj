(ns cypris-form.util-test
  (:require [clojure.test :refer :all]
            [cypris-form.util :as u]))

(deftest join
  (let [coll [:foo 0 "bar"]]
    (is (= "foo_0_bar") (u/join coll "_"))))

(deftest ->str
  (is (= "foo" (u/->str :foo)))
  (is (= "0" (u/->str 0))))

(deftest dissoc-in
  (testing "map vec map"
    (let [coll {:key [{:subkey 'foo :subkey2 'bar}]}]
      (is (= {:key [{:subkey 'foo}]}
             (u/dissoc-in coll [:key 0 :subkey2])))))

  (testing "map vec"
    (let [coll {:key [{:subkey 'foo} {:subkey 'bar} {:subkey 'baz}]}]
      (is (= {:key [{:subkey 'foo} {:subkey 'baz}]}
             (u/dissoc-in coll [:key 1])))
      (is (= {:key [{:subkey 'foo}]}
             (-> coll
              (u/dissoc-in [:key 1])
              (u/dissoc-in [:key 1])))))))
