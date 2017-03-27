(ns cypris-form.input-test
  (:require [clojure.test :refer :all]
            [cypris-form.input :as i]))

(deftest map-input
  (testing "empty values"
    (let [v {}
          df {:key 'foo}]
      (is (= {:key 'foo} (i/input v df :not-found)))))

  (testing "nil value"
    (let [v nil
          df {:key 'foo}]
      (is (= {:key 'foo} (i/input v df :not-found)))))

  (testing "missing property"
    (let [v :not-found
          df {:key 'foo}]
      (is (= {:key 'foo} (i/input v df :not-found)))))

  (testing "missing value"
    (let [v {:key :not-found}
          df {:key 'foo}]
      (is (= {:key 'foo} (i/input v df :not-found)))))

  (testing "missing nested property"
    (let [v {:key {:subkey :not-found}}
          df {:key {:subkey 'foo}}]
      (is (= {:key {:subkey 'foo}} (i/input v df :not-found)))))

  (testing "present value"
    (let [v {:key 'bar}
          df {:key 'foo}]
      (is (= {:key 'bar} (i/input v df :not-found)))))

  (testing "present nested value without defaults"
    (let [v {:key {:content 'foo}}
          df {:key nil}]
      (is (= {:key {:content 'foo}} (i/input v df :not-found)))))

  (testing "Set property with defaults"
    (let [v {:key #{'foo}}
          df {:key #{}}]
      (is (= {:key #{'foo}} (i/input v df :not-found)))))

  (testing "collection property"
    (let [v {:key [{:content :not-found}]}
          df {:key [{:content 'foo}]}]
      (is (= {:key [{:content 'foo}]} (i/input v df :not-found)))))

  (testing "empty nested collection property"
    (let [v {}
          df {:key [{:content 'foo}]}]
      (is (= {:key []} (i/input v df :not-found)))))

  (testing "missing nested collection property"
    (let [v {:key :not-found}
          df {:key [{:content 'foo}]}]
      (is (= {:key []} (i/input v df :not-found))))))

(deftest collection-input
  (testing "empty values"
    (let [v []
          df [{:key 'foo} {}]
          input (i/input v df :not-found)]
      (is (vector? input))
      (is (= [] input))))

  (testing "empty values with size"
    (let [v []
          df [{:key 'foo} {:default-size 2}]
          input (i/input v df :not-found)]
      (is (vector? input))
      (is (= [{:key 'foo} {:key 'foo}] input))))

  (testing "present item with present key"
    (let [v [{:key 'bar}]
          df [{:key 'foo}]
          input (i/input v df :not-found)]
      (is (= [{:key 'bar}] input))))

  (testing "one item with present, one with missing key"
    (let [v [{:key 'bar} {:key :not-found}]
          df [{:key 'foo}]
          input (i/input v df :not-found)]
      (is (vector? input))
      (is (= [{:key 'bar} {:key 'foo}] input)))))
