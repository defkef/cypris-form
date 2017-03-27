(ns cypris-form.fdef-test
  (:require [cypris-form.fdef :as f]
            [cypris-form.contract :as c]
            [clojure.test :refer :all]))


(deftest ->fdef
  (testing "single field"
    (let [fdef (f/->fdef [:a {}])]
      (is fdef)))

  (testing "multiple fields"
    (let [fdef (f/->fdef [:a {} :b {}])]
      (is fdef)))

  (testing "nested form"
    (let [fdef (f/->fdef [:a [:b {}]])]
      (is fdef)))

  (testing "coll form"
    (let [fdef (f/->fdef [:a [[:b {}]]])]
      (is fdef)))

  (testing "coll form with opt"
    (let [fdef (f/->fdef [:a [[:b {}] {}]])]
      (is fdef)))

  (testing "error"
    (is (thrown? Exception (f/->fdef 'wrong)))))

(deftest fields
  (let [fdef (f/->fdef [:a {} :b {} :c [:d {}]])]
    (is (= [:a :b :c] (f/fields fdef)))))

(deftest fd
  (testing "field"
    (let [fdef (f/->fdef [:a {} :b {}])
          [t f] (f/fd fdef :a)]
      (is (= :field t))
      (is (= {} f))))

  (testing "form"
    (let [fdef (f/->fdef [:a [:b {}]])
          [t f] (f/fd fdef :a)]
      (is (= :form t))
      (is (= [:field {}] (f/fd f :b)))))

  (testing "collform"
    (let [fdef (f/->fdef [:a [[:b {}]]])
          [t [f o]] (f/fd fdef :a)]
      (is (= :collform t))
      (is (= [:field {}] (f/fd f :b)))))

  (testing "collform with options"
    (let [fdef (f/->fdef [:a [[:b {}] {:foo 'bar}]])
          [t [f o]] (f/fd fdef :a)]
      (is (= :collform t))
      (is (= [:field {}] (f/fd f :b)))
      (is (= {:foo 'bar} o)))))


(deftest fspec-tree
  (testing "without options"
    (let [fdef (f/->fdef [:a {:foo 'foo} :b [:c {:bar 'bar}] :d [[:e {:baz 'baz}]]])
          tree (f/fspec-tree identity fdef)]
      (is (= {:a {:foo 'foo}
              :b {:c {:bar 'bar}}
              :d [{:e {:baz 'baz}}]} tree))))

  (testing "with options"
    (let [fdef (f/->fdef [:d [[:e {:baz 'baz}] {:foo 'foo}]])
          tree (f/fspec-tree identity fdef)]
      (is (= {:d [{:e {:baz 'baz}} {:foo 'foo}]} tree)))))

;; default-values

(deftest default-values
  (testing "single field form"
    (let [f (f/->fdef [:field {:default-value 'foo :foo 'bar}])]
      (is (= {:field 'foo} (f/default-values f))))))

(deftest default-value-opt
  (testing "no default-values"
    (let [fdspec {}]
      (is (= nil (f/default-value-opt fdspec)))))

  (testing "explicit default"
    (let [fdspec {:default-value 'foo}]
      (is (= 'foo (f/default-value-opt fdspec)))))

  (testing "select type"
    (let [fdspec {:type :select :options [["first" "First"] ["second" "Second"]]}]
      (is (= "first" (f/default-value-opt fdspec)))))

  (testing "select type without options"
    (let [fdspec {:type :select :options []}]
      (is (= "" (f/default-value-opt fdspec)))))

  (testing "checkbox type"
    (let [fdspec {:type :checkbox :options [["first" "First"] ["second" "Second"]]}]
      (is (= #{} (f/default-value-opt fdspec)))))

  (testing "boolean type"
    (let [fdspec {:type :boolean}]
      (is (= false (f/default-value-opt fdspec))))))

;; coercions

(deftest coercion-opt
  (testing "no coercion"
    (let [fdspec {}]
      (is (= identity (f/coercion-opt fdspec)))))

  (testing "explicit coercion"
    (let [fdspec {:coerce 'foo}]
      (is (= 'foo (f/coercion-opt fdspec))))))

(deftest coercion
  (testing "empty values"
    (is (= {} ((f/coercion nil) nil))))

  (testing "missing coercion"
    (let [v {:field 1}
          f (f/->fdef [:field {}])
          c (f/coercion f)]
      (is (= {:field 1} (c v)))))

  (testing "single field form"
    (let [f (f/->fdef [:field {:coerce inc}])
          c (f/coercion f)]
      (is (= {:field 2} (c {:field 1})))))

  (testing "nested form"
    (let [v {:field {:subfield 1}}
          f (f/->fdef [:field [:subfield {:coerce inc}]])
          c (f/coercion f)]
      (is (= {:field {:subfield 2}} (c v)))))

  (testing "coll form"
    (let [v {:field [{:subfield 1} {:subfield 3}]}
          f (f/->fdef [:field [[:subfield {:coerce inc}]]])
          c (f/coercion f)]
      (is (= {:field [{:subfield 2} {:subfield 4}]} (c v))))))


;; contract

(deftest contract
  (testing "empty contract"
    (let [fdef (f/->fdef [:a {}
                          :coll [[:b {}]]])
          c (f/contract fdef)]
      (is (c/ct? c))))

  (testing "field"
    (let [fdef (f/->fdef [:a {:validate 'foo}])
          c (f/contract fdef)]
      (is (= ['() {:a 'foo}] c))))

  (testing "multiple fields"
    (let [fdef (f/->fdef [[:a :b] {:validate 'foo}])]
      (is (= ['() {[:a :b] 'foo}] (f/contract fdef)))))

  (testing "form"
    (let [fdef (f/->fdef [:a [:b {:validate 'foo}]])]
      (is (= ['() {:a ['() {:b 'foo}]}] (f/contract fdef)))))

  (testing "coll with field constraint"
    (let [fdef (f/->fdef [:a [[:b {:validate 'foo}]]])]
      (is (= ['() {:a [() [() {:b 'foo}]]}] (f/contract fdef)))))

  (testing "coll with base constraint"
    (let [fdef (f/->fdef [:a [[:b {}] {:validate 'bar}]])]
      (is (= ['() {:a ['(bar) [() {}]]}] (f/contract fdef)))))

  (testing "coll with multiple base constraints"
    (let [fdef (f/->fdef [:a [[:b {}] {:validate '(bar baz)}]])]
      (is (= ['() {:a ['(bar baz) [() {}]]}] (f/contract fdef))))))

