(ns cypris-form.contract-test
  (:require [clojure.test :refer :all]
            [cypris-form.contract :as c]
            [cypris-form.error :as e]
            [cypris-form.validation :as v]))


;; type checks

(deftest ct?
  (is (c/ct? [() {:foo (fn [v] '("wrong"))}]))
  (is (c/ct? [() {[:foo :bar] (fn [v1 v2] '(("wrong") ("wrong")))}])))

;; validation

(defn vld [x] (list "wrong"))

(deftest no-validation
  (let [validation (c/no-validation)]
    (is (= ['() {}] (validation {:field "whatever"})))))

(deftest generate-validation
  (testing "empty contract"
    (let [contract ['() {}]
          vld (c/gen-vld contract)]
      (is (= [() {}] (vld {})))))

  (testing "base constraint"
    (let [contract [(list #(list (inc (:field %)))) {}]
          input {:field 1}
          vld (c/gen-vld contract)]
      (is (= ['(2) {}] (vld input)))))

  (testing "single field constraint"
    (let [contract ['() {:field (fn [v] (list v))}]
          input {:field "hello world"}
          vld (c/gen-vld contract)]
      (is (= ['() {:field '("hello world")}] (vld input))))))

(deftest generate-collection-validation
  (testing "empty contract"
    (let [contract ['() ['() {}]]
          vld (c/gen-coll-vld contract)]
      (is (= ['() [['() {}]]] (vld [{}])))))

  (testing "base constraint"
    (let [contract [(list #(list (count %))) ['() {}]]
          vld (c/gen-coll-vld contract)]
      (is (= ['(1) [['() {}]]] (vld [{}])))
      (is (= ['(2) [['() {}]['() {}]]] (vld [{} {}])))))

  (testing "single field constraint"
    (let [contract ['() ['() {:field (fn [v] (list v))}]]
          vld (c/gen-coll-vld contract)]
      (is (= ['() [['() {:field '("hello world")}]]] (vld [{:field "hello world"}])))
      (is (= ['() []] (vld [])))))

  (testing "wrong input"
    (let [contract ['() ['() {}]]
          vld (c/gen-coll-vld contract)]
      (is (= ['("not a collection") [['() {}]]] (vld {}))))))

(deftest contract
  (testing "ct"
    (let [ct [() {:nested [() {:f vld}]}]]
      (is (= [() {:f vld}] (c/contract ct :nested)))))
  (testing "cct"
    (let [ct [() [() {:f vld}]]]
      (is (= [() {:f vld}] (c/contract ct))))))

(deftest contract?
  (is (c/ct? ['() {}]))
  (is (not (c/ct? ['() ['() {}]]))))

(deftest collection-contract?
  (is (not (c/cct? ['() {}])))
  (is (c/cct? ['() ['() {}]])))

;; base constraints

(deftest generate-base-constraints
  (testing "no constraints"
    (let [cs '()
          input {:field 1}
          bc (c/gen-bc cs)]
      (is (= '() (bc input)))))

  (testing "fn constraints"
    (let [cs (list #(list (inc (:field %))) #(list (inc (:field %))))
          input {:field 1}
          bc (c/gen-bc cs)]
      (is (= '(2 2) (bc input))))))


;; field constraints

(deftest generate-field-constraints
  (testing "empty field constraints"
    (let [fc (c/gen-fc {})]
      (is (= {} (fc {:field "hello world"})))))

  (testing "single field constraint"
    (let [fc (c/gen-fc {:field (fn [v] (list v))})]
      (is (= {:field '("hello world")} (fc {:field "hello world"})))))

  (testing "composite field constraint"
    (let [fc (c/gen-fc {[:field :other-field] (fn [v] (list (list v) (list v)))})]
      (is (= {:field '(["hello world" "hello you"])
              :other-field '(["hello world" "hello you"])} (fc {:field "hello world" :other-field "hello you"}))))))

(deftest value-fn
  (testing "single field"
    (let [fd :field
          value (c/value-fn fd)]
      (is (= "value" (value {:field "value"})))))

  (testing "multiple field"
    (let [fd [:field :another-field]
          value (c/value-fn fd)]
      (is (= ["value" "another value"]
             (value {:field "value" :another-field "another value"}))))))

(deftest value-fns
  (let [fds [:field]
        values (c/value-fns str fds)]
    (is (= {:field ":field"} values))))

(deftest constraint-fns
  (let [fc {:field 1}]
    (is (= {:field 2} (c/constraint-fns inc fc)))))

(deftest validation-fns
  (let [fc {:field identity}
        values {:field identity}
        fcs-map (c/validation-fns fc values)]
  (is (= {:field "5"} ((:field fcs-map) {:field "5"})))))

(deftest apply-field-constraints
  (let [fc {:field #(inc (:field %))}
        input {:field 1}]
      (is (= {:field 2} (c/apply-field-constraints fc input)))))

(deftest flatten-composite-constraint-result
  (testing "do nothing"
    (let [fcr {:field 'foo}]
      (is (= fcr (c/flatten-composite-constraint-result fcr)))))

  (testing "split multiple fields"
    (let [fcr {[:field :another-field] '(("error 1") ("error 2"))}]
      (is (= {:field '("error 1")
              :another-field '("error 2")} (c/flatten-composite-constraint-result fcr)))))

  (testing "merge messages"
    (let [fcr {[:field :another-field] '(("error 1") ("error 2"))
               :field '("another error")}]
      (is (= {:field '("another error" "error 1")
              :another-field '("error 2")} (c/flatten-composite-constraint-result fcr))))))

(deftest expand-composite-field-constraint-result
  (testing "empty values"
    (let [fcr-vec [[[:a :b] '()]]]
      (is (= {:a '() :b '()}
             (c/expand-composite-field-constraint-result fcr-vec)))))

  (testing "distinct fields"
    (let [fcr-vec [[[:a :b] '(("value 1") ("value 2"))]]]
      (is (= {:a '("value 1") :b '("value 2")}
             (c/expand-composite-field-constraint-result fcr-vec)))))

  (testing "overlapping fields"
    (let [fcr-vec [[[:a :b] '(("value 1") ("value 2"))][[:b :c] '(("other value 1") ("other value 2"))]]]
      (is (= {:a '("value 1") :b '("value 2" "other value 1") :c '("other value 2")}
             (c/expand-composite-field-constraint-result fcr-vec))))))

(deftest combine-field-constraint-and-value-fn
  (testing "execution order of value fn and constraint"
    (let [v #(conj % :v)
          c #(conj % :c)
          input []
          apply-fc (c/comb-constraint-value-fn c v)]
        (is (= [:v :c] (apply-fc input))))))

;; mapping field constraints

(deftest constraint-mapping
  (testing "some function"
    (is (= identity (c/constraint-mapping identity))))

  (testing "nested contract"
    (let [ct ['() {}]
          c (c/constraint-mapping ct)]
      (is (= :ct (-> c meta :constraint)))))

  (testing "collection contract"
    (let [ct ['() ['() {}]]
          c (c/constraint-mapping ct)]
      (is (= :cct (-> c meta :constraint)))))

  (testing "list constraints"
    (let [c (c/constraint-mapping `(foo bar))]
      (is (= :all (-> c meta :constraint)))))

  (testing "no constraint"
    (let [c (c/constraint-mapping nil)]
      (is (= :nothing (-> c meta :constraint)))))

  (is (thrown? Exception (c/constraint-mapping :wrong))))


;; execution

(deftest all
  (testing "no constraints"
    (let [c (c/all)]
      (is (= () (c "whatever")))))

  (testing "all ok"
    (let [c (c/all (fn [_] ()) (fn [_] ()))]
      (is (= () (c "whatever")))))

  (testing "first fails"
    (let [c (c/all (fn [v] (list v)) (fn [v] (throw (Exception. "shall not be executed"))))]
      (is (= '("error") (c "error")))))

  (testing "last fails"
    (let [c (c/all (fn [_] ()) (fn [v] (list v)))]
      (is (= '("error") (c "error"))))))

(deftest no-constraint
  (is (= () (c/no-constraint 'foo))))

;; merge contracts

(deftest merge-contracts
  (testing "empty contract"
    (let [ct1 [() {}]
          ct2 [() {}]]
      (is (= [() {}] (c/merge-contracts ct1 ct2)))))

  (testing "simple contracts"
    (let [ct1 ['(foo) {:field 'baz}]
          ct2 ['(bar) {:field 'qux}]]
      (is (= ['(foo bar) {:field '(baz qux)}] (c/merge-contracts ct1 ct2))))))

(deftest merge-collection-contracts
  (testing "simple contracts"
    (let [ct1 [['(foo) {:field 'baz}]]
          ct2 [['(bar) {:field 'qux}]]]
      (is (= [['(foo bar) {:field '(baz qux)}]] (c/merge-coll-contracts ct1 ct2))))))

(deftest merge-base-constraints
  (is (= '(foo bar) (c/merge-bcs '(foo) '(bar)))))

(deftest merge-field-constraints
  (testing "empty fields"
    (is (= {} (c/merge-fcs nil {} {}))))

  (testing "different fields"
    (is (= {:field 'foo :other-field 'bar} (c/merge-fcs nil {:field 'foo} {:other-field 'bar}))))

  (testing "same fields"
    (is (= {:field 3} (c/merge-fcs + {:field 1} {:field 2})))))

(deftest merge-contracts-field-constraint-mapping
  (testing "field contracts"
    (let [merge-fn (c/merge-contracts-mapping {} {})]
      (is (= :fc (-> merge-fn meta :merger)))))

  (testing "different but empty contract"
    (let [merge-fn (c/merge-contracts-mapping [() {}] [() [() {}]])]
      (is (= :last (-> merge-fn meta :merger)))))

  (testing "nested contracts"
    (let [merge-fn (c/merge-contracts-mapping [() {}] [() {}])]
      (is (= :ct (-> merge-fn meta :merger)))))

  (testing "collection contracts"
    (let [merge-fn (c/merge-contracts-mapping [() [() {}]] [() [() {}]])]
      (is (= :cct (-> merge-fn meta :merger))))))

(deftest merge-field-constraints-strategy
  (testing "same fields with lists"
    (is (= '(foo bar) (c/merge-fcs-strategy '(foo) '(bar)))))

  (testing "same fields with something and something"
    (is (= '(foo bar) (c/merge-fcs-strategy 'foo 'bar))))

  (testing "same fields with something and list"
    (is (= '(foo bar) (c/merge-fcs-strategy 'foo '(bar)))))

  (testing "same fields with list and something"
    (is (= '(foo bar) (c/merge-fcs-strategy '(foo) 'bar)))))

(deftest empty-contract?
  (is (c/empty-contract? [() {}])))
