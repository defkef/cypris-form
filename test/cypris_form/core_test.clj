(ns cypris-form.core-test
  (:require [clojure.test :refer :all]
            [cypris-form.core :as c]
            [cypris-form.fdef :as f]
            [cypris-form.error :as e]))

;; test helper

(defn state-with-error
  ([err values]
    (let [state (c/new-state values)]
      (swap! state assoc c/*error-key* err)
      state))
  ([err]
   (atom {c/*error-key* err})))

;; init

(deftest new-state
  (testing "init values without defaults"
    (let [values {:key :value}
          form-values (c/new-state values)]
      (is (= {c/*input-key* {:key :value}
              c/*original-key* {:key :value}} @form-values)))))

;; form

(deftest form-tree
  (let [udef [:a {} :b [:c {}] :d [[:e {}]]]
        form (c/form udef {:a 'a :b {:c 'c} :d [{:e 'e1} {:e 'e2}]} #())]

    (testing "a"
      (is (= 'a
             (-> form
                 (c/field :a)
                 c/value))))

    (testing "b"
      (is (= 'c
             (-> form
                 (c/field :b)
                 (c/field :c)
                 c/value))))

    (testing "d"
      (is (= 2 (c/size (c/field form :d))))
      (is (= 'e2
             (-> form
                 (c/field :d)
                 second
                 (c/field :e)
                 c/value))))))

(deftest validation
  (letfn [(is-int [x] (if (integer? x) () (list "expected int")))
          (more-than-one [x] (if (> (count x) 1) () '("at least 2")))]
    (let [fdef [:a {:validate is-int}
                :b [:b1 {:validate is-int}]
                :c [[:c1 {:validate is-int}] {:validate more-than-one}]]]

      (testing "valid state"
        (let [f (c/form fdef {:a 1 :b {:b1 1} :c [{:c1 1} {:c1 2}]} nil)]
          (c/validate! f)
          (is (c/valid? f))))

      (testing "invalid state"
        (let [f (c/form fdef {:a "WRONG"
                              :b {:b1 "WRONG"}
                              :c [{:c1 "WRONG"}]} nil)]
          (c/validate! f)
          (is (not (c/valid? f)))
          (is (= '("expected int") (-> f (c/field :a) (c/errors))))
          (is (= '("expected int") (-> f (c/field :b :b1) (c/errors))))
          (is (= '("at least 2") (-> f (c/field :c) (c/base-errors))))
          (is (= '("expected int") (-> f (c/field :c) (c/forms) first (c/field :c1) (c/errors))))))

      (testing "valid subform"
        (let [s (c/new-state {:a "WRONG" :b {:b1 1} :c [{:c1 1} {:c1 2}]})
              f (c/form-with-state fdef s nil)
              f1 (c/field f :b)]
          (c/validate! f1)
          (is (c/valid? f1))))

      (testing "invalid collform"
        (let [s (c/new-state {:a 1 :b {:b1 1} :c [{:c1 "WRONG"}]})
              f (c/form-with-state fdef s nil)
              f1 (c/field f :c)]
          (c/validate! f1)
          (is (not (c/valid? f1)))
          (is (= ['("at least 2") [[() {:c1 '("expected int")}]]] (c/errors f1)))))

      (testing "field"
        (let [s (c/new-state {:a "WRONG" :b {:b1 1} :c [{:c1 1} {:c1 2}]})
              f (c/form-with-state fdef s nil)
              f1 (c/field f :a)]
          (c/validate! f1)
          (is (not (c/valid? f1)))
          (is (= '("expected int") (c/errors f1)))))
      )))


(deftest additional-contract
  (let [constraint #(if (= {:field "right"} %) () '("field is not right"))
        contract [(list constraint) {}]]

    (testing "with valid state"
      (let [action-called-with (atom :not-called)
            action #(reset! action-called-with %)
            state (c/new-state {:field "right"})
            f (c/form-with-state [:field {}] state action contract)
            _ (c/submit! f)]

        (is (c/valid-base? f))
        (is (= {:values {:field "right"} :form f} @action-called-with))))

    (testing "with invalid state"
      (let [action-called-with (atom :not-called)
            action #(reset! action-called-with %)
            state (c/new-state {:field "wrong"})
            f (c/form-with-state [:field {}] state action contract)
            _ (c/submit! f)]

        (is (= :not-called @action-called-with))
        (is (not (c/valid-base? f)))))

    (testing "with former invalid state"
      (let [action-called-with (atom :not-called)
            action #(reset! action-called-with %)
            state (state-with-error (e/->error '("some error") {}) {:field "right"})
            f (c/form-with-state [:field {}] state action contract)]
        ;;precondition check
        (is (not (c/valid-base? f)))
        (c/submit! f)
        (is (true? (c/valid-base? f)))))))

(deftest form
  (let [fdef [:user {:type :text}]
        state (c/new-state {:user "me"})
        submitted-values (atom nil)
        action #(reset! submitted-values %)
        f (c/form-with-state fdef state action)]

    (testing "fields"
      (is (= [:user] (c/fields f))))

    (testing "field"
      (is (c/field f :user)))

    (testing "input"
      (is (= {:user "me"} (c/input f))))

    (testing "no errors"
      (is (c/valid-base? f))
      (is (empty? (c/base-errors f))))

    (testing "path"
      (is (= [] (c/path f))))

    (testing "submit"
      (c/submit! f)
      (is (= {:values {:user "me"} :form f} @submitted-values))))

  (testing "set input manually"
    (let [fdef [:user {:type :text}]
          state (c/new-state {:user "me"})
          f (c/form-with-state fdef state #())]
      (c/set-input! f {:user "john"})
      (is (= {:user "john"} (c/input f)))))

  (testing "has changed input"
    (let [fdef [:field {:type :text}]
          state (c/new-state {})
          f (c/form-with-state fdef state (fn [_]))
          fd (c/field f :field)]

      (is (= false (c/changed? f)))
      (c/set-value! fd "hello")
      (is (= true (c/changed? f)))
      (c/submit! f)
      (is (= false (c/changed? f)))))

  (testing "nested fields"
    (testing "2"
      (let [f (c/form-with-state [:user [:name {:type :text}]] nil nil)]
        (is (c/field f :user :name))))

    (testing "3"
      (let [f (c/form-with-state [:user [:name [:first {:type :text}]]] nil nil)]
        (is (c/field f :user :name :first)))))

  (testing "has base errors"
    (let [err (e/->error '("base error") {})
          state (state-with-error err)
          f (c/form-with-state [:a {}] state nil)]
      (is (not (c/valid-base? f)))
      (is (not (c/valid? f)))
      (is (= '("base error") (c/base-errors f)))))

  (testing "has field errors"
    (let [err (e/->error '() {:field '("field error")})
          state (state-with-error err)
          f (c/form-with-state [:field {}] state nil)]
      (is (c/valid-base? f))
      (is (not (c/valid? f)))
      (is () (c/base-errors f))))

  (testing "set errors manually"
    (let [err (e/->error '() {:field '("field error")})
          f (c/form [:field {}] {} nil)]
      (c/set-error! f err)
      (is (not (c/valid? (c/field f :field))))
      (is (not (c/valid? f))))))

(deftest subform
  (let [fdef (f/->fdef [:user [:name {:type :text}]])]
    (testing "has base errors"
      (let [err (e/->error () {:user (e/->error '("base error") {})})
            state (state-with-error err)
            cursor (c/cursor state [:user])
            f (c/subform fdef [() {}] nil cursor)]

        (is (not (c/valid-base? f)))
        (is (= '("base error") (c/base-errors f)))))))

(deftest collform
  (testing "empty coll"
    (let [state (c/new-state {:coll []})
          fdef (f/->fdef [:coll [[:field {}]]])
          [_ cfdef] (f/fd fdef :coll)
          c (c/cursor state [:coll])
          f (c/collform cfdef [() [() {}]] nil c)]
      (is (= 0 (c/size f)))
      (is (empty? (c/forms f)))
      (is (c/valid-base? f))
      (is (empty? (c/base-errors f)))))

  (testing "has base errors"
    (let [err (e/->error {:coll ['("base error") [[() {}]]]})
          state (state-with-error err)
          fdef (f/->fdef [:coll [[:field {}]]])
          [_ cfdef] (f/fd fdef :coll)
          c (c/cursor state [:coll])
          f (c/collform cfdef [() [() {}]] nil c)]
      (is (not (c/valid-base? f)))
      (is (= '("base error") (c/base-errors f)))))

  (testing "two element collection"
    (let [state (c/new-state {:coll [{:field 'bar} {:field 'baz}]})
          fdef (f/->fdef [:coll [[:field {}]]])
          [_ cfdef] (f/fd fdef :coll)
          c (c/cursor state [:coll])
          f (c/collform cfdef [() [() {}]] nil c)]
      (is (= 2 (c/size f)))
      (is (= (list {:field 'bar} {:field 'baz}) (c/input f)))))

  (testing "add form"
    (let [state (c/new-state {:coll []})
          fdef (f/->fdef [:coll [[:field {}]]])
          [_ cfdef] (f/fd fdef :coll)
          c (c/cursor state [:coll])
          f (c/collform cfdef [() [() {}]] nil c)]
      (c/add! f)
      (is (= 1 (c/size f)))
      (is (= {:coll [{:field nil}]} (c/*input-key* @state)))))

  (testing "add form with values"
    (let [state (c/new-state {:coll []})
          fdef (f/->fdef [:coll [[:field {}]]])
          [_ cfdef] (f/fd fdef :coll)
          c (c/cursor state [:coll])
          f (c/collform cfdef [() [() {}]] nil c)]
      (c/add! f {:field 'foo})
      (is (= 1 (c/size f)))
      (is (= {:coll [{:field 'foo}]} (c/*input-key* @state)))
      (is (c/has? f {:field 'foo}))))

  (testing "remove form by index"
    (let [state (c/new-state {:coll [{:field 'foo}]})
          fdef (f/->fdef [:coll [[:field {}]]])
          [_ cfdef] (f/fd fdef :coll)
          c (c/cursor state [:coll])
          f (c/collform cfdef [() [() {}]] nil c)]
      (c/remove-by-index! f 0)
      (is (= 0 (c/size f)))
      (is (= {:coll []} (c/*input-key* @state)))))

  (testing "remove form by obj"
    (let [state (c/new-state {:coll [{:field 'foo}]})
          fdef (f/->fdef [:coll [[:field {}]]])
          [_ cfdef] (f/fd fdef :coll)
          c (c/cursor state [:coll])
          f (c/collform cfdef [() [() {}]] nil c)
          child (first (c/forms f))]

      (is child)
      (c/remove! f child)
      (is (= 0 (c/size f)))
      (is (= {:coll []} (c/*input-key* @state))))))

(deftest field
  (let [state (c/new-state {:field "a value"})
        c (c/cursor state [:field])
        fd (c/new-field :field {:some-key "some value"} #() c)]

    (testing "field name"
      (is (= "field" (c/name fd))))

    (testing "field spec accessor"
      (is (= "some value" (:some-key fd))))

    (testing "value"
      (is (= "a value" (c/value fd))))

    (testing "path"
      (is (= [:field] (c/path fd))))

    (testing "id"
      (is (= "field" (c/id fd))))

    (testing "no errors"
      (is (c/valid? fd))
      (is (empty? (c/errors fd))))

    (testing "set-value!"
      (let [_ (c/set-value! fd "new value")]
        (is (= "new value" (c/value fd)))))

    (testing "set-error!"
      (let [_ (c/set-error! fd "error")]
        (is (not (c/valid? fd)))
        (is (= '("error") (c/errors fd))))))

  (testing "has changed value"
    (let [state (c/new-state {:field "a value"})
          c (c/cursor state [:field])
          fd (c/new-field :field {:some-key "some value"} #() c)]

      (is (= false (c/changed? fd)))
      (c/set-value! fd "new value")
      (is (= true (c/changed? fd)))))

  (testing "has errors"
    (let [state (state-with-error (e/->error {:field '("error")}))
          cursor (c/cursor state [:field])
          fd (c/new-field :field nil #() cursor)]
      (is (not (c/valid? fd)))
      (is (= '("error") (c/errors fd)))))

  (testing "with path"
    (let [state (c/new-state {:parent {:field "a value"}})
          cursor (c/cursor state [:parent :field])
          fd (c/new-field :field 'foo #() cursor)]

      (testing "value"
        (is (= "a value" (c/value fd))))

      (testing "id"
        (is (= "parent_field" (c/id fd))))

      (testing "set-value!"
        (let [_ (c/set-value! fd "new value")]
          (is (= "new value" (c/value fd)))))

      (testing "set-error!"
        (let [_ (c/set-error! fd "error")]
          (is (not (c/valid? fd)))
          (is (= '("error") (c/errors fd))))))))

;; impl

(deftest cursor
  (testing "init"
    (let [c (c/cursor 'state ['bar])]
      (is (= {:state 'state :path ['bar]} c))))

  (testing "subcursor"
    (let [c (c/cursor 'state ['bar])
          sc (c/cursor c ['foo])]
      (is (= {:state 'state :path ['bar 'foo]} sc)))))

(deftest forms*
  (testing "sequence"
    (let [state (c/new-state {:coll []})
          c (c/cursor state [:coll])
          f (c/forms* nil [() {}] nil c)]

      (is (seq? f))
      (is (= 3 (count (take 3 f))))
      (is (every? #(satisfies? c/IForm %) (take 3 f)))
      (is (= '([:coll 0] [:coll 1] [:coll 2]) (map c/path (take 3 f)))))))

(deftest set-value!*
  (testing "with empty state"
    (let [s (c/new-state {})
          _ (c/set-value!* (c/cursor s [:field]) "new value")]
      (is (= {:field "new value"} (c/input* (c/cursor s []))))))

  (testing "with present state"
    (let [s (c/new-state {:field {:subfield "old value"}})
          c (c/cursor s [:field :subfield])
          _ (c/set-value!* c "new value")]
      (is (= "new value" (c/input* c))))))

(deftest update-value!*
  (testing "with empty state"
    (let [s (c/new-state {:field ['foo]})
          _ (c/update-value!* (c/cursor s [:field]) conj 'bar)]
      (is (= {:field ['foo 'bar]} (c/input* (c/cursor s [])))))))

(deftest contains-value?*
  (let [s (c/new-state {})
        _ (c/update-value!* (c/cursor s [:field]) conj {:foo 'bar})]
    (is (c/contains-value?* (c/cursor s [:field]) {:foo 'bar}))
    (is (not (c/contains-value?* (c/cursor s [:field]) {:foo 'baz})))))

(deftest remove-value!*
  (testing "with empty state"
    (let [s (c/new-state {:field ['foo]})
          _ (c/remove-value!* (c/cursor s [:field]))]
      (is (= {} (c/input* (c/cursor s [])))))))

(deftest errors*
  (testing "unvalidated state"
    (let [s (c/new-state {})]
      (is (empty? (c/errors* (c/cursor s []))))))

  (testing "state with errors"
    (let [s (state-with-error "whatever")]
      (is (= "whatever" (c/errors* (c/cursor s []))))))

  (testing "nested state with errors"
    (let [s (state-with-error (e/->error {:field (e/->error {:subfield '("whatever")})}))]
      (is (= '("whatever") (c/errors* (c/cursor s [:field :subfield])))))))

(deftest set-errors!*
  (testing "root"
    (let [s (c/new-state {})
          err '("whatever")
          c (c/cursor s [])
          _ (c/set-errors!* c err)]
      (is (= '("whatever") (c/errors* c)))))

  (testing "child"
    (let [s (c/new-state {})
          err '("whatever")
          c (c/cursor s [:a])
          _ (c/set-errors!* c err)]
      (is (= '("whatever") (c/errors* c))))))

(deftest set-field-error!*
  (let [s (c/new-state {:field "value"})
        err "whatever"
        _ (c/set-field-error!* (c/cursor s [:parent :field]) err)]
    (is (= (e/->error () {:field '("whatever")})
           (c/errors* (c/cursor s [:parent]))))))

(deftest input*
  (testing "empty state"
    (let [s (c/new-state {})]
      (is (= {} (c/input* (c/cursor s []))))))

  (testing "state"
    (let [s (c/new-state {:field "abc"})]
      (is (= {:field "abc"} (c/input* (c/cursor s []))))))

  (testing "with path"
    (let [s (c/new-state {:field "abc"})]
      (is (= "abc" (c/input* (c/cursor s [:field]))))))

  (testing "without errors"
    (let [s (state-with-error "some errors" {})]
      (is (= {} (c/input* (c/cursor s [])))))))

(deftest changed?*
  (testing "empty state"
    (let [s (c/new-state {})]
      (is (= false (c/changed?* (c/cursor s []))))))

  (testing "no change"
    (let [s (c/new-state {:field 'foo})]
      (is (= false (c/changed?* (c/cursor s [:field]))))))

  (testing "change"
    (let [s (c/new-state {:field 'foo})
          c (c/cursor s [:field])]
      (c/set-value!* c 'bar)
      (is (= true (c/changed?* c)))))

  (testing "reset change"
    (let [s (c/new-state {:field 'foo})
          c (c/cursor s [:field])]
      (c/set-value!* c 'bar)
      (c/reset-changes!* c)
      (is (= false (c/changed?* c))))))

(deftest reset-changes!*
  (testing "empty state"
    (let [s (c/new-state {})
          c (c/cursor s [])]
      (c/reset-changes!* c)
      (is (= {} (c/original* c)))))

  (testing "unchanged input"
    (let [s (c/new-state {:field 'foo})
          c (c/cursor s [:field])]
      (c/reset-changes!* c)
      (is (= 'foo (c/original* c)))))

  (testing "changed input"
    (let [s (c/new-state {:field 'foo})
          c (c/cursor s [:field])]
      (c/set-value!* c 'bar)
      (c/reset-changes!* c)
      (is (= 'bar (c/original* c))))))

(deftest validate!*
  (testing "invalid"
    (let [s (c/new-state {})
          c (c/cursor s [])
          validation (fn [vs] [() {:field (list "meep")}])]
      (c/validate!* c validation)
      (is (= [() {:field '("meep")}] (c/errors* c)))))

  (testing "valid"
    (let [s (c/new-state {})
          c (c/cursor s [])
          validation (fn [vs] [() {:field ()}])]
      (c/validate!* c validation)
      (is (= [() {:field ()}] (c/errors* c))))))

(deftest submit!*
  (testing "happy path with coercion"
    (let [s (c/new-state 1)
          c (c/cursor s [])
          coercion inc
          validation (fn [vs] [])
          result (atom nil)
          action #(reset! result %)]

      (c/submit!* c validation coercion action)
      (is (= 2 @result))))

  (testing "invalid input"
    (let [s (c/new-state {:field 1})
          c (c/cursor s [])
          coercion nil
          validation (fn [vs] [() {:field '("meep")}])
          result (atom nil)
          action #(reset! result %)]

      (c/submit!* c validation coercion action)
      (is (= nil @result)))))

;; regression

(deftest coll-path-id
  (let [cursor (c/cursor nil [:parent 0 :field])
        fd (c/new-field :field 'foo #() cursor)]
    (is (= "parent_0_field" (c/id fd)))))
