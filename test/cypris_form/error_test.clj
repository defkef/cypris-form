(ns cypris-form.error-test
  (:require [clojure.test :refer :all]
            [cypris-form.error :as e]))

(deftest error
  (testing "no errors"
    (is (= [() {}] (e/->error () {}))))

  (testing "field errors"
    (is (= [() {:field '("error")}] (e/->error {:field '("error")}))))

  (testing "base errors"
    (is (= ['("error") {}] (e/->error '("error") {}))))

  (testing "base and field errors"
    (is (= ['("base error") {:field '("field error")}]
           (e/->error '("base error") {:field '("field error")}))))

  (testing "collection errors"
    (is (= ['("coll base error") [['("entity base error") {:field '("field error")}]]]
           (e/->error
             '("coll base error")
             [(e/->error '("entity base error") {:field '("field error")})])))))

(deftest error-tree
  (testing "empty path"
    (let [err (e/error-tree '(foo) [])]
      (is (= '(foo) (e/lookup-errors err [])))))

  (testing "nested path"
    (let [err (e/error-tree '(foo) [:field 0 :subfield])]
      (is (= '(foo) (e/lookup-errors err [:field 0 :subfield]))))))

(deftest lookup-errors
  (testing "empty path"
    (let [err (e/->error {})]
      (is (= err (e/lookup-errors err [])))))

  (testing "empty errors"
    (let [err (e/->error {})]
      (is (= nil (e/lookup-errors err [:field])))
      (is (= nil (e/lookup-errors err [:field 0 :other-field])))))

  (testing "nested errors"
    (let [entity-err (e/->error {:subfield '("whatever")})
          err (e/->error {:field entity-err})]
      (is (= entity-err (e/lookup-errors err [:field])))
      (is (= '("whatever") (e/lookup-errors err [:field :subfield])))))

  (testing "collection errors"
    (let [entity-err (e/->error {:subfield '("whatever")})
          coll-err (e/->error '("base error") [entity-err])
          err (e/->error {:field coll-err})]
      (is (= coll-err (e/lookup-errors err [:field])))
      (is (= entity-err (e/lookup-errors err [:field 0])))
      (is (= '("whatever") (e/lookup-errors err [:field 0 :subfield]))))))

(deftest field-error
  (let [err (e/->error-on-field :field "foo")]
    (is (= [() {:field '("foo")}] err))))

(deftest base-errors
  (testing "no errors"
    (let [err (e/->error {})]
      (is (= () (e/base-errors err)))))

  (testing "has errors"
    (let [err (e/->error '("error") {})]
      (is (= '("error") (e/base-errors err))))))

(deftest field-errors
  (testing "no errors"
    (let [err (e/->error {})]
      (is (= {} (e/field-errors err)))
      (is (= nil (e/field-errors err :field))))) ;; think about that

  (testing "has errors"
    (let [err (e/->error {:field '("whatever")})]
      (is (= {:field '("whatever")} (e/field-errors err)))
      (is (= '("whatever") (e/field-errors err :field))))))

(deftest valid?
  (testing "no error"
    (let [err (e/->error () {})]
      (is (e/valid? err))
      (is (e/valid? err :field))))

  (testing "no field error"
    (let [err (e/->error () {:field ()})]
      (is (e/valid? err))
      (is (e/valid? err :field))))

  (testing "base error"
    (let [err (e/->error '("error") {})]
      (is (not (e/valid? err)))
      (is (e/valid? err :field))))

  (testing "field error"
    (let [err (e/->error {:field '("error")})]
      (is (not (e/valid? err)))
      (is (not (e/valid? err :field)))
      (is (e/valid? err :other-field)))))

(deftest valid-base?
  (testing "no error"
    (is (e/valid-base?* '())))

  (testing "with error"
    (is (not (e/valid-base?* '("error"))))))

(deftest valid-fields?
  (testing "no error"
    (is (e/valid-fields?* {}))
    (is (e/valid-fields?* {:field ()}))
    (is (e/valid-fields?* {:field (e/->error () {:sub-field ()})}))
    (is (e/valid-fields?* {:field (e/->error () [(e/->error () {:coll-field ()})])})))

  (testing "with error"
    (is (not (e/valid-fields?* {:field '("error")})))
    (is (not (e/valid-fields?* {:field (e/->error '("error") {})})))
    (is (not (e/valid-fields?* {:field (e/->error () {:sub-field '("error")})})))
    (is (not (e/valid-fields?* {:field (e/->error '("error") [(e/->error () {})])}))))
    (is (not (e/valid-fields?* {:field (e/->error () [(e/->error '("error") {})])})))
    (is (not (e/valid-fields?* {:field (e/->error () [(e/->error () {:coll-field '("error")})])}))))



; (deftest merge-errors
;   (testing "base errors"
;     (let [err1 (e/error (e/be "Base error 1"))
;           err2 (e/error (e/be "Base error 2"))
;           res (e/merge-errors err1 err2)]

;       (is (= () (e/field-errors res)))
;       (is (= (e/be "Base error 1" "Base error 2") (e/base-errors res)))))

;   (testing "no errors"
;     (let [err1 (e/no-error)
;           err2 (e/no-error)]

;       (is (= (e/no-error) (e/merge-errors err1 err2)))))

;   (testing "base and no-error"
;     (let [err1 (e/no-error)
;           err2 (e/error (e/be "Base"))]
;       (is (= err2 (e/merge-errors err1 err2)))
;       (is (= err2 (e/merge-errors err2 err1)))))

;   (testing "errors on same fields"
;     (let [err1 (e/error (e/fe :password "Error 1"))
;           err2 (e/error (e/fe :password "Error 2"))
;           res (e/merge-errors err1 err2)]

;       (is (= [(e/fe :password (e/msg "Error 1" "Error 2"))] (e/field-errors res)))
;       (is (empty? (e/base-errors res)))))

;   (testing "field and no-error"
;     (let [err1 (e/error (e/fe :password "Error 1"))
;           err2 (e/no-error)]

;       (is (= err1 (e/merge-errors err1 err2)))
;       (is (= err1 (e/merge-errors err2 err1)))))

;   (testing "different fields"
;     (let [err1 (e/error (e/fe :confirmation "Error 1"))
;           err2 (e/error (e/fe :password "Error 2"))
;           res (e/merge-errors err1 err2)]

;       (is (= [(e/fe :confirmation (e/msg "Error 1")
;                        :password (e/msg "Error 2"))] (e/field-errors res)))
;       (is (empty? (e/base-errors res)))))

;   (testing "base and field error"
;     (let [err1 (e/err (e/msg "Base") [])
;           err2 (e/err [{:field (e/msg "Error 1")}])]
;       (is (= (e/err (e/msg "Base") [{:field (e/msg "Error 1")}]) (e/merge-errors err1 err2)))))

;   (testing "errors on same nested fields"
;     (let [err1 (e/err [{:field (e/msg "Error 1")
;                           :form (e/err [{:field (e/msg "Error 2")
;                                            :form (e/err [{:field (e/msg "Error 3")}])}])}])
;           err2 (e/err [{:field (e/msg "Error 4")
;                           :form (e/err [{:field (e/msg "Error 5")
;                                            :form (e/err [{:field (e/msg "Error 6")}])}])}])]

;       (is (= (e/err [{:field (e/msg "Error 1" "Error 4")
;                         :form (e/err [{:field (e/msg "Error 2" "Error 5")
;                                          :form (e/err [{:field (e/msg "Error 3" "Error 6")}])}])}])
;             (e/merge-errors err1 err2)))))

;   (testing "errors on different nested fields"
;     (let [err1 (e/err [{:field (e/msg "Error 1")
;                           :form (e/err [{:field (e/msg "Error 2")
;                                            :form (e/err [{:field (e/msg "Error 3")}])}])}])
;           err2 (e/err [{:field (e/msg "Error 4")
;                           :form (e/err [{:other_field (e/msg "Error 5")
;                                            :other_form (e/err [{:field (e/msg "Error 6")}])}])}])]

;       (is (= (e/err [{:field (e/msg "Error 1" "Error 4")
;                         :form (e/err [{:field (e/msg "Error 2")
;                                          :form (e/err [{:field (e/msg "Error 3")}])
;                                          :other_field (e/msg "Error 5")
;                                          :other_form (e/err [{:field (e/msg "Error 6")}])}])}])
;             (e/merge-errors err1 err2)))))

;   (testing "nested fields and no error"
;     (let [err1 (e/err [{:field (e/msg "Error 1")
;                           :form (e/err [{:field (e/msg "Error 2")
;                                            :form (e/err [{:field (e/msg "Error 3")}])}])}])
;           err2 (e/err [{:form (e/no-error)}])]


;       (is (= err1 (e/merge-errors err1 err2)))
;       (is (= err1 (e/merge-errors err2 err1)))))

;   (testing "errors of has many associations"
;     (let [err1 (e/error
;                   (e/fe :trx
;                     (e/error
;                       (e/error
;                         (e/fe :credit (e/msg "Missing")))
;                       (e/no-error)
;                       (e/error
;                         (e/fe :debit (e/msg "Unknown"))))))

;           err2 (e/err [{:trx (e/err [(e/no-error)
;                                          (e/err [{:amount (e/msg "Tooo much")}])
;                                          (e/err [{:debit (e/msg "Not allowed")}])])}])]

;       (is (= (e/err [{:trx (e/err [(e/err [{:credit (e/msg "Missing")}])
;                                        (e/err [{:amount (e/msg "Tooo much")}])
;                                        (e/err [{:debit (e/msg "Unknown" "Not allowed")}])])}])
;             (e/merge-errors err1 err2)))))

;   (testing "errors of a has many association and no error"
;     (let [err1 (e/err [{:trx (e/err [(e/err [{:credit (e/msg "Missing")}])
;                                          (e/no-error)
;                                          (e/err [{:debit (e/msg "Unknown")}])])}])
;           err2 (e/err [{:trx (e/err [])}])]

;       (is (= err1 (e/merge-errors err1 err2)))))

;   )
