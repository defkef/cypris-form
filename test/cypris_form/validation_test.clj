(ns cypris-form.validation-test
  (:require [clojure.test :refer :all]
            [cypris-form.validation :as v]))

(v/defvalidation password-confirmation (fn [[pass confirm]] (= pass confirm)) "" "Password does not match")

(deftest required
  (is (= () (v/required "value")))
  (is (= '("required") (v/required ""))))

(deftest confirmation
  (is (= '() (password-confirmation ["value" "value"])))
  (is (= '(("") ("Password does not match")) (password-confirmation ["value" ""]))))
