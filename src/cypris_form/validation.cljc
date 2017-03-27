(ns cypris-form.validation
  #?(:cljs (:require-macros [cypris-form.validation :refer [defvalidation]])))

(defmacro defvalidation [name expr & msg]
  `(defn ~name [value#]
    (if (~expr value#)
      ()
      (cond
        (empty? (list ~@msg))
        (list "invalid")

        (= 1 (count (list ~@msg)))
        (list ~@msg)

        :else
        (map list (list ~@msg))))))

(defvalidation required (comp not empty?) "required")
