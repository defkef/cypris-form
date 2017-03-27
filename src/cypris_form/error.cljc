(ns cypris-form.error
  (:require [cypris-form.logging :as log]
            #?(:clj [clojure.spec :as s]
               :cljs [cljs.spec :as s])))

;; data structure

(s/def ::msg (s/coll-of (complement nil?)))
(s/def ::be (s/coll-of (s/or :string string? :integer integer?)))
(s/def ::name keyword?)
(s/def ::fe (s/or :field (s/map-of ::name (s/or :msg ::msg :err ::err))
                  :collerr (s/coll-of ::err)))
(s/def ::err (s/tuple ::be ::fe))

;; type checks

(def base-error? (partial s/valid? ::be))
(def field-error? (partial s/valid? ::fe))
(def error? (partial s/valid? ::err))
(def error-msg? (partial s/valid? ::msg))

;; constructor

(defn ->error
  ([fe]
    (->error () fe))
  ([be fe]
   {:pre [(or (base-error? be)
              (log/error be))
          (or (field-error? fe)
              (log/error fe))]
    :post (error? %)}
    [be fe]))

(defn ->error-on-field [name & msgs]
  (->error {name msgs}))

;; set-errors is not form agnostic
;; as long as errors cannot be merge that should not be a problem
(defn error-tree [err path]
  {:post [(or (error? %)
              (error-msg? %))]}
  (if (empty? path)
    err
    (let [[fd & more] path]
      ['() {fd (error-tree err more)}])))

(defn lookup-errors [err path]
  (if (empty? path)
    err
    (let [[fd & more] path
          field-errors (second err)
          fe (get field-errors fd)]
      (recur fe more))))

(defn base-errors [err]
  (first err))

(defn field-errors
  ([err]
    (second err))
  ([err fd]
    (fd (field-errors err))))

(declare valid-base? valid-fields?)

(defn valid?
  ([err]
   (or (empty? err)
       (and (valid-base? err)
            (valid-fields? err))))

  ([err fd]
    (valid? (field-errors err fd))))

(defn valid-base?* [be]
  (empty? be))

(defn valid-base? [err]
  (valid-base?* (base-errors err)))

(defn valid-fields?* [fe]
  (or (empty? fe)
      (if (vector? fe)
        (every? valid? fe)
        (every? valid? (vals fe)))))

(defn valid-fields? [err]
  (valid-fields?* (field-errors err)))

; (declare merge-errors merge-error-msgs merge-base-errors merge-field-errors)

; (defn merge-errors [a b]
;   {:pre [(err-msgs? (base-errors a))
;          (err-msgs? (base-errors b))
;          (= (type (field-errors a)) (type (field-errors b)))]}

;   (let [be (merge-base-errors a b)
;         fe-a (field-errors a)
;         fe-b (field-errors b)]
;     (cond
;       (empty? fe-a) (err be fe-b)
;       (empty? fe-b) (err be fe-a)
;       :else (err be (mapv merge-field-errors fe-a fe-b)))))

; (defn merge-error-msgs [a b]
;   (into b a))

; (defn merge-base-errors [a b]
;   (merge-error-msgs (base-errors a) (base-errors b)))

; (defn merge-field-errors [a b]
;   (if (err? a)
;     (merge-errors a b)
;     (merge-with (fn [x y]
;                   (if (err? y)
;                     (merge-errors x y)
;                     (merge-error-msgs x y)))
;       a b)))



