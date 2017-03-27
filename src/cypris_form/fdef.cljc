(ns cypris-form.fdef
  (:require [cypris-form.util :as u]
            #?@(:cljs
                 ([cljs.core.match :refer-macros [match]]
                  [cljs.spec :as s])
                 :clj
                 ([clojure.core.match :refer [match]]
                  [clojure.spec :as s]))))

;; data structure

(s/def ::name keyword?)
(s/def ::names (s/coll-of ::name :min-count 1))
(s/def ::field map?)
(s/def ::opts map?)
(s/def ::vld map?)
(s/def ::fspec (s/cat :name ::name
                      :spec (s/or :field ::field
                                  :form ::fdef
                                  :coll ::coll)))
(s/def ::vspec (s/cat :name ::names :spec ::vld))
(s/def ::coll (s/or
               :w-opt (s/tuple ::fdef ::opts)
               :wo-opt (s/tuple ::fdef)))
(s/def ::fdef (s/+ (s/alt :f ::fspec :v ::vspec)))

;; api

(defn ->fdef [x]
  (let [fdef (s/conform ::fdef x)]
    (if (= fdef #?(:clj :clojure.spec/invalid
                   :cljs :cljs.spec/invalid))
      (u/throw-error (s/explain-str ::fdef x))
      fdef)))

(defn valid? [x]
  (s/valid? ::fdef x))

(defn fspec [fdef]
  (->> fdef
       (filter (fn [[t _]] (= t :f)))
       (map (fn [[_ f]] [(:name f) (:spec f)]))
       (into (array-map))))

(defn fspec-tree [ff fdef]
  (let [self (partial fspec-tree ff)]
    (->> fdef
         (filter (fn [[t _]] (= t :f)))
         (map (fn [[_ {:keys [name spec]}]]
                (match spec
                       [:field field] [name (ff field)]
                       [:form form] [name (self form)]
                       [:coll [:wo-opt [coll]]] [name [(self coll)]]
                       [:coll [:w-opt [coll opt]]] [name [(self coll) opt]])))
         (into (array-map)))))

;; forms

(defn fields [fdef]
  (let [fspec (fspec fdef)]
    (keys fspec)))

(defn fd [fdef fd]
  (let [fd (-> fdef fspec fd)]
    (match fd
           [:field field] [:field field]
           [:form form] [:form form]
           [:coll [:wo-opt coll]] [:collform coll]
           [:coll [:w-opt coll]] [:collform coll])))

;; default values

(defn default-value-opt [fdspec]
  (cond
    (contains? fdspec :default-value) (:default-value fdspec)
    (= (:type fdspec) :checkbox) #{}
    (= (:type fdspec) :boolean) false
    (= (:type fdspec) :select) (or (ffirst (:options fdspec)) "")
    :else nil))

(def default-values (partial fspec-tree default-value-opt))

;; coercions

(defn coercion-opt [fdspec]
  (cond
    (contains? fdspec :coerce) (:coerce fdspec)
    :else identity))

(defn traverse-and-apply [fs vs]
  (into {}
    (for [[name value] vs
          :let [f (get fs name identity)]]
      (cond
        (map? value)
        [name (traverse-and-apply f value)]

        (vector? value)
        [name (mapv (partial traverse-and-apply (first f)) value)]

        :else
        [name (f value)]))))

(defn coercion [fdef]
  (let [fs (fspec-tree coercion-opt fdef)]
    (fn [vs]
      (traverse-and-apply fs vs))))

;; contract

(declare constraints)

(defn contract [fdef]
  [() (constraints fdef)])

(defn coll-contract [[fdef opts]]
  (let [vld (get opts :validate ())
        base (if (seq? vld) vld (list vld))]
    [base (contract fdef)]))

(defn constraints [fdef]
  (->> fdef
       (map (fn [[t {:keys [name spec]}]]
              (match [t spec]
                     [:v {:validate f}] [name f]
                     [:f [:field {:validate f}]] [name f]
                     [:f [:form form]] [name (contract form)]
                     [:f [:coll [:wo-opt coll]]] [name (coll-contract coll)]
                     [:f [:coll [:w-opt coll]]] [name (coll-contract coll)]
                     [_ _] [])))
       (filter seq)
       (into {})))
