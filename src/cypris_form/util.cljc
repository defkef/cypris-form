(ns cypris-form.util
  (:require [cypris-form.logging :as log]
            [clojure.string]))

(defn throw-error [& msgs]
  #?(:clj  (throw (Exception. (apply str msgs)))
     :cljs (apply log/error msgs)))

(defn vec-remove [coll pos]
  (let [coll (vec coll)]
    (vec (concat (subvec coll 0 pos) (subvec coll (inc pos))))))

(defn dissoc-in [m ks]
  (let [k (last ks)
        ks (butlast ks)]

    (update-in m ks (fn [e]
      (cond
        (vector? e) (vec-remove e k)
        :else (dissoc e k))
      ))))

(defn ->str [x]
  (cond
    (keyword? x) (name x)
    :else (str x)))

(defn join [coll sep]
  (let [str-coll (map ->str coll)]
    (clojure.string/join sep str-coll)))
