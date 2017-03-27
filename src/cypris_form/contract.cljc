(ns cypris-form.contract
  (:require [cypris-form.error :as e]
            [cypris-form.logging :as log]
            [cypris-form.util :as u]
            #?(:clj [clojure.spec :as s]
               :cljs [cljs.spec :as s])))

;; Data structure

;; CT - contract
;; CCT - collection contract
;; BC - base constraint
;; FC - form constraint
;; C - field or input constraints
;; FD - field

;; CT = [ BC FC ]
;; CCT = [ BC [CT] ]
;; BC = (C*)
;; FC = {FD => C}
;; C = fn :: value -> (str)
;; C = (fn)

;; contract

;; ['() {}]

;; Collection contract

;; ['() ['() {}]]

(s/def ::vld fn?)
(s/def ::c (s/or :multi (s/coll-of ::vld) :single ::vld))
(s/def ::name (s/or :single keyword? :multi (s/coll-of keyword?)))
(s/def ::fc (s/map-of ::name (s/or :c ::c :ct ::ct :cct ::cct)))
(s/def ::bc (s/coll-of ::vld))
(s/def ::cct (s/tuple ::bc ::ct))
(s/def ::ct (s/tuple ::bc ::fc))

;; type checks

(defn ct? [x]
  (s/valid? ::ct x))

(defn cct? [x]
  (s/valid? ::cct x))

;; contract

(defn contract
  ([cct]
   {:pre [(cct? cct)]}
   (last cct))
  ([ct fd]
   {:pre [(ct? ct)]}
   (let [[_ fc] ct]
     (get fc fd))))

;; validation

(declare gen-bc gen-fc all)

(defn no-validation []
  (fn [input]
    (e/->error '() {})))

(defn gen-vld [ct]
  {:pre [(or (ct? ct)
             (log/error ct))]}
  (let [bc (gen-bc (first ct))
        fc (gen-fc (second ct))]
    (with-meta
      (fn [input]
        {:post [(e/error? %)]}
        (e/->error (bc input) (fc input)))
      {:constraint :ct})))

(defn gen-coll-vld [cct]
  {:pre [(cct? cct)]}
  (let [bc (gen-bc (first cct))
        vld (gen-vld (second cct))]
    (with-meta
      (fn [input]
        {:post [(e/error? %)]}
        (if (vector? input)
          (e/->error (bc input) (mapv #(vld %) input))
          (e/->error '("not a collection") [(e/->error {})])))
      {:constraint :cct})))

(defn gen-bc [bc]
  (fn [input]
    (mapcat #(% input) bc)))

(declare constraint-fns validation-fns apply-field-constraints constraint-mapping value-fns value-fn flatten-composite-constraint-result no-constraint)

(defn gen-fc [fcs]
  (let [value-fns (value-fns value-fn (keys fcs))
        constraint-fns (constraint-fns constraint-mapping fcs)
        validation-fns (validation-fns constraint-fns value-fns)]
    (fn [input]
      (flatten-composite-constraint-result
        (apply-field-constraints validation-fns input)))))

(defn value-fn [fd]
  (if (vector? fd)
    (apply juxt fd)
    (comp fd)))

(defn value-fns [f fds]
  (zipmap fds (map f fds)))

(defn constraint-fns [constraint-mapping fc]
  (into {} (for [[fd c] fc] [fd (constraint-mapping c)])))

(defn constraint-mapping [c]
  (cond
    (seq? c) (apply all c)
    (fn? c) c
    (ct? c) (gen-vld c)
    (cct? c) (gen-coll-vld c)
    (nil? c) no-constraint
    :else
    (u/throw-error "unhandled constraint mapping" c)))

(defn expand-composite-field-constraint-result [fcr-vec]
  (apply merge-with concat
    (map (fn [[fds r]]
           (if (empty? r)
            (zipmap fds (repeat r))
            (zipmap fds r)))
      fcr-vec)))

(defn flatten-composite-constraint-result [fcr]
  (let [{flat false nested true} (group-by #(vector? (first %)) (seq fcr))
        fcr1 (into {} flat)
        fcr2 (expand-composite-field-constraint-result nested)]
    (merge-with concat fcr1 fcr2)))

(defn comb-constraint-value-fn [c v] (comp c v))

(defn validation-fns [cs vs]
  (merge-with comb-constraint-value-fn cs vs))

(defn apply-field-constraints [fcs input]
  (into {} (for [[fd fc] fcs] [fd (fc input)])))

(def no-constraint
  (with-meta
    (fn [& _] ())
    {:constraint :nothing}))

(defn all [& cs]
  (with-meta
    (fn [value]
      (reduce concat
        (take 1
          (filter (complement empty?)
            (map #(% value) cs))))) {:constraint :all}))

(declare merge-bcs merge-fcs-strategy merge-contracts-mapping present-contract? empty-contract?)

(defn merge-contracts [[bc1 fc1] [bc2 fc2]]
  (let [merge-fcs (merge-contracts-mapping fc1 fc2)]
    [(merge-bcs bc1 bc2) (merge-fcs fc1 fc2)]))

(defn merge-coll-contracts [ct1 ct2]
  (mapv merge-contracts ct1 ct2))

(defn merge-bcs [a b]
  (concat a b))

(defn merge-fcs [strategy a b]
  (merge-with strategy a b))

(defn merge-contracts-mapping [a b]
  (cond
    (and (map? a) (map? b)) (with-meta (partial merge-fcs merge-fcs-strategy) {:merger :fc})
    (and (ct? a) (ct? b)) (with-meta merge-contracts {:merger :ct})
    (and (cct? a) (cct? b)) (with-meta merge-coll-contracts {:merger :cct})
    (empty-contract? a) (with-meta (fn [_ b] b) {:merger :last})
    (empty-contract? b) (with-meta (fn [a _] a) {:merger :first})
    :else
    (u/throw-error "Cannot merge contracts " a " and " b)))

(defn merge-fcs-strategy [a b]
  (condp = [(seq? a) (seq? b)]
    [true true] (concat a b)
    [false true] (concat [a] b)
    [true false] (concat a [b])
    [false false] (list a b)))

(defn empty-contract? [[bc fc]]
  (and (empty? bc) (empty? fc)))

; (defn and [& cs]
;   (fn [value]
;     (reduce e/merge-error-msgs () (map #(% value) cs))))

; (defn or [& cs]
;   (fn [value]
;     (let [r (map #(% value) cs)]
;       (if (not-any? e/valid? r)
;         (reduce e/merge-error-msgs () r)
;         (e/ok)))))



