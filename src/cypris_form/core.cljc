(ns cypris-form.core
  (:refer-clojure :exclude [name])
  (:require [cypris-form.fdef :as f]
            [cypris-form.error :as e]
            [cypris-form.input :as i]
            [cypris-form.contract :as c]
            [cypris-form.logging :as log]
            [cypris-form.util :as u]))

(def ^:dynamic *atom-fn* atom)
(def ^:dynamic *error-key* :_errors)
(def ^:dynamic *input-key* :_current)
(def ^:dynamic *original-key* :_original)
(def ^:dynamic *not-found-value* :not-found)

(declare ->Form cursor input* errors* set-field-error!* set-errors!* set-value!* update-value!* contains-value?* remove-value!* new-field subform collform forms* changed?* reset-changes!* validate!* submit!*)

;; constructor

(defn new-state [values]
  (*atom-fn* {*input-key* values *original-key* values}))

(defn new-form [fdef state action & contracts]
  (let [contract (f/contract fdef)
        contract (reduce c/merge-contracts contract contracts)
        validation (c/gen-vld contract)
        coercion (f/coercion fdef)
        cursor (cursor state [])]
    (->Form fdef contract validation coercion action cursor)))

(defn form-with-state [udef & args]
  (let [fdef (f/->fdef udef)]
    (apply new-form fdef args)))

(defn form [udef values & args]
  (let [fdef (f/->fdef udef)
        defaults (f/default-values fdef)
        values-with-defaults (i/input values defaults *not-found-value*)
        state (new-state values-with-defaults)]
    (apply new-form fdef state args)))

;; api

(defprotocol IHasChanged
  (changed? [this]))

(defprotocol IPath
  (path [this]))

(defprotocol IBaseErrors
  (base-errors [this])
  (valid-base? [this]))

(defprotocol IInput
  (set-input! [f vs])
  (input [f]))

(defprotocol IValidate
  (validate! [f])
  (valid? [f])
  (errors [fd])
  (set-error! [fd vs]))

(defprotocol IField
  (name [fd])
  (id [fd])
  (value [fd])
  (set-value! [fd v]))

(defprotocol IForm
  (fields [f])
  (field [f fd] [f fd1 fd2] [f fd1 fd2 fd3])
  (submit! [this] [this options]))

(defprotocol IColl
  (size [c])
  (forms [c])
  (add! [c][c v])
  (has? [c v])
  (remove-by-index! [c i])
  (remove! [c f]))

(deftype Field [fdname fdspec validation state-cursor]
  #?@(:clj (clojure.lang.ILookup
            (valAt [fd k not-found] (get fdspec k not-found))
            (valAt [fd k] (k fdspec)))
      :cljs (ILookup
            (-lookup [fd k not-found] (get fdspec k not-found))
            (-lookup [fd k] (k fdspec))))

  IField
  (name [_]
    (clojure.core/name fdname))

  (id [this]
    (u/join (path this) "_"))

  (value [_]
    (input* state-cursor))

  (set-value! [this v]
    (set-value!* state-cursor v))

  IValidate
  (valid? [_]
    (e/valid? (errors* state-cursor)))

  ;; Executes single field constraints only
  (validate! [_]
    (validate!* state-cursor validation))

  (errors [_]
    (errors* state-cursor))

  (set-error! [_ msg]
    (set-field-error!* state-cursor msg))

  IPath
  (path [_]
    (:path state-cursor))

  IHasChanged
  (changed? [_]
    (changed?* state-cursor)))

(deftype Form [fdef contract validation coercion action state-cursor]
  IForm
  (fields [_]
    (f/fields fdef))

  (field [_ fd]
    (let [[type fspec] (f/fd fdef fd)
          c (cursor state-cursor [fd])
          ct (c/contract contract fd)]
      (condp = type
        :field
        (new-field fd fspec ct c)

        :form
        (subform fspec ct action c)

        :collform
        (collform fspec ct action c))))

  (field [this fd1 fd2]
    (-> this (field fd1) (field fd2)))

  (field [this fd1 fd2 fd3]
    (-> this (field fd1 fd2) (field fd3)))

  (submit! [this]
    (submit! this {}))

  (submit! [this options]
    (let [{:keys [validation coercion action params]
           :or {validation validation
                coercion coercion
                action action}} options
          action (fn [vs]
                   (if params
                     (action {:values vs :form this :params params})
                     (action {:values vs :form this})))]
      (submit!* state-cursor validation coercion action)))

  IInput
  (set-input! [_ vs]
    (set-value!* state-cursor vs))

  (input [_]
    (input* state-cursor))

  IBaseErrors
  (base-errors [_]
    (e/base-errors (errors* state-cursor)))

  (valid-base? [_]
    (e/valid-base? (errors* state-cursor)))

  IValidate
  (valid? [_]
    (e/valid? (errors* state-cursor)))

  (validate! [_]
    (validate!* state-cursor validation))

  (errors [_]
    (errors* state-cursor))

  (set-error! [_ errs]
    (set-errors!* state-cursor errs))

  IPath
  (path [_] (:path state-cursor))

  IHasChanged
  (changed? [_]
    (changed?* state-cursor)))

(deftype Coll [f fdef validation coercion state-cursor]
  #?@(:clj (clojure.lang.ISeq
            (first [this] (-> this forms first))
            (next [this] (-> this forms next))
            (more [this] (-> this forms .more)))

      :cljs (ISeq
             (-first [this] (-> this forms .-first))
             (-rest [this] (-> this forms .-rest))
             ISeqable
             (-seq [this] (-> this forms seq))))

  IColl
  (forms [this]
    (take (size this) f))

  (size [_]
    (let [input (input* state-cursor)]
      (count input)))

  (add! [_]
    (let [new-values (f/default-values fdef)]
      (update-value!* state-cursor conj new-values)))

  (add! [_ v]
    (let [new-values (merge (f/default-values fdef) v)]
      (update-value!* state-cursor conj new-values)))

  (has? [_ v]
    (contains-value?* state-cursor v))

  (remove-by-index! [_ i]
    (update-value!* state-cursor u/vec-remove i))

  (remove! [_ f]
    (remove-value!* (.-state-cursor f)))

  IValidate
  (valid? [_]
    (e/valid? (errors* state-cursor)))

  (validate! [_]
    (validate!* state-cursor validation))

  (errors [_]
    (errors* state-cursor))

  IInput
  (input [this]
    (map input (forms this)))

  IBaseErrors
  (base-errors [_]
    (e/base-errors (errors* state-cursor)))

  (valid-base? [_]
    (e/valid-base? (errors* state-cursor)))

  IPath
  (path [_] (:path state-cursor)))

;; impl

(defn new-field [fdname fdspec constraint state-cursor]
  (let [constraint (c/constraint-mapping constraint)]
    (->Field fdname fdspec constraint state-cursor)))

(defn subform [fdef ct action cursor]
  (let [validation (c/gen-vld ct)
        coercion (f/coercion fdef)]
    (->Form fdef ct validation coercion action cursor)))

(defn collform [[fdef _] cct action cursor]
  (let [ct (c/contract cct)
        forms (forms* fdef ct action cursor)
        validation (c/gen-coll-vld cct)
        coercion (f/coercion fdef)]
    (->Coll forms fdef validation coercion cursor)))

(defn cursor? [x]
  (map? x))

(defn cursor [x path]
  (if (cursor? x)
    {:state (:state x) :path (concat (:path x) path)}
    {:state x :path path}))

(defn forms*
  ([fdef contract action cursor]
    (forms* fdef contract action cursor 0))

  ([fdef contract action c index]
    (lazy-seq
      (let [new-cursor (cursor c [index])]
        (cons (subform fdef contract action new-cursor)
          (forms* fdef contract action c (inc index)))))))

(defn original* [{:keys [state path]}]
  (let [original (get @state *original-key*)]
    (get-in original path)))

(defn input* [{:keys [state path]}]
  (let [input (get @state *input-key*)]
    (get-in input path)))

(defn errors* [{:keys [state path]}]
  (e/lookup-errors (*error-key* @state) path))

(defn set-value!* [{:keys [state path]} v]
  (let [input-path (cons *input-key* path)]
    (swap! state assoc-in input-path v)))

(defn update-value!* [{:keys [state path]} & fs]
  (let [input-path (cons *input-key* path)]
    (apply swap! state update-in input-path fs)))

(defn contains-value?* [c v]
  (let [input (input* c)]
    (some #{v} input)))

(defn remove-value!* [{:keys [state path]}]
  (let [input-path (cons *input-key* path)]
    (swap! state u/dissoc-in input-path)))

(defn set-errors!* [{:keys [state path]} errs]
  (let [nested-err (e/error-tree errs path)]
    (swap! state assoc *error-key* nested-err)))

(defn set-field-error!* [{:keys [path state]} msg]
  (let [fd (last path)
        parent-path (butlast path)
        err (e/->error-on-field fd msg)
        nested-err (e/error-tree err parent-path)]
    (swap! state assoc *error-key* nested-err)))

(defn changed?* [c]
  (let [original (original* c)
        current (input* c)]
    (not= current original)))

(defn reset-changes!* [{:keys [state path] :as c}]
  (let [current (input* c)
        original-path (cons *original-key* path)]
    (swap! state assoc-in original-path current)))

(defn validate!* [c validation]
  (let [values (input* c)
        err (validation values)]
    (set-errors!* c err)))

(defn submit!* [c validation coercion action]
  (let [values (input* c)
        err (validation values)]
    (log/debug "Submit form with input" values "and errors" err)
    (set-errors!* c err)
    (when (e/valid? err)
      (let [cvalues (coercion values)]
        (do
          (action cvalues)
          (reset-changes!* c))))))
