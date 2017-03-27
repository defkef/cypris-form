# cypris-form

A Cljs library to provide a decent interface to forms including state and error handling. A reagent-bootstrap binding can be found in examples/reagent-bootstrap.cljs

## Overview

### Definition

```clojure
(def formdef [:fieldA {:type :text :coerce (fn [value] ...) :validate (fn [value] ...) :default-value "foo"}
              :fieldB {:type :text :my-prop 'value} ;; add custom props as you need
              [:fieldA :fieldB] {:validate (fn [valueA valueB] ...)} ;; for optional multifield constraints only
              :nestedC [
                :fieldCA {...}
                :fieldCB [
                  :fieldCB1 {...}
                  ...
                ]
              ]
              :collD [[:fieldDA {}
                       :fieldDB {}
                       ...] 
                       {:validate (fn [coll-values] ...)}]
              ...
              ])
```

Then you can instantiate a form with the following interface

### Form

```clojure
(:require [cypris-form.core :as cf])
...

(defn action [{:keys [values form]}]
  ;; values - submitted values
  ;; form - submitting form
  )
  
(def values {})

(def form (cf/form formdef values action)) ;; see Usage for alternativ state

(cf/fields form) ;; => [:fieldA :fieldB :nestedC :collD]
(cf/field form :fieldA) ;; => a Field, see interface below
(cf/field form :nestedC) ;; => a Form as well
(cf/field form :collD) ;; => a CollForm, see interface below
(cf/submit! form) ;; call action after validation and coercion
(cf/submit! form {:validation (fn [input] ...)
                  :action (fn [{:keys [values form]}] ...)
                  :coercion (fn [input] ...)}) ;; for custom validation, coercion and action
(cf/input form) ;; => {:fieldA "value" 
                ;;     :fieldB "value" 
                ;;     :nestedC {:fieldCA "value" :fieldCB {:fieldCB1 "value"}} 
                ;;     :collD [{:fieldDA "value" :fieldDB "value"} ...]}
                
(cf/validate! form) ;; call all validations and set error
(ct/set-error! form error) ;; manually set error, see Usage for data type
(cf/valid? form) ;; => boolean 
(cf/path form) ;; path to the element in the form tree
(cf/changed? form) ;; => boolean 

```

### Field

```clojure
(def field (cf/field form :nestedC :fieldCA))

(:any-prop-defined field) ;; => it's value
(cf/name field) ;; => "fieldCA"
(cf/id field) ;; => "nestedC_fieldCA"
(cf/value field) ;; => input value
(cf/set-value! field "my value")
(cf/set-error! field "wrong value") ;; manually set errors
(cf/validate! field) ;; call functions defined in :validate and set errors
(cf/valid? field) ;; => boolean
(cf/path field) ;; => [:nestedC :fieldCA]
(cf/changed? field) ;; => boolean
```

### CollForm

```clojure
(def coll (cf/field form :collD))

(seq coll) ;; => seq of Forms
(cf/forms coll) ;; => seq of Forms
(cf/size coll) ;; => collection size
(cf/add! coll) ;; add a new Form 
(cf/add! coll {:fieldDA "value"}) ;; add a new Form with values
(cf/has? coll {:fieldDA "value"}) ;; => boolean
(cf/remove-by-index! coll 0) ;; remove first Form
(cf/remove! coll form) ;; remove Form directly
(cf/validate! coll) ;; call all validation and set errors
(cf/valid? coll) ;; => boolean
(cf/path coll) ;; => ["collD"]
(cf/changed? coll) ;; => boolean
```

## Install

[![Clojars Project](http://clojars.org/reagent-forms/latest-version.svg)](http://clojars.org/reagent-forms)


## Usage

### Custom state

A plain atom

```clojure

;; Manually set state atom, but default values will not be set

(def state-atom (cf/new-state {}))
(def state-atom (cf/new-state {:fieldA ...}))

(cf/form-with-state formdef state-atom action) ;; => Form

```

A reagent atom

```clojure
(set! cf/*atom-fn* reagent/atom)

;; or

(binding [cf/*atom-fn* reagent/atom]
  (cf/form formdef values action))

```

### Submit Action

A function that accepts the input values and submitting form as parameter

```clojure
(defn action [{:keys [values form]}] ...)
```

### Validation

When defining the form you can set **single field constraints**:

```clojure
(def formdef [:field {:validate vld}])
(def formdef [:field {:validate (list vld1 vld2)}]) ;; run both
```

Define the validation function or use the macro defined in cypris-form.validation

```clojure
(defn vld [value]
  (if (pred value)
    () ;; empty list - ok 
    '("Wrong") ;; some error messages - invalid
    ))
    
(cypris-form.validation/defvalidation vld2 pred "Wrong")
```

**multifield constraints**:

```clojure
(def formdef [:fieldA {}
              :fieldB {}
              [:fieldA :fieldB] {:validate vld}])

(defn vld [valueA valueB]
  (if (pred ...)
    (() ()) ;; empty list - ok 
    '(("Wrong A") ("Wrong B")) ;; some error messages - invalid
    ))
```

**collection constraints**:

```clojure
(def formdef [:coll [[:fieldDA {}
                      :fieldDB {}
                      ...] 
                      {:validate vld}]])

(defn vld [coll]
  (if (pred coll)
    () ;; empty list - ok 
    '("Must not be empty") ;; some error messages - invalid
    ))
```

### Coercion

Will get called AFTER validation:

```clojure
(def formdef [:fieldA {:coerce coerce}]

(defn coerce [valueA]
;; return modified value
)
```

### Default values

```clojure
(def formdef [:fieldA {:type "text" :default-value "default"}]

(defn coerce [valueA]
;; return modified value
)
```

**Special cases**

When no default value is set the following applies for fields of a certain :type

```clojure
(cond
  (contains? props :default-value) (:default-value props)
  (= (:type props) :checkbox) #{}
  (= (:type props) :boolean) false
  (= (:type props) :select) (or (ffirst (:options props)) "") ;; :options [["key" "value"][..]]
  :else nil)
```

### Errors

To manually set form errors, see error.cljc for spec

```clojure

(cf/set-error! form [("base error") {:field ("field error")}])

```

### Using with Reagent and Bootstrap

Copy examples/reagent-bootstrap.cljs into your project and edit as appropriate

Example:

```clojure
(:require [reagent.core :as r]
          [cypris-form :as cf]
          [my-ns.reagent-bootstrap :as ui] ;; see examles/reagent-bootstrap
          )
          
(set! f/*atom-fn* r/atom)

(defn my-component []
  (let [action (fn [{:keys [values form]}] ...)
        values {:name "John Doe"
                :gender "male"
                :friends [{:name "Donald"} {:name "Daisy"}]}
        fdef [:name {:type :text :label "Name" :hint "Your Name"}
              :gender {:type :radio
                       :label "Sex"
                       :options [["male" "Male"]["female" "Female"]]
                       :default-value "male"}
              :friends [[:name {:type :text 
                                :label "Name" 
                                :default-value "John" 
                                :hint "Your friends name"}]]]
        form (cf/form fdef values action)]
        
    (fn []
      [:div
       [:h1 "Hello World"]
       [:form
        [ui/render form :name]
        [ui/render form :gender]
        [:h2 "Friends"]
        (doall (for [fs (cf/field form :friends)]
                 ^{:key (cf/path fs)}
                 [:div
                  [ui/render fs :name]
                  [ui/remove-btn form :friends fs]
                  ]))
        [ui/add-btn form :friends]
        [ui/submit-btn form "Submit"]]]
      )))

```

## License

Copyright © 2017 vinett-video Mediaservice

Distributed under the Eclipse Public License version 1.0 
