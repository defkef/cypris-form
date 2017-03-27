(ns cypris-form-example.ui
  (:refer-clojure :exclude [list])
  (:require [cypris-form.core :as f]
            [clojure.string :as str]
            [reagent.core :as r]))


;; react helper to prevent stupid key warnings

(defn with-key [items]
  (map #(with-meta %1 {:key %1}) items))

(defn list [& items]
  (with-key items))

;; renderer

(defmulti render*
  (fn [fd]
    (:type fd)))

(defn render [form fdname]
  (let [fd (f/field form fdname)]
    (render* fd)))

(defn container [fd & xs]
  [:div.form-group {:class (when-not (f/valid? fd) "has-error")}
   (with-key xs)])

(defn render-errors [fd]
  [:span {:class "form-validation"}
   (str/join ", " (f/errors fd))])

(defn render-label [fd label]
  [:label {:class "label-control" :for (f/id fd)} label])

(defn render-hint [text]
  [:div.help-block text])

(defn submit-btn [fd label]
   [:button {:type "submit"
             :on-click #(f/submit! fd)
             :class "btn btn-primary"}
    label])

(defn remove-btn [form fdname item]
  [:span.glyphicon.glyphicon-remove
   {:on-click #(f/remove! (f/field form fdname) item)}
   "Remove"])

(defn add-btn [form fdname]
  (let [fd (f/field form fdname)]
    [:a.btn.btn-default
     [:span.glypicon.glphicon-plus
      {:on-click #(f/add! fd)}
      "Add"]]))

(defn render-input
  ([fd]
   (render-input fd {:on-change #(f/set-value! fd (-> % .-target .-value))}))
  ([fd {:keys [on-change]}]
   [:input {:value (f/value fd)
            :id (f/id fd)
            :type (name (:type fd))
            :placeholder (:placeholder fd)
            :class "form-control"
            :on-change on-change}]))

(defmethod render* :radio [fd]
   (container fd
              (when-let [label (:label fd)]
                (render-label fd label))
              (doall (for [[k v] (:options fd)]
                       ^{:key k} [:div.radio
                                  [:label
                                   [:input {:type "radio"
                                            :on-change #(f/set-value! fd (-> % .-target .-value))
                                            :value k
                                            :id (str (f/name fd) "_" k)
                                            :default-checked (= k (f/value fd))
                                            :name (f/name fd)}]
                                   v]]))))

(defmethod render* :default [fd]
  (container fd
             (when-let [label (:label fd)]
               (render-label fd label))
             (if (:addon fd)
               [:div.input-group
                [:div.input-group-addon (:addon fd)]
                (render-input fd)]
               (render-input fd))
             (when-let [hint (:hint fd)]
               (render-hint hint))
             (when-not (f/valid? fd)
               (render-errors fd))
             ))
