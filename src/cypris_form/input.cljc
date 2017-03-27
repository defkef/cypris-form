(ns cypris-form.input)

(declare input-coll)

(defn input-map [values defaults missing]
  (into {}
    (for [[k dfv] defaults
          :let [v (get values k)]]
      (cond
        (map? dfv)
        (if (= missing v)
          [k (input-map {} dfv missing)]
          [k (input-map v dfv missing)])

        (vector? dfv)
        (if (= missing v)
          [k (input-coll [] dfv missing)]
          [k (input-coll v dfv missing)])

        :else
        (let [v (get values k missing)]
          (if (= missing v)
            [k dfv]
            [k v]))))))

(defn input-coll [values [default options] missing]
  (let [options (or options {})]
    (if (empty? values)
      (vec (repeat (get options :default-size 0) default))
      (mapv #(input-map % default missing) values))))

(defn input [present-values default-values missing-key]
  (if (map? default-values)
    (input-map present-values default-values missing-key)
    (input-coll present-values default-values missing-key)))
