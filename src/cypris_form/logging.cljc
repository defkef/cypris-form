(ns cypris-form.logging
  (:refer-clojure :exclude [time])
  (:require #?(:clj  [clojure.tools.logging :as log]
               :cljs [goog.log :as glog]))
  #?(:cljs (:import goog.debug.Console)))


#?(:cljs
   (def logger
     (glog/getLogger "cypris")))

#?(:cljs
   (def levels {:severe goog.debug.Logger.Level.SEVERE
                :warning goog.debug.Logger.Level.WARNING
                :info goog.debug.Logger.Level.INFO
                :config goog.debug.Logger.Level.CONFIG
                :fine goog.debug.Logger.Level.FINE
                :finer goog.debug.Logger.Level.FINER
                :finest goog.debug.Logger.Level.FINEST}))

#?(:cljs
   (defn log-to-console! []
     (.setCapturing (goog.debug.Console.) true)))

#?(:cljs
   (defn set-level! [level]
     (.setLevel logger (get levels level (:info levels)))))

(def ^:dynamic *max-length* 500)

(defn truncate [s n]
  (subs s 0 (min (count s) n)))

(defn fmt [msgs]
  (letfn [(trunc [s] (-> s
                       (pr-str)
                       (truncate *max-length*)))]
    (apply str (interpose " " (map trunc msgs)))))

(defn info [& s]
  (let [msg (fmt s)]
    #?(:clj  (log/info msg)
       :cljs (glog/info logger msg))))

(defn debug [& s]
  (let [msg (fmt s)]
    #?(:clj  (log/debug msg)
       :cljs (glog/fine logger msg))))

(defn error [& s]
  (let [msg (fmt s)]
    #?(:clj (log/error msg)
       :cljs (glog/error logger msg))))
