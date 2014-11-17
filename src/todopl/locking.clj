(ns todopl.locking
  )
;; if nil then refreshing schedule in background is allowed, otherwise not
(def refresh-locked (atom nil))


;; you are watching the past, do not refresh
(def past-locked (atom nil))


;; you are persisting data to files, prevent anything else from doing that
(def file-write-locked (atom nil))


(defn lock-refreshing
  []
  (reset! refresh-locked true))

(defn unlock-refreshing
  []
  (reset! refresh-locked false))

(defn is-refreshing-locked?
  []
  (deref refresh-locked))

(defn lock-past
  []
  (reset! past-locked true))

(defn unlock-past
  []
  (reset! past-locked false))

(defn is-past-locked?
  []
  (deref past-locked))


(defn is-file-write-locked?
  []
  (deref file-write-locked))

(defn lock-file-write
  []
  (reset! file-write-locked true))

(defn unlock-file-write
  []
  (reset! file-write-locked false))


(defn do-not-shutdown
  []
  (and is-refreshing-locked?
       is-file-write-locked?))
