;; from Google group
;; https://groups.google.com/forum/?fromgroups=#!topic/seesaw-clj/XVQdBNcLQRE

(ns todopl.jcalendar
  (:use seesaw.core
;        seesaw.dev
        seesaw.table
        seesaw.widgets.log-window
        seesaw.mig
        seesaw.value
        [seesaw.options
         :only (bean-option default-option apply-options ignore-options option-map option-provider)]
        [seesaw.widget-options :only [widget-option-provider]]
        )
  (:import (com.toedter.calendar
             JCalendar
             JDateChooser          
             ))
  )

(def common-options
 (option-map
   (default-option
     :date
     (fn [widget v]
       (.setDate widget v)
       )
     (fn [widget]
       (.getDate widget)
       )
     ["Gets or Sets date as java.util.Date"]
     )
   (default-option
     :calendar
     (fn [widget v]
       (.setCalendar widget v)
       )
     (fn [widget]
       (.getCalendar widget)
       )
     ["Gets or Sets date as "]
     )
   ))

;; (def jcalendar-options
;;  (merge
;;    default-options
;;    common-options
;;    ))
;; 
;; (widget-option-provider com.toedter.calendar.JCalendar
;;                         jcalendar-options)
;; 

;; (defn jcalendar [& opts]
;;   (apply-options (construct JCalendar) opts)
;;   )
;; 
;; 
(def jdatechooser-options
  (merge
    default-options
    common-options
    (option-map
      (default-option
        :date-format
        (fn [widget v]
          (.setDateFormatString widget v))
        (fn [widget]
          (.getDateFormatString widget))
        ["gets or sets the date format string"])

      )))

(widget-option-provider com.toedter.calendar.JDateChooser
                        jdatechooser-options)

(defn jdatechooser [& opts]
  (apply-options (construct JDateChooser) opts)
  )

(extend-protocol Value

  com.toedter.calendar.JDateChooser
  (container?* [this] false)
  (value* [this] (.getDate this))
  (value!* [this v]
    (.setDate this v)
    )

  com.toedter.calendar.JCalendar
  (container?* [this] false)
  (value* [this] (.getDate this))
  (value!* [this v]
    (.setDate this v)
    )

  )

