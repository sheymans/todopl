"""
    Copyright (C) 2012-2014 Stijn Heymans

    This program is free software: you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program.  If not, see <http://www.gnu.org/licenses/>.
"""

(ns todopl.utilities
  (:import [java.util Calendar Date])
  (:use (clj-time [core :exclude (extend)]))
  (:use (clj-time [local :exclude (extend)]))
  (:use (clj-time [coerce :exclude (extend)]))
  (:use (clj-time [format :exclude (extend)])))

(defn find-if [f s]
  "Find the first item in list s that satisfies the function f."
  (let [reduced (filter f s)]
    (when reduced
      (first reduced))))

(defn replace-map-in-vector [items k item]
  (map (fn [i]
         (if (= (k i) (k item))
           item
           i))
       items))

(defn remove-map-in-vector [items k item]
  (remove (fn [a]
            (= (k a) (k item)))
          items))

(defn remove-if-foreign [items]
  (remove (fn [item]
            (= (:subtype item) :foreign))
          items))


(defn get-old-offset []
  (let [cal (Calendar/getInstance)]
    (/ (+ (.get cal Calendar/ZONE_OFFSET) 
          (.get cal Calendar/DST_OFFSET))
       (* 1000 60))))


(defn get-offset []
  ;; get local now via clj-time
  (let [local-n (local-now)]
    (/ (.getOffset (.getZone local-n) (.getMillis local-n))
       (* 1000 60))))


(defn convert-json-to-datetime
  [json]
  (let [f (formatters :date-time-no-ms)]
    (parse f json)))

(defn create-date
  [jsonformat offset]
  ;; to-date is to convert again to java.util.Date (print-dup cannot print
  ;; jodaTime or cljtime)
  (to-date (convert-json-to-datetime jsonformat)))



(defn create-list-of-hours-to-max
  ;; for max = 5 returns [0 1 2 3 4 5]
  [min max]
  (let [iter (fn thisfnname[m akk]
               (if (= m min)
                 (into akk [m])
                 (thisfnname (- m 1) (into akk [m]))))]
    (iter max nil)))

(defn create-list-of-hours-to-max-with-quarters
  ;; for max = 5 returns [ [0 "00"] [1 "15"] ... [5 "00"]...[5 "45"]]
  [min max]
  (let [iter (fn thisfnname[m akk]
               (cond (= m min) (into akk [[m "45"] [m "30"] [m "15"] [m "00"]])
                     (= m 24) (thisfnname (- m 1) akk)
                     (= m max) (thisfnname (- m 1) (into akk [[m "00"]]))
                     :else (thisfnname (- m 1) (into akk [[m "45"] [m "30"] [m "15"] [m "00"]]))))]
    (iter max nil)))

(defn create-list-of-hours-to-max
  ;; for max = 5 returns [0 1 2 3 4 5]
  [min max]
  (let [iter (fn thisfnname[m akk]
               (if (= m min)
                 (into akk [m])
                 (thisfnname (- m 1) (into akk [m]))))]
    (iter max nil)))


(defn transform-hours-and-minutes-to-mins
  [hs mins]
  (+ (* hs 60) mins))

(defn transform-months-days-hours-and-minutes-to-mins
  [ms days hs mins daylength]
  (+ (* (* ms 30) daylength) ;; a month is always 30 days here ( a bit shorter than the user might expect). Note that daylength indicates how many minutes in a WORKING day
     (* days daylength)
     (* hs 60)
     mins))

(defn transform-mins-to-months-days-hours-minutes
  [mins daylength]
  (let [
        months (quot mins (* 30 daylength))  ; that's the amount of months that go into that emfc. The remainder is the minutes still to be divived:
        remainder (mod mins (* 30 daylength))
        days (quot remainder daylength)
        remainder (mod remainder daylength)
        hours (quot remainder 60)
        remainder (mod remainder 60)
        minutes remainder
        ]
    {:months months :days days :hours hours :minutes minutes}))


(defn convert-string-time-to-numbers
  "Convert a string s such as 17:15 to something like {:hours 17 :minutes 15}
  or something like 07:00 to {:hours 7 :minutes 0}"
  [s]
  (let [spl (clojure.string/split s #":")
        hours (first spl)
        minutes (second spl)]
    {:hours (if (= (subs hours 0 1) "0")
              (read-string (subs hours 1))
              (read-string hours))
     :minutes (if (= minutes "00")
                0
                (read-string minutes))}))


(defn convert-number-to-string-time
  "Opposite of convert-string-time-to-number."
  [n]
  (cond (and (< -1 n) (< n 10)) (str "0" n ":00")
        (and (> n 9) (< n 24)) (str n ":00")
        :else nil))

(defn convert-number-and-quarter-to-string-time
  "Convert a pair [5 15] to 05:15"
  [n quarter]
  (let [quarter (if (= quarter 0) "00" quarter)]
    (cond (and (< -10 n) (< n 10)) (str "0" n ":" quarter)
          (and (> n 9) (< n 100)) (str n ":" quarter)
          (and (< n -9) (> n -99)) (str n ":" quarter)
          :else nil)))


(defn normalize-date
  "Set date to real start of date (only year month day + offset"
  ([date offset] (normalize-date date offset (minus (from-date date) (minutes (* -1 offset)))))
   ;; next with three arguments: if the local-date is given use that one
   ([date offset local-date] 
    (let [ only-day (plus (date-time (year local-date) (month local-date) (day local-date)) (minutes (* -1 offset)))
          ]
      (to-date only-day))))

  (defn add-hours-to-java-date
    [date hs ms offset]
    (let [only-day (normalize-date date offset)
          cnew (plus (plus (from-date only-day) (hours hs)) (minutes ms))]
      (to-date cnew)))


  (defn convert-due-day-due-time-to-duedate
    "dueday is a java date and duetime is a string 08:00 or 17:00 (for example). We 
    add the duetime to the dueday and return a java date."
    [dueday duetime offset]
    (let [{:keys [hours minutes]} (convert-string-time-to-numbers duetime)
          newdate (add-hours-to-java-date dueday hours minutes offset)]
      newdate))

  (defn get-local-clj-now [offset]
    (minus (now) (minutes (* -1 offset))))

  (defn get-local-java-now [offset]
    (to-date (get-local-clj-now offset)))

  (defn get-local-clj-date [date offset]
    "from a java date and an offset in minutes get the local clj-time date."
    (minus (from-date date) (minutes (* -1 offset))))

  (defn get-local-java-date [date offset]
    "from a java date and an offset in minutes get the local java date."
    (to-date (minus (from-date date) (minutes (* -1 offset)))))


  (defn java-date-is-in-the-past-p
    "date is a java Date"
    [date offset]
    (let [current-date (get-local-clj-now offset)
          cdate (get-local-clj-date date offset)]
      (after? current-date cdate)))

  (defn clj-date-is-in-the-past-p
    "date is a clj-time date"
    [date offset]
    (let [current-date (get-local-clj-now offset)]
      (after? current-date date)))

  (defn java-now 
    []
    (to-date (now)))

  (defn to-date-before-from-date-p
    [to from]
    (or (= to from)
        (before? (from-date to) (from-date from))))

  (defn readable-month [date]
    (let [;d (from-date date)
          month (month date)]
      (case month
        1 "Jan"
        2 "Feb"
        3 "Mar"
        4 "Apr"
        5 "May"
        6 "Jun"
        7 "Jul"
        8 "Aug"
        9 "Sep"
        10 "Oct"
        11 "Nov"
        12 "Dec")))

  (defn readable-number-day [date]
    ;(let [d (from-date date)]
    (day date))


  (defn readable-day-of-week [date]
    "Returns a string like Monday..."
    (let [;d (from-date date)
          w (day-of-week date)]
      (case w
        1 "Monday"
        2 "Tuesday"
        3 "Wednesday"
        4 "Thursday"
        5 "Friday"
        6 "Saturday"
        7 "Sunday")))

  (defn readable-day-title [date offset]
    ;  (let [date (normalize-date date offset)]
    ;; make sure you have the local time (it comes in UTC)
    (let [date (plus (from-date date) (minutes offset))]
      (str 
        "<html>"
        (readable-day-of-week date)
        "<br>"
        (readable-month date)
        " "
        (readable-number-day date)
        "</html>")))


(defn round-readable-time
  [readable-time]
  (let [numbers (convert-string-time-to-numbers readable-time)
        hours (:hours numbers)
        minutes (:minutes numbers)
        rounded_mins (cond
                       (< minutes 15) 0
                       (< minutes 30) 15
                       (< minutes 45) 30 
                       (< minutes 60) 45)]
    (convert-number-and-quarter-to-string-time hours rounded_mins)))

(defn readable-time [date offset]
  (let [local-date (get-local-clj-date date offset)]
    (unparse (formatters :hour-minute) local-date)))



(defn minus-string-times
  "minus things like 17:15 from 8:15 resulting in {:hours 9 :minutes 0}"
  [t1 t2]
  (let [{hours1 :hours minutes1 :minutes} (convert-string-time-to-numbers t1)
        {hours2 :hours minutes2 :minutes} (convert-string-time-to-numbers t2)
        hoursminus (- hours1 hours2)
        quartersminus (- minutes1 minutes2)]
    (cond (< quartersminus 0) {:hours (- hoursminus 1) :minutes (+ quartersminus 60)}
          :else {:hours hoursminus :minutes quartersminus})))


(defn greater-than-string-time
  [t1 t2]
  (let [diff (minus-string-times t1 t2)]
    (>= (:hours diff) 0)))

(defn strictly-greater-than-string-time
  [t1 t2]
  (let [diff (minus-string-times t1 t2)]
    (or (> (:hours diff) 0)
        (and (= (:hours diff) 0) (> (:minutes diff) 0)))))



(defn append
  [& vecs]
  (reduce into vecs))

(defn duration-in-minutes
  [java-date1 java-date2]
  (in-minutes (interval (from-date java-date1) (from-date java-date2))))

(defn duration-in-minutes-between-string
  [from to]
  (let [diff (minus-string-times to from)]
    (+ (* (:hours diff) 60)
       (:minutes diff))))


(defn normalized-java-date-before-today?
  [java-date]
  (let [offset (get-offset)
        ; normalized-today (normalize-date (get-local-java-now offset) offset)]
        normalized-today (normalize-date java-date offset (get-local-clj-now offset))]
    (before? (from-date java-date) (from-date normalized-today))))


(defn more-than-a-year-away?
  [java-date]
  (let [offset (get-offset)
        current-date (get-local-clj-now offset)
        cdate (get-local-clj-date java-date offset)
        minutes_in_interval (in-minutes (interval current-date cdate))
        days_in_interval (quot (quot minutes_in_interval 60) 24)]
    (> days_in_interval 365)))


(defn add-nr-of-days
  [start-clj nr]
  """ Add nr of days to the clojure date start-clj """
  (plus start-clj (days nr)))


(defn split-up-interval-per-day
  [start end]
  """ start and end are java dates. For example 1 Jan - 4 Jan, becomes 1 Jan - 2 Jan, 2 Jan - 3 Jan, 4 Jan - 5 Jan """
  (let [ start-clj (from-date start)
        end-clj (from-date end)
        minutes_in_interval (in-minutes (interval start-clj end-clj))
        days_in_interval (quot (quot minutes_in_interval 60) 24)
        iter (fn this_fn_name [nr_of_days result]
               (if (= nr_of_days 0)
                 result
                 (this_fn_name (- nr_of_days 1) (conj result [(to-date (add-nr-of-days start-clj (- nr_of_days 1)))
                                                              (to-date (add-nr-of-days start-clj nr_of_days))]
                                                      ))))]
    ;; days_in_interval is always supposed to be at least 1
    (iter days_in_interval [])))











