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


(ns todopl.database
  (:require [todopl.utilities :as utils])
  (:use (overtone [at-at :exclude (now) :as overtone])) ;; for scheduling processes at certain intervals
  (:use (clj-time [core :exclude (extend)]))
  (:use (clj-time [coerce :exclude (extend)])))

;; to store the latest offset in
(def offset (atom 0))
(reset! offset (utils/get-offset))


;;;
;;;
;;; The Pool used for background processes

(defonce progressbar_pool (overtone/mk-pool))
(defonce username_pool (overtone/mk-pool))
(defonce background_pool (overtone/mk-pool))


;;
;;
;; global lock

(defonce global_lock (Object.))

;;;
;;; Preferences
;;;
;;;

(def preferences (atom nil))

(defn create-preferences
  [prefs]
  (let [p (dissoc 
            (assoc prefs :offset (utils/get-offset))
            :browse-preferences-directory-button)]
    (reset! offset (:offset p))
    (reset! preferences p)))

(defn get-preferences
  []
  (deref preferences))

(defn add-preferences
  [prefs]
  (reset! preferences prefs))


(defn add-exchange-version-to-preferences
  [version]
  (reset! preferences (assoc @preferences :exchangeversion version)))

;;;
;;; Tasks
;;;


(def tasks (atom []))

;; A task is a map {:id <number> :title <string> :due <date> :uwm <minutes> :emfc <minutes> :before <some id> :after <some id> :description <long string> }
;;
;;

(defn number-of-tasks
  []
  (count @tasks))

(defn create-task
  [title]
  (let [task {:id (str (java.util.UUID/randomUUID)) 
              :title title 
              :duedate (java.util.Date.) 
              :emfc 60 
              ; :hours-uwm 0
              ; :minutes-uwm 0
              :just_add false
              :uwm 15 
              :type "ToDo"
              :offset (utils/get-offset)}]
    (reset! offset (:offset task))
    (swap! tasks conj task)
    task))

(defn make-new-task
  "Note that this not add the task to the DB"
  []
  (let [task {:id (str (java.util.UUID/randomUUID)) 
              :title ""
              :duedate (java.util.Date.) 
              :due_day (java.util.Date.)
              :due_time (:endday (get-preferences)) 
              ; :hours-uwm 1
              ; :minutes-uwm 0
              :hours-emfc 1
              ; :months-emfc 0
              :just_add false
              :days-emfc 0
              :minutes-emfc 0
              :emfc 60 
              :uwm 15 
              :type "ToDo"
              :offset (utils/get-offset)}]
    (reset! offset (:offset task))
    task))

(defn remove-after-links
  [tasks id]
  (map #(if (= (first (:after %)) id)
          (assoc % :after [nil nil])
          %)
       tasks))

(defn delete-task
  [task]
  (swap! tasks
         utils/remove-map-in-vector
         :id
         task)
  (swap! tasks
         remove-after-links
         (:id task)))


(defn add-task
  [task]
  (swap! tasks conj task))

(defn add-tasks
  [tasklist]
  (when (not (empty? tasklist))
    (do 
      (add-task (first tasklist))
      (add-tasks (rest tasklist)))))


(defn get-tasks
  []
  (deref tasks))





(defn update-task
  [task]
  ;; get the offset up dated again
  (let [t (assoc task :offset (utils/get-offset))]
    (reset! offset (:offset t))
    (swap! tasks 
           utils/replace-map-in-vector
           :id 
           t)))

(defn get-task-title
  [id]
  (let [ts (get-tasks)]
    (:title 
      (utils/find-if #(= (:id %) id) ts))))


(defn sort-tasks
  []
  (let [ts (get-tasks)]
    (reset! tasks
            (sort-by #(to-long (from-date (utils/convert-due-day-due-time-to-duedate (:due_day %) (:due_time %) (utils/get-offset))))
                     ts))))


(defn clear-tasks
  []
  (reset! tasks []))

;;;
;;; Meetings
;;;


(def meetings (atom []))

;; A meeting is a map {:id <number> :title <string> :from <date> :to <date> :offset long}
;;


(defn number-of-meetings
  []
  (count @meetings))

(defn create-meeting
  [title]
  (let [meeting 
        {:id (str (java.util.UUID/randomUUID)) 
         :title title 
         :from (java.util.Date.) 
         :to (java.util.Date.)
         :type "Meeting"
         :where ""
         :who ""
         :offset (utils/get-offset)}]
    (reset! offset (:offset meeting))
    (swap! meetings conj meeting)
    meeting))

(defn make-new-meeting
  []
  (let [meeting 
        {:id (str (java.util.UUID/randomUUID)) 
         :title ""
         :from (java.util.Date.) 
         :to (java.util.Date.)
         :from_day (java.util.Date.)
         :from_time (:startday (get-preferences))
         :to_time (:endday (get-preferences))
         ;   :to_day (java.util.Date.)
         :where ""
         :who ""
         :type "Meeting"
         :offset (utils/get-offset)}]
    (reset! offset (:offset meeting))
    meeting))


(defn clear-meetings
  []
  (reset! meetings []))

(defn delete-meeting
  [meeting]
  (swap! meetings
         utils/remove-map-in-vector
         :id
         meeting)
  (swap! tasks
         remove-after-links
         (:id meeting)))



(defn add-meeting
  [meeting]
  (swap! meetings conj meeting))

(defn add-meetings
  [meetinglist]
  (when (not (empty? meetinglist))
    (do 
      (add-meeting (first meetinglist))
      (add-meetings (rest meetinglist)))))


(defn get-meetings
  []
  (deref meetings))

(defn update-meeting
  [meeting]
  (let [m (assoc meeting :offset (utils/get-offset))]
    (reset! offset (:offset m))
    (swap! meetings
           utils/replace-map-in-vector
           :id 
           m)))

(defn get-meeting-title
  [id]
  (let [ts (get-meetings)]
    (:title 
      (utils/find-if #(= (:id %) id) ts))))




;;;
;;; Schedule
;;;


(def schedule (atom []))

;; A meeting is a map {:id <number> :title <string> :from <date> :to <date> :offset long}
;;

(defn add-schedule-item
  [item]
  (swap! schedule conj item))

(defn clear-schedule
  []
  (reset! schedule []))

(defn add-schedule
  [scheduleitemlist]
  (when (not (empty? scheduleitemlist))
    (do 
      (add-schedule-item (first scheduleitemlist))
      (add-schedule (rest scheduleitemlist)))))

(defn get-schedule
  []
  (deref schedule))

(defn delete-schedule-item
  [item key]
  (swap! schedule
         utils/remove-map-in-vector
         key
         item))


;;
;; Foreign Meetings
;;

(def username (atom nil))
(def password (atom nil))



(defn generate-foreign-full-day-meetings
  [title from to location id]
  (let [intervals (utils/split-up-interval-per-day from to)
        iter (fn thisfnname [ivals meetings]
               (if (empty? ivals)
                 meetings
                 (let [f_int (first ivals)
                       f (first f_int)
                       e (second f_int)
                       remains (rest ivals)
                       meeting {:id id
                                :title title
                                :from f
                                :to e
                                :where location
                                :type "Meeting"
                                :subtype :foreign
                                :allday true
                                :offset (utils/get-offset)}]
                   (thisfnname remains (conj meetings meeting)))))]
    (iter intervals [])))


(defn add-foreign-meeting
  [title from to location id full-day]
  (if full-day
    ;; note that we split up the full day meeting but keep 1 ID: TODO?
    (let [day-meetings (generate-foreign-full-day-meetings title from to location id)]
      (swap! meetings into day-meetings))
    (let [ meeting {:id id
                    :title title
                    :from from
                    :to to
                    :where location
                    :type "Meeting"
                    :subtype :foreign
                    :allday full-day
                    :offset (utils/get-offset)}]
      (swap! meetings conj meeting))))

(defn delete-all-foreign-meetings
  []
  (swap! meetings
         utils/remove-if-foreign))



;;
;; Combinations
;;

(defn is-not-in-schedule-p 
  [id]
  "Something with id is not in schedule."
  (let [sched (get-schedule)]
    (not (utils/find-if #(= id (:item_id %)) sched))))

(defn not-in-schedule-because-estimated-time-became-0
  [task]
  (let [ emfc (if task (:emfc task) 1)]
    (<= emfc 0)))

(defn is-existing-task?
  [id]
  (utils/find-if #(= (:id %) id) (get-tasks)))

(defn get-items-except-id
  [id]
  (let [tasks (map #(vector (:id %) (:type %))  (filter #(not (= (:id %) id)) (get-tasks)))
        meetings (map #(vector (:id %) (:type %)) (filter #(not (= (:id %) id)) (get-meetings)))
        all (into tasks meetings)]
    all))


(defn get-item
  [id type]
  (case type
    "Meeting" (utils/find-if #(= (:id %) id) (get-meetings))
    "ToDo" (utils/find-if #(= (:id %) id) (get-tasks))
    nil nil))


(defn clear-all
  []
  (clear-tasks)
  (clear-meetings)
  (clear-schedule))


(defn title-main-frame
  []
  (let [nr-tasks (number-of-tasks)
        nr-meetings (number-of-meetings)
        ts (if (= 1 nr-tasks) "task" "tasks")
        mts (if (= 1 nr-meetings) "event" "events")]
    (str "Todopl (" nr-tasks " " ts ", " nr-meetings " " mts ")")))
