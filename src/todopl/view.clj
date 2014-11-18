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



(ns todopl.view
  (:refer-clojure :exclude (promise deliver future future-call))
  (:require [todopl.database :as db])
  (:require [todopl.locking :as lock])
  (:require [todopl.maintenance :as maintenance])
  (:require [todopl.utilities :as utils])
  (:require [todopl.scheduler :as scheduler])
  (:require [todopl.tasks :as tasks])
  (:require [todopl.meetings :as meetings])
  (:require [todopl.ms_exchange :as ms_exchange])
  (:require [todopl.preferences :as preferences])
  (:require [todopl.lookfeel :as lookfeel])
  (:require [todopl.rounded-label :as rounded])
  (:require [todopl.common :as common])
  (:require [seesaw.bind :as bin])
  (:use (clj-time [core :exclude (extend now)]))
  (:use (clj-time [coerce :exclude (extend)]))
  (:use seesaw.core)
  (:use seesaw.table)
  (:use seesaw.color)
  (:use seesaw.mig)
  (:use seesaw.chooser)
  (:use seesaw.border)
  (:use seesaw.swingx)
  (:use seesaw.font)
  (:use (overtone [at-at :as overtone])) ;; for scheduling processes at certain intervals
  (:use cljque.promises) ;; for experimental implementation of promises etc
  (:import [javax.swing BorderFactory])
  (:import [net.miginfocom.layout LinkHandler])
  (:import [javax.swing SwingUtilities RepaintManager])
  ) 



;;; TODO: have a @changed variable that tells when to actually do a full redraw
;;; of the miglayout

(def layoutpast (atom nil))
(def layouttoday (atom nil))

(defn clean-up-scrollable [scrollable]
  (.removeAll scrollable)
  scrollable)

(defn dispose-mig-and-redraw-it [new_mig_past new_mig_today]

  (if @layoutpast
    (swap! layoutpast clean-up-scrollable))
  (if @layouttoday
    (swap! layouttoday clean-up-scrollable) )

  ;    TODO: the below 2 might be removeable if we manage to get rid of the
  ;    scroll panel
  (LinkHandler/clearWeakReferencesNow)
  (RepaintManager/setCurrentManager nil)

  (reset! layoutpast new_mig_past)
  (reset! layouttoday new_mig_today))


;;; Forward declarations

(declare refresh-viewer-panel) ;; mutually recursive

;;;;
;;;;
;;;;
;;;;

;; Disposing pop-ups means also switching the lights back on:

(defn clean-up
  [frame parent unlock]
  (dispose! frame)
  (if unlock (lock/unlock-refreshing))
  (lookfeel/switch-on-the-lights parent))


;;
;; The "refresh schedule action"
;;


(defn delete-task-and-schedule [task]
  (do 
    (db/delete-task task)
    (db/delete-schedule-item task :item_id)))

(defn delete-meeting-and-schedule [meeting]
  (do
    (db/delete-meeting meeting)
    (db/delete-schedule-item meeting :item_id)))

(defn update-and-cleanup-task [task]
  "This updates the DB with the task and removes any associated schedule items."
  (db/update-task task)
  (db/delete-schedule-item {:item_id (:id task)} :item_id))

(defn update-and-cleanup-meeting [meeting]
  (db/update-meeting meeting)
  (db/delete-schedule-item {:item_id (:id meeting)} :item_id))


(defn scheduling-failed
  [frame-to-reconsider message root]
  (lookfeel/sh-alert root message)
  (when frame-to-reconsider
    (config! frame-to-reconsider :visible? true))
  (when (not frame-to-reconsider)
    (lookfeel/switch-on-the-lights root))
  nil)

(defn scheduler-action-popup
  [sched frame-to-dispose root]
  (cond (= sched "time_limit_exceeded") (scheduling-failed frame-to-dispose (lookfeel/time_limit_exceeded_string) root)
        (= sched "no_schedule_found") (scheduling-failed frame-to-dispose (lookfeel/no_schedule_found_string) root)
        (= sched "unknown_exception") (scheduling-failed frame-to-dispose (lookfeel/unknown_exception_string) root)
        :else ;; you found a schedule so show it
        (do
          ;; dispose the frame you want to get rid of (e.g. the task or
          ;; meeting or preference frame)
          (clean-up frame-to-dispose root true)
          (refresh-viewer-panel root))))

(defn get-schedule-and-update-progress-bar 
  [bar panel-with-bar frame-to-dispose root]
  (let [;pool db/progressbar_pool ;; pool of threads to use
        fut (future (scheduler/scheduler))
        prom (promise)]
    (overtone/every 100 #(if (not (realized? fut))
                           (let [current-value (config bar :value)]
                             (config! bar :value (+ current-value 10)))
                           (do 
                             (overtone/stop-and-reset-pool! db/progressbar_pool)
                             (dispose! panel-with-bar)
                             (deliver prom @fut)))
                    db/progressbar_pool)
    (attend prom #(scheduler-action-popup (deref %) frame-to-dispose root))))

(defn make-progress-frame
  [root panel]
  (let [f (frame :undecorated? true :content panel)]
    ;    (.setLocationRelativeTo f root)
    (.setAlwaysOnTop f true)
    f))


(defn scheduler-action
  [frame-to-dispose root]
  (let [bar (progress-bar :size [300 :by 50] :id  :progress :orientation :horizontal :min 0 :max 1500 :value 10)
        panel (mig-panel
                :id :progress-bar-panel
                :background lookfeel/clouds
                :constraints ""
                :items [[(label :text "Calculating your schedule...") "wrap"]
                        [bar "wrap"]])]
    (when frame-to-dispose (config! frame-to-dispose :visible? false)) ;; get the task/meeting/preferences that you are editing out of the way during calculation of a schedule to prevent that a user gets tempted to click around.
    ;; lights are already dimmed when there is a frame-to-dispose, if you dim
    ;; twice you are fucked and in full light, stupid.
    (when (not frame-to-dispose) (lookfeel/dim-the-lights root))
    (-> (make-progress-frame frame-to-dispose panel)
      pack!
      (lookfeel/apply-lookfeel-frame root)
      show!)
    (get-schedule-and-update-progress-bar bar panel frame-to-dispose root)))


;;
;; The task pop-up behavior
;;


;; calculate an uwm based on a emfc
;;
;;
;;

; (defn round-off-emfc
;   [emfc]
;   (if (<= emfc 2400)
;     emfc
;     (* (quot emfc (preferences/get-minutes-in-a-day))
;        (preferences/get-minutes-in-a-day))))

(defn calculate-uwm-based-on-emfc 
  "Set the uwm to 60 minutes unless the emfc is smaller than that. "
  [emfc]
  (cond (<= emfc 60) emfc
        :else 60))

(defn add-behavior-task-panel
  [task-frame root id]
  (let [s (if id
            (utils/find-if #(= (:id %) id) (db/get-tasks))
            (db/make-new-task))
        new-id (:id s)]
    (value! 
      (select task-frame [:#task-edit-panel]) 
      {
       :id (:id s) 
       :title (:title s)
       :due_day (:due_day s)
       :due_time (let [endday (:endday (db/get-preferences))]
                   (if (utils/strictly-greater-than-string-time (:due_time s) endday)
                     endday
                     (:due_time s)))

       :offset (:offset s)
       ; :hours-uwm (:hours-uwm s)
       ; :minutes-uwm (:minutes-uwm s)
       ; :months-emfc (:months-emfc s)
       :days-emfc (:days-emfc s)
       :hours-emfc (:hours-emfc s)
       :minutes-emfc (:minutes-emfc s)
       :after (:after s)
       })


    (listen (select task-frame [:#just-add-task-button]) :action
            (fn [a]
              (let [current-task-in-panel (select task-frame [:#task-edit-panel])
                    value-task (value current-task-in-panel)
                    ;; the duedate (out of due_day and due_time)
                    duedate (utils/convert-due-day-due-time-to-duedate 
                              (:due_day (value current-task-in-panel))
                              (:due_time (value current-task-in-panel))
                              (deref db/offset))
                    normalized_day (utils/normalize-date (:due_day (value current-task-in-panel)) (deref db/offset)) ;; set it to real beginning of day (jcalendar fucks this up)
                    duedate-in-past-p (utils/java-date-is-in-the-past-p duedate (deref db/offset))
                    ;; the emfc:
                    emfc (utils/transform-months-days-hours-and-minutes-to-mins 0
                                                                                (:days-emfc value-task)
                                                                                (:hours-emfc value-task)
                                                                                (:minutes-emfc value-task)
                                                                                ;; daylength
                                                                                (preferences/get-minutes-in-a-day))

                    ; emfc-smaller-than-uwm (< emfc uwm) 
                    unvalid-emfc-too-small (tasks/unvalid-emfc-too-small-p emfc)
                    ;; the uwm (calculated based on emfc)
                    uwm (calculate-uwm-based-on-emfc emfc)
                    ; uwm (utils/transform-hours-and-minutes-to-mins (:hours-uwm value-task) (:minutes-uwm value-task))
                    ; unvalid-uwm-too-big (tasks/unvalid-uwm-too-big-p uwm)
                    ; unvalid-uwm-too-small (tasks/unvalid-uwm-too-small-p uwm)
                    too-far-in-future? (and
                                         (not duedate-in-past-p)
                                         (utils/more-than-a-year-away? duedate))
                    ]
                (cond 
                  ; unvalid-uwm-too-big (lookfeel/sh-alert task-frame (lookfeel/uwm-too-big-string unvalid-uwm-too-big))
                  ; unvalid-uwm-too-small (lookfeel/sh-alert task-frame (lookfeel/uwm-too-small-string))
                  duedate-in-past-p (do 
                                      (lookfeel/sh-alert task-frame (lookfeel/due-date-in-past-string)))
                  unvalid-emfc-too-small (lookfeel/sh-alert task-frame (lookfeel/emfc-too-small-string))
                  too-far-in-future? (lookfeel/sh-alert task-frame (lookfeel/too-far-in-future-string))
                  ; emfc-smaller-than-uwm (lookfeel/sh-alert task-frame (lookfeel/emfc-smaller-than-uwm-string))
                  :else 
                  (do
                    (let [updated-task (assoc (value current-task-in-panel)
                                              :id new-id
                                              :offset (deref db/offset) 
                                              :type "ToDo" 
                                              :uwm uwm
                                              :just_add true 
                                              :due_day normalized_day
                                              :duedate duedate
                                              :emfc emfc
                                              )] 
                      (if (or id (utils/find-if #(= (:id %) new-id) (db/get-tasks))) ;; you edited it (id present) or you added the new one already (like after an earlier "calculation")
                        (update-and-cleanup-task updated-task)
                        (db/add-task updated-task)))
                    (clean-up task-frame root true)
                    (refresh-viewer-panel root)
                    )))))
    (listen (select task-frame [:#cancel-add-task-button]) :mouse-clicked
            (fn [a] 
              (if (not id) ;; this was NOT edit, only then delete 
                (db/delete-task {:id new-id}))
              (if id
                ;; set the updated task back to the old meeting
                (db/update-task s))
              (clean-up task-frame root true)))
    (if id ;; this was an EDIT, only add delete behavior on existing thing
      (listen (select task-frame [:#task-delete]) :mouse-clicked
              (fn [a] 
                (delete-task-and-schedule {:item_id id :id new-id})
                (refresh-viewer-panel root)
                (clean-up task-frame root true))))

    (listen (select task-frame [:#calculate-and-add-task-button]) :action
            (fn [a]
              (let [current-task-in-panel (select task-frame [:#task-edit-panel])
                    value-task (value current-task-in-panel)
                    ;; the duedate (out of due_day and due_time)
                    duedate (utils/convert-due-day-due-time-to-duedate 
                              (:due_day (value current-task-in-panel))
                              (:due_time (value current-task-in-panel))
                              (deref db/offset))
                    normalized_day (utils/normalize-date (:due_day (value current-task-in-panel)) (deref db/offset)) ;; set it to real beginning of day (jcalendar fucks this up)
                    duedate-in-past-p (utils/java-date-is-in-the-past-p duedate (deref db/offset))
                    ;; the emfc:
                    emfc (utils/transform-months-days-hours-and-minutes-to-mins 0
                                                                                (:days-emfc value-task)
                                                                                (:hours-emfc value-task)
                                                                                (:minutes-emfc value-task)
                                                                                ;; daylength
                                                                                (preferences/get-minutes-in-a-day))
                    unvalid-emfc-too-small (tasks/unvalid-emfc-too-small-p emfc)
                    ; emfc-smaller-than-uwm (< emfc uwm) 
                    uwm (calculate-uwm-based-on-emfc emfc)
                    too-far-in-future? (and
                                         (not duedate-in-past-p)
                                         (utils/more-than-a-year-away? duedate))
                    ]
                (cond 
                  ; unvalid-uwm-too-big (lookfeel/sh-alert task-frame (lookfeel/uwm-too-big-string unvalid-uwm-too-big))
                  ; unvalid-uwm-too-small (lookfeel/sh-alert task-frame (lookfeel/uwm-too-small-string))
                  duedate-in-past-p (lookfeel/sh-alert task-frame (lookfeel/due-date-in-past-string))
                  unvalid-emfc-too-small (lookfeel/sh-alert task-frame (lookfeel/emfc-too-small-string))
                  too-far-in-future? (lookfeel/sh-alert task-frame (lookfeel/too-far-in-future-string))
                  ; emfc-smaller-than-uwm (lookfeel/sh-alert task-frame (lookfeel/emfc-smaller-than-uwm-string))
                  :else 
                  (do 
                    (let [updated-task (assoc (value current-task-in-panel)
                                              :id new-id
                                              :offset (deref db/offset) 
                                              :type "ToDo" 
                                              :uwm uwm
                                              :due_day normalized_day
                                              :duedate duedate
                                              :emfc emfc
                                              )] 
                      (if (or id (utils/find-if #(= (:id %) new-id) (db/get-tasks))) ;; you edited it (id present) or you added the new one already (like after an earlier "calculation")
                        (db/update-task updated-task)
                        (db/add-task updated-task)))
                    (scheduler-action task-frame root)
                    )))
              ))
task-frame))



;;;
;;; The Meeting pop-up behavior
;;;


(defn add-behavior-meeting-panel [meeting-frame root id]

  ;; first add binding from from_time to to_time (if from time in meeting is
  ;; chosen, to time gets set automatically)
  ;; and only bind when new meeting
  (if (not id)
    (bin/bind
      (select meeting-frame [:#from_time])
      (select meeting-frame [:#to_time])))

  (let [s (if id
            (utils/find-if #(= (:id %) id) (db/get-meetings))
            (db/make-new-meeting))
        new-id (:id s)]
    (value! 
      (select meeting-frame [:#meeting-edit-panel]) 
      {
       :id (:id s) 
       :title (:title s)
       :from_day (:from_day s)
       :from_time (:from_time s)
       ; :to_day (:to_day s) 
       :where (:where s)
       :who (:who s)
       :to_time (:to_time s)
       :offset (:offset s)
       })



    ;; only bind when making new meeting:


    (listen (select meeting-frame [:#just-add-meeting-button]) :action
            (fn [a]
              (let [current-meeting-in-panel (select meeting-frame [:#meeting-edit-panel])
                    value-meeting (value current-meeting-in-panel)
                    ;; the from date
                    from (utils/convert-due-day-due-time-to-duedate
                           (:from_day value-meeting)
                           (:from_time value-meeting)
                           (deref db/offset))
                    ;; the to date
                    to (utils/convert-due-day-due-time-to-duedate
                         (:from_day value-meeting) ;; on SAME DAY
                         (:to_time value-meeting)
                         (deref db/offset))

                    todate-before-from-date-p (utils/to-date-before-from-date-p to from)

                    ;; normalize the from_day and to_day
                    normalized_from_day (utils/normalize-date (:from_day value-meeting) (deref db/offset))]
                ; normalized_to_day (utils/normalize-date (:to_day value-meeting) (deref db/offset))]
                (cond 
                  todate-before-from-date-p (lookfeel/sh-alert meeting-frame (lookfeel/today-before-from-string))
                  :else
                  (do 
                    (let [updated-meeting (assoc (value current-meeting-in-panel)
                                                 :id new-id
                                                 :offset (deref db/offset) 
                                                 :type "Meeting"
                                                 :from_day normalized_from_day
                                                 ;:to_day normalized_to_day
                                                 :just_add true
                                                 :from from
                                                 :to to 
                                                 )] 
                      (if (or id (utils/find-if #(= (:id %) new-id) (db/get-meetings))) ;; you edited it (id present) or you added the new one already (like after an earlier "calculation")
                        (update-and-cleanup-meeting updated-meeting)
                        (db/add-meeting updated-meeting)))
                    (clean-up meeting-frame root true)
                    (refresh-viewer-panel root))))))
    (listen (select meeting-frame [:#cancel-add-meeting-button]) :mouse-clicked
            (fn [a]
              (if (not id) ;; this was NOT edit, only then delete 
                (db/delete-meeting {:id new-id}))
              ;; when it was an edit, you want to rule out any intermediate
              ;; behavior:
              (if id
                ;; set the updated meeting back to the old meeting
                (db/update-meeting s))
              (clean-up meeting-frame root true)))

    (if id 
      (listen (select meeting-frame [:#meeting-delete]) :mouse-clicked
              (fn [a] 
                (delete-meeting-and-schedule {:item_id id :id new-id})
                (refresh-viewer-panel root)
                (clean-up meeting-frame root true))))




    (listen (select meeting-frame [:#calculate-and-add-meeting-button]) :action
            (fn [a]
              (let [current-meeting-in-panel (select meeting-frame [:#meeting-edit-panel])
                    value-meeting (value current-meeting-in-panel)
                    ;; the from date
                    from (utils/convert-due-day-due-time-to-duedate
                           (:from_day value-meeting)
                           (:from_time value-meeting)
                           (deref db/offset))
                    ;; the to date
                    to (utils/convert-due-day-due-time-to-duedate
                         (:from_day value-meeting)
                         (:to_time value-meeting)
                         (deref db/offset))

                    todate-before-from-date-p (utils/to-date-before-from-date-p to from)

                    ;; normalize the from_day and to_day
                    normalized_from_day (utils/normalize-date (:from_day value-meeting) (deref db/offset))]
                ;                    normalized_to_day (utils/normalize-date (:to_day value-meeting) (deref db/offset))]
                (cond 
                  todate-before-from-date-p (lookfeel/sh-alert meeting-frame (lookfeel/today-before-from-string))
                  :else
                  (do 
                    (let [updated-meeting (assoc (value current-meeting-in-panel)
                                                 :id new-id
                                                 :offset (deref db/offset) 
                                                 :type "Meeting"
                                                 :from_day normalized_from_day
                                                 ;:to_day normalized_to_day
                                                 :from from
                                                 :to to 
                                                 )]
                      (if (or id (utils/find-if #(= (:id %) new-id) (db/get-meetings))) ;; you edited it (id present) or you added the new one already (like after an earlier "calculation")
                        (db/update-meeting updated-meeting)
                        (db/add-meeting updated-meeting))) 
                    (scheduler-action meeting-frame root))))
              ))
meeting-frame))


(defn add-behavior-ms_exchange-panel
  [username-frame root]
  (value! 
    (select username-frame [:#msexchange-panel]) 
    {
     :username @db/username
     :password @db/password
     })


  (listen (select username-frame [:#submit-button]) :action
          (fn [a]
            (let [current-usernamepassword-in-panel (select username-frame [:#msexchange-panel])
                  prefs (db/get-preferences)
                  value-usernamepassword (value current-usernamepassword-in-panel)
                  username (:username value-usernamepassword)
                  password  (:password value-usernamepassword)
                  version (:exchangeversion prefs)
                  foreigntype (:foreigntype prefs)
                  email (:email prefs)
                  from (utils/normalize-date (utils/get-local-java-now (utils/get-offset)) (utils/get-offset))
                  ;;; TODO instead of 30 go for at least 30 and for sure till
                  ;;; last task deadline
                  to (to-date (plus (from-date from) (days 30)))]
              (let [fut (future (ms_exchange/get_msexchange_events version from to username password email))]a
                (do 
                  (reset! db/username username)
                  (reset! db/password password)
                  (overtone/every 50 #(if (realized? fut)
                                        (do 
                                          (overtone/stop-and-reset-pool! db/username_pool)
                                          (clean-up username-frame root true)
                                          (if (= @fut :failure)
                                            (lookfeel/sh-alert root (lookfeel/ms_exchange_error))
                                            (refresh-viewer-panel root))))
                                  db/username_pool)
                  ;; the below line is a UI change; since the dialog is modal
                  ;; at this point everything that would come after blocks!
                  ;; This is why before this we are using overtone to check for
                  ;; when the ms_exchange has been realized.
                  (config! (select username-frame [:#foreignaccess]) :busy? true))))))



  (listen (select username-frame [:#cancel-username-button]) :mouse-clicked
          (fn [a]
            (clean-up username-frame root true)))
  username-frame)





;;; 
;;; Preferences pop-up behavior
;;;
;;;
;;;

;;; update :emfc (for example after schedule was changed)

(defn sync-emfc-with-user-emfc
  [task]
  (let [updated-emfc (utils/transform-months-days-hours-and-minutes-to-mins 0
                                                                            (:days-emfc task)
                                                                            (:hours-emfc task)
                                                                            (:minutes-emfc task)
                                                                            ;; daylength
                                                                            (preferences/get-minutes-in-a-day))
        updated-task (assoc task :emfc updated-emfc)]
    (db/update-task updated-task)))


(defn sync-all-emfc-with-user-emfc
  [tasks]
  (if (empty? tasks)
    true
    (do 
      (sync-emfc-with-user-emfc (first tasks))
      (sync-all-emfc-with-user-emfc (rest tasks)))))




(defn add-behavior-preferences-panel [preference-panel root]
  (listen (select preference-panel [:#update-preferences-button]) :action
          (fn [a]
            (let [current-preferences-in-panel (select preference-panel [:#preferences-edit-panel])
                  validDir (.isDirectory (java.io.File. (:data (value current-preferences-in-panel))))]
              (cond (not validDir) 
                    (lookfeel/sh-alert preference-panel (lookfeel/data-directory-not-exists-string))
                    :else
                    (do 
                      (preferences/gracefully-revert-to-new-data (value current-preferences-in-panel))
                      (db/create-preferences (value current-preferences-in-panel))
                      (db/clear-schedule) ;; you want to get rid of previous schedule items that might not fit on the grid anymore now
                      (sync-all-emfc-with-user-emfc (db/get-tasks))
                      (refresh-viewer-panel root)
                      (clean-up preference-panel root nil)
                      )))))

  (listen (select preference-panel [:#update-schedule-and-preferences-button]) :action
          (fn [a]
            (let [current-preferences-in-panel (select preference-panel [:#preferences-edit-panel])
                  validDir (.isDirectory (java.io.File. (:data (value current-preferences-in-panel))))]
              (cond (not validDir) 
                    (lookfeel/sh-alert preference-panel (lookfeel/data-directory-not-exists-string))
                    :else
                    (do 
                      (preferences/gracefully-revert-to-new-data (value current-preferences-in-panel))
                      (db/create-preferences (value current-preferences-in-panel))
                      (sync-all-emfc-with-user-emfc (db/get-tasks))
                      (scheduler-action preference-panel root))))))

  (listen (select preference-panel [:#cancel-preferences-button]) :mouse-clicked
          (fn [a]
            (clean-up preference-panel root nil)))


  (listen (select preference-panel [:#browse-preferences-directory-button]) :action
          (fn [a]
            (choose-file preference-panel
                         :selection-mode :dirs-only ;; only allow to select directories
                         :success-fn (fn [file-chooser dir] (value! (select preference-panel [:#data]) (.getAbsolutePath dir)))
                         :cancel-fn (fn [fc] (println "user cancelled")))))

  preference-panel)


;; The main pane display
;;

(defn upper-buttons-panel [card-panel]
  (let [
        add-deadline-button (lookfeel/sh-button :add-a-deadline-button "Add a Task" "deadline_clouds.png")
        add-meeting-button (lookfeel/sh-button :add-a-meeting-button "Add an Event" "meeting_cloud.png" )
        refresh-schedule-button (lookfeel/sh-button :refresh-schedule-button "Refresh Schedule" "refresh_cloud.png")
        foreign-button (lookfeel/sh-button :foreign-button "Get External Events" "foreign_cloud.png")
        ;cardnames ["Upcoming" "Past"]
        ;combo (combobox :id :card-selector :model cardnames)
        ;combo (do
        ;        (.setFocusable combo false)
        ;        combo)
        previous-button (lookfeel/sh-button :previous-button "Past" "previous_cloud.png")
        next-button (lookfeel/sh-button :next-button "Upcoming" "next_cloud.png")
        preferences-button (lookfeel/sh-button :preferences-button "Update preferences" "preferences_cloud.png")
        upper-butns (mig-panel
                      :id :buttons-scheduling
                      :background lookfeel/upper-buttons-color
                      :constraints ["" "[][]40[][]push[][]5[]" "30[]40"]
                      :items [
                              [add-deadline-button ""]
                              [add-meeting-button ""]
                              [refresh-schedule-button ""]
                              [foreign-button ""]
                              [previous-button ""]
                              [next-button ""]
                              [preferences-button ""]
                              ])]
    ;; bind the selection of the combobox to what we show (past or present
    ;; schedule)
    ;(bin/bind
    ;  (select upper-butns [:#card-selector])
    ;  (bin/b-do* #(cond (= % (first cardnames)) (show-card! card-panel :today)
    ;                    (= % (second cardnames)) (show-card! card-panel :past))))

    upper-butns))


(defn lower-buttons-panel
  []
  (let [
        ; twitter-button (lookfeel/sh-button :twitter-button "Follow us on Twitter" "twitter.png")
        ; facebook-button (lookfeel/sh-button :facebook-button "Like us on Facebook" "facebook.png")
        ; google-button (lookfeel/sh-button :google-button "Follow us on Google+" "google.png")
        ; todopl-button (lookfeel/sh-button :todopl-button "Go to the Todopl website" "todopl_full.png")
        ; help (lookfeel/sh-hyperlink "help" :help-link)
        license (lookfeel/sh-hyperlink "license" :license-link)
        ; ack (lookfeel/sh-hyperlink "acknowledgements" :ack-link)
        lower-butns (mig-panel
                      :id :lower-buttons
                      :background lookfeel/upper-buttons-color
                      :constraints ["" "[left]10[left]8[left]8[left]push[right]10[right]10[right]" "5[]5"]
                      :items [
                              ; [todopl-button ""]
                              ; [twitter-button ""]
                              ; [facebook-button ""]
                              ; [google-button ""]
                              ; [ack ""]
                              [license ""]
                              ; [help ""]
                              ])]

;    (config! help
;             :listen
;             [:mouse-clicked common/a-help])
;    ;    (config! help
;    ;             :tip
;    ;             "Get some online help")
    (config! license
             :listen
             [:mouse-clicked common/a-license])
    ;    (config! license
    ;             :tip
    ;             "What you can and cannot do with this software")
;    (config! ack 
;             :listen
;             [:mouse-clicked common/a-ack])
;    ;    (config! ack
;    ;             :tip
;    ;             "Todopl relies on some great open source software")
;    (config! todopl-button
;             :listen
;             [:action common/a-sheldy])
;    (config! twitter-button
;             :listen
;             [:action common/a-twitter])
;    (config! facebook-button
;             :listen
;             [:action common/a-facebook])
;
;    (config! google-button
;             :listen
;             [:action common/a-google])
    lower-butns))


(defn next-day [number]
  (if (< number 7)
    (+ 1 number)
    1))

(defn free-day [number]
  (let [prefs (db/get-preferences)]
    (case number
      1 (:mon prefs)
      2 (:tue prefs)
      3 (:wed prefs)
      4 (:thu prefs)
      5 (:fri prefs)
      6 (:sat prefs)
      7 (:sun prefs))))

(defn new-week-display [day previous-day]
  (if (not previous-day)
    false
    (let [prefs (db/get-preferences)
          d (from-date day)
          w (day-of-week d)
          p (from-date previous-day)
          pw (day-of-week p)
          iter (fn fnname [looking-at]
                 (cond
                   (= looking-at w) false
                   (free-day looking-at) true
                   :else (fnname (next-day looking-at))))]
      (if (= w pw)
        true
        (iter pw)))))


(defn day-title-panel [day previous-day]
  (let [is-it-first-after-free-day? (new-week-display day previous-day)
        base-label (if is-it-first-after-free-day?
                     (rounded/rounded-label :text (utils/readable-day-title day (utils/get-offset)) 
                                            :font (lookfeel/sh-day-title-font) 
                                            :foreground lookfeel/clouds 
                                            :background lookfeel/todopl-logo-background
                                            :icon "empty.png")
                     (label :text (utils/readable-day-title day (utils/get-offset)) 
                            :font (lookfeel/sh-day-title-font)
                            :foreground lookfeel/todopl-logo-background
                            :icon "empty.png"))]
    base-label))

(defn day-item-time-display [from to type]
  (let [ l (label :text (str from " - " to))]
    (config! l :font (lookfeel/sh-link-font))
    [l "w 100!"]))



(defn add-title-display-behavior
  [label id type subtype relevant-labels]
  (config! label
           :listen
           [:mouse-clicked 
            (fn [e] (do
                      (case type
                        :deadline (do
                                    (lookfeel/dim-the-lights (to-root e))
                                    (lock/lock-refreshing)
                                    (->
                                      (tasks/make-task-edit-frame (to-root e) id)
                                      (add-behavior-task-panel (to-root e) id)
                                      pack!
                                      (lookfeel/apply-lookfeel-frame (to-root e))
                                      show!))
                        "Meeting" (if (not (= subtype :foreign))
                                    (do
                                      (lookfeel/dim-the-lights (to-root e))
                                      (lock/lock-refreshing)
                                      (-> 
                                        (meetings/make-meeting-edit-frame (to-root e) id)
                                        (add-behavior-meeting-panel (to-root e) id)
                                        pack!
                                        (lookfeel/apply-lookfeel-frame (to-root e))
                                        show!)))
                        "ToDo" (do
                                 (lookfeel/dim-the-lights (to-root e))
                                 (lock/lock-refreshing)
                                 (-> 
                                   (tasks/make-task-edit-frame (to-root e) id)
                                   (add-behavior-task-panel (to-root e) id)
                                   pack!
                                   (lookfeel/apply-lookfeel-frame (to-root e))
                                   show!))

                        )))
            :mouse-entered
            (fn [e] (doseq [i relevant-labels]
                      (if (not (= (:subtype (:item i)) :foreign))
                        (lookfeel/apply-label-styling! (:label i) (:type (:item i)) (:subtype (:item i))  (:past i) (:not-in-schedule i) nil))))
            :mouse-exited
            (fn [e] 
              (doseq [i relevant-labels]
                (if (not (= (:subtype (:item i)) :foreign))
                  (lookfeel/apply-label-styling! (:label i) (:type (:item i)) (:subtype (:item i)) (:past i) (:not-in-schedule i) true))))]))

;;
;; Grid
;;

(def cells-for-deadline 6)
(def cells-after-endday (+ cells-for-deadline 1))
(def cells-before-startday 2)
(def cell-width 36)
(def height-cell 36)
(def date-width 130)

(defn cell-constraint
  [width]
  (str "h " height-cell "!, w " width "!"))

(defn date-cell-constraint
  [width]
  (str "h 52!, w " width "!"))

(defn last-cell-constraint
  [width]
  (str "h " height-cell "!, w " width "!, wrap"))

(defn item-cell-constraint
  [cells-during]
  (str "h " height-cell "!, grow, span " cells-during ", w null:null:" (* cell-width cells-during)))


(defn total-numbers-of-cells
  []
  (let [mins-needed (preferences/get-minutes-in-a-day)
        quarters (quot mins-needed 15)]
    (+ quarters
       cells-before-startday
       cells-after-endday)))


(defn next-time
  [start]
  (let [{:keys [hours minutes]} (utils/convert-string-time-to-numbers start)]
    (cond
      (= minutes 0) (utils/convert-number-and-quarter-to-string-time hours (+ minutes 15))
      (= minutes 30) (utils/convert-number-and-quarter-to-string-time hours (+ minutes 15))
      (= minutes 15) (utils/convert-number-and-quarter-to-string-time hours (+ minutes 15))
      (= minutes 45) (utils/convert-number-and-quarter-to-string-time  (+ hours 1) 0)
      :else "")))


(defn previous-time
  [start]
  (let [{:keys [hours minutes]} (utils/convert-string-time-to-numbers start)]
    (cond
      (= minutes 0) (utils/convert-number-and-quarter-to-string-time (- hours 1) 45)
      (= minutes 30) (utils/convert-number-and-quarter-to-string-time hours (- minutes 15))
      (= minutes 15) (utils/convert-number-and-quarter-to-string-time hours (- minutes 15))
      (= minutes 45) (utils/convert-number-and-quarter-to-string-time hours (- minutes 15))
      :else "")))




(defn next-nth-time
  [start n]
  (let [iter (fn fnname [n ti]
               (if (<= n 0)
                 ti
                 (fnname (- n 1) (next-time ti))))]
    (iter n start)))

(defn on-the-30?
  [stime]
  (let [{:keys [hours minutes]} (utils/convert-string-time-to-numbers stime)]
    (or (= minutes 30)
        (= minutes 0))))




(defn number-of-cells-for-item
  [type starttime endtime before? after? completely-before? completely-after? startday endday]
  (let [actual-time (quot (utils/duration-in-minutes-between-string starttime endtime) 15)]
    (case type
      :deadline cells-for-deadline
      ;; default
      (cond 
        completely-before? 1
        completely-after? 1
        (and before? after?) (quot (utils/duration-in-minutes-between-string (previous-time startday)
                                                                             (next-time endday)) 15)
        before? (quot (utils/duration-in-minutes-between-string (previous-time startday) endtime) 15)
        after? (quot (utils/duration-in-minutes-between-string starttime (next-time endday)) 15)
        :else actual-time))))

(defn print-label
  [l]
  (if (= label "")
    ""
    "l"))

(defn print-rows
  [rows]
  (if (empty? rows)
    (println "")
    (let [f (first rows)
          label (print-label (first f))
          constraint (second f)]
      (if (.contains constraint "wrap")
        (println "[l " constraint "]")
        (print "[l " constraint "]"))
      (print-rows (rest rows)))))



(defn number-of-cells-in-front-of-item
  [item string-time-previous-item total-cells] 
  (let [type (:type item)
        startday (:startday (db/get-preferences))
        endday (:endday (db/get-preferences))
        start-date (utils/readable-time (if (= type :deadline) (:duedate item) (:from item))
                                        (:offset item))
        diff (utils/minus-string-times start-date string-time-previous-item)
        diffend (utils/minus-string-times endday start-date)
        diffstart (utils/minus-string-times start-date startday)
        after-schedule? (or (< (:hours diffend) 0) (< (:minutes diffend) 0))
        before-schedule? (or (< (:hours diffstart) 0) (< (:minutes diffstart) 0))
        cells-before (+ (* (:hours diff) 4) (quot (:minutes diff) 15))]
    (cond
      after-schedule? (- total-cells (+ cells-after-endday 1))
      before-schedule? (- cells-before-startday 2)
      ;      (= string-time-previous-item startday) (+ cells-before-startday (- cells-before 1))
      :else (- cells-before 1))))


(defn set-title-and-add-behavior!
  [item-in-labels relevant-labels]
  (let [l (:label item-in-labels)
        i (:item item-in-labels)
        title (:title i)
        type (:type i)
        subtype (:subtype i)
        id (if (= type "ToDo") (:item_id i) (:id i))
        past-p (:past item-in-labels)
        not-in-schedule-p (:not-in-schedule item-in-labels)]
    (config! l :text (str 
                       (lookfeel/ind-title title)
                       (if (and (:where i)
                                (not (= (:where i) "")))
                         (str " @ " (:where i))
                         "")))

    (add-title-display-behavior l id type subtype relevant-labels)
    (lookfeel/apply-label-styling! l type subtype past-p not-in-schedule-p true)))


(defn create-border
  [px]
  (line-border :color lookfeel/halfhourborder :right px))

(defn create-top-border
  [px]
  (line-border :color lookfeel/fatcellcolor :top px))

(defn create-bottom-border
  [px]
  (line-border :color lookfeel/halfhourborder :bottom px))

(defn create-border-left
  [px]
  (line-border :color lookfeel/halfhourborder :left px))

(defn create-border-fat
  []
  (line-border :color lookfeel/fatcellcolor :right 3))

(defn create-border-fat-left
  []
  (line-border :color lookfeel/fatcellcolor :left 3))

(defn empty-cell
  []
  (label :text " " :border (create-border 1)))

(defn top-cell
  []
  (label :text " " :border (create-top-border 6)))

(defn empty-cell-fat-on-left
  []
  (label :text " " :border (create-border-fat)))

(defn empty-cell-fat-on-right
  []
  (label :text " " :border (create-border-fat-left)))

(defn set-border-fat!
  [item-in-labels]
  (config! (:label item-in-labels) :border (create-border-fat)))


(defn set-border-nil!
  [item-in-labels]
  (config! (:label item-in-labels) :border nil))

(defn display-on-grid?
  [stime startday endday]
  (and (utils/greater-than-string-time stime startday)
       (utils/greater-than-string-time endday stime)
       (on-the-30? stime)))

(defn make-label-fat!
  [label]
  (config! label :border (create-border-fat)))

(defn set-out-of-bound!
  [item-in-labels]
  ;  (set-title-and-add-behavior! item-in-labels relevant-labels)
  ;  (config! (:label item-in-labels) :text "")
  (if (not (= (:subtype (:item item-in-labels)) :foreign))
    (config! (:label item-in-labels) :icon "dot-white.png")))

(defn set-out-of-bound-normal!
  [item-in-labels]
  ;  (set-title-and-add-behavior! item-in-labels relevant-labels)
  ;  (config! (:label item-in-labels) :text "")
  (if (not (= (:subtype (:item item-in-labels)) :foreign))
    (config! (:label item-in-labels) :icon "dot_normal_white.png")))



(defn generate-row-item
  [item item-in-labels relevant-labels timeindex]
  """ Timeindex is where the generation of the row should start. It should be initially called (for the first row item with timeindex = previous time of startday)"""
  (let [
        startday (:startday (db/get-preferences))
        type (:type item)
        endday (:endday (db/get-preferences))

        start-time-this-item (case type
                               :deadline (:due_time item)
                               ;; we round it because foreign stuff might have
                               ;; times like 8:48
                               (utils/round-readable-time (utils/readable-time (:from item) (:offset item))))
        end-time-this-item (case type
                             :deadline (next-nth-time start-time-this-item 6)
                             (utils/round-readable-time (utils/readable-time (:to item) (:offset item))))
        ;; when item is a all-day thing overwrite the previous
        start-time-this-item (if (:allday item)
                               startday
                               start-time-this-item)
        end-time-this-item (if (:allday item)
                             endday
                             end-time-this-item)
        might-go-out-of-bounds? (or (= type "Meeting") (= type :deadline))
        item-before-schedule? (and might-go-out-of-bounds? 
                                   (utils/strictly-greater-than-string-time startday start-time-this-item))
        item-completely-before-schedule? (and item-before-schedule?
                                              (utils/greater-than-string-time startday end-time-this-item))
        item-after-schedule? (and might-go-out-of-bounds?
                                  (utils/strictly-greater-than-string-time end-time-this-item endday))
        item-completely-after-schedule? (and item-after-schedule?
                                             (utils/greater-than-string-time start-time-this-item endday))
        cells-during (number-of-cells-for-item type start-time-this-item end-time-this-item item-before-schedule? item-after-schedule? item-completely-before-schedule? item-completely-after-schedule? startday endday)


        run-through-grid (fn thisfnname [where-we-are-at akk]
                           (cond 
                             ;; if we are the first cell (before the actual
                             ;; start of the day)
                             (= where-we-are-at (previous-time startday))
                             (cond 
                               ;; (1) a item that is completely before the start of
                               ;; day (just one cell big): draw the label with
                               ;; out of bound logo, draw a big fat border on
                               ;; right, and be done.
                               item-completely-before-schedule? (do 
                                                                  (set-out-of-bound! item-in-labels)
                                                                  (set-border-fat! item-in-labels)
                                                                  {:cells [[(:label item-in-labels) (item-cell-constraint cells-during)]]
                                                                   :time startday})

                               ;; (2) a item that is before the day but runs
                               ;; over into the day: set out of bound logo,
                               ;; draw label with the right length of item 
                               ;; and set the where-we-are-at at that right
                               ;; length of item.
                               item-before-schedule? (do
                                                       (set-out-of-bound-normal! item-in-labels)
                                                       (let [next (next-nth-time where-we-are-at cells-during)]
                                                         (if (= next endday) ;; then we have to already draw a fat border on this label
                                                           (set-border-fat! item-in-labels)) 
                                                         (if (not (on-the-30? next))
                                                           (set-border-nil! item-in-labels))
                                                         (thisfnname next
                                                                     (conj akk 
                                                                           [(:label item-in-labels) (item-cell-constraint cells-during)]))))

                               ;; :else the item starts within the schedule (no
                               ;; guarantee on where it ends but it does not
                               ;; start here so no need to considere whether we
                               ;; hvae to draw the label fat)
                               ;; schedule: draw an empty fat cell and continue
                               :else (do
                                       (thisfnname startday
                                                   (into akk
                                                         [[(empty-cell-fat-on-left) (cell-constraint cell-width)]]))))

                             ;; we are 
                             (= where-we-are-at endday)
                             ;; if you made it here this means nothing threw
                             ;; you past the schedule
                             (cond 
                               ;; (1) item is completely after schedule
                               item-completely-after-schedule?  (do 
                                                                  (if (not (= type :deadline)) (set-out-of-bound! item-in-labels))
                                                                  (set-border-nil! item-in-labels)
                                                                  {:cells (into akk
                                                                                [[(:label item-in-labels) (item-cell-constraint cells-during)]])
                                                                   :time (if (not (= type :deadline)) (next-time endday) (next-nth-time endday cells-during))})
                               ;; item is deadline
                               ;; (everywhere else it can be treated as normal,
                               ;; but here we have to bypass the above
                               ;; behavior)
                               (= type :deadline) (do
                                                    (set-border-nil! item-in-labels)
                                                    {:cells (into akk
                                                                  [[(:label item-in-labels) (item-cell-constraint cells-during)]])
                                                     :time (next-nth-time endday cells-during)})

                               ;; otherwise you are done
                               :else 
                               {:cells akk :time where-we-are-at})

                             ;; stop here definitely
                             (= where-we-are-at (next-time endday)) {:cells akk :time where-we-are-at}

                             ;; you are past the item so we can stop
                             (= where-we-are-at end-time-this-item)
                             {:cells akk :time where-we-are-at}


                             ;; you are somewhere in the schedule range but
                             ;; NOT where the item starts (so we just an
                             ;; empty-cell, no border, fat border or normal
                             ;; border has to be decided)
                             (not (= where-we-are-at start-time-this-item)) 
                             (let [next (next-time where-we-are-at)]
                               (cond (= next endday)
                                     ;; then write fat border
                                     (thisfnname next
                                                 (into akk [[(empty-cell-fat-on-left) (cell-constraint cell-width)]]))

                                     (on-the-30? next)
                                     (thisfnname next
                                                 (into akk [[(empty-cell) (cell-constraint cell-width)]]))

                                     :else 
                                     (thisfnname next
                                                 (into akk [["" (cell-constraint cell-width)]]))))


                             ;; anything else you are somewhere between the 
                             ;; start and the end AND you are looking at the
                             ;; start-time-this-item
                             :else
                             (cond
                               item-after-schedule? (do
                                                      (if (not (= type :deadline)) (set-out-of-bound-normal! item-in-labels))
                                                      (set-border-nil! item-in-labels)
                                                      (let [next (next-nth-time where-we-are-at cells-during)]
                                                        (if (= next endday) ;; then we have to already draw a fat border on this label
                                                          (set-border-fat! item-in-labels)) 
                                                        (thisfnname next
                                                                    (into akk [[(:label item-in-labels) (item-cell-constraint cells-during)]]))))
                               :else 
                               (let [next (next-nth-time where-we-are-at cells-during)]
                                 (if (= next endday) ;; then we have to already draw a fat border on this label
                                   (set-border-fat! item-in-labels))
                                 (if (not (on-the-30? next))
                                   (set-border-nil! item-in-labels))
                                 (if (= next endday)
                                   {:cells (into akk [[(:label item-in-labels) (item-cell-constraint cells-during)]]) :time next}
                                   (thisfnname next
                                               (into akk [[(:label item-in-labels) (item-cell-constraint cells-during)]])))))))]

(set-title-and-add-behavior! item-in-labels relevant-labels)
(run-through-grid timeindex [])))



(defn cells-to-end-day
  [deadlinestart]
  (let [endday (:endday (db/get-preferences))
        diff (utils/minus-string-times endday deadlinestart)
        cells (+ (* (:hours diff) 4)
                 (quot (:minutes diff) 15))]
    cells))


(defn generate-empty-cells
  [amount timeindex]
  (let [endday (:endday (db/get-preferences))
        startday (:startday (db/get-preferences))
        iterafter (fn thisfnname [n akk ti]
                    (cond 
                      (<= n 1)  (into akk [[(empty-cell-fat-on-left) (cell-constraint cell-width)]]) 
                      (= n amount) (thisfnname (- n 1)
                                               (into akk 
                                                     [[(empty-cell-fat-on-left) (cell-constraint cell-width)]])
                                               (next-time ti))
                      (display-on-grid? (next-time ti)
                                        startday endday)
                      (thisfnname (- n 1)
                                  (into akk
                                        [[(empty-cell) (cell-constraint cell-width)]])
                                  (next-time ti))
                      :else (thisfnname (- n 1)
                                        (into akk
                                              [["" (cell-constraint cell-width)]])
                                        (next-time ti))))]
    (iterafter amount [] timeindex)))


(defn generate-empty-cells-to-end
  [timeindex]
  (let [endday (:endday (db/get-preferences))
        startday (:startday (db/get-preferences))

        run-through-grid (fn thisfnname [where-we-are-at akk]
                           (cond 
                             ;; cell that needs fat border on right
                             (or (= where-we-are-at (previous-time startday))
                                 (= where-we-are-at (previous-time endday)))
                             (thisfnname (next-time where-we-are-at)
                                         (conj akk [(empty-cell-fat-on-left) (cell-constraint cell-width)])) 

                             ;; last cell
                             (= where-we-are-at (next-nth-time endday cells-for-deadline))
                             (conj akk ["" (last-cell-constraint cell-width)]) 

                             (utils/greater-than-string-time where-we-are-at endday)
                             ;; no borders on these
                             (thisfnname (next-time where-we-are-at)
                                         (conj akk [" " (cell-constraint cell-width)]))

                             :else
                             (let [next (next-time where-we-are-at)]
                               (if 
                                 (on-the-30? next)
                                 (thisfnname next
                                             (conj akk [(empty-cell) (cell-constraint cell-width)]))
                                 (thisfnname next
                                             (conj akk ["" (cell-constraint cell-width)]))))))]
    (run-through-grid timeindex [])))

(defn generate-line-cells-to-end
  [timeindex]
  (let [endday (:endday (db/get-preferences))
        startday (:startday (db/get-preferences))

        run-through-grid (fn thisfnname [where-we-are-at akk]
                           (cond 
                             (= where-we-are-at (previous-time startday))
                             (thisfnname (next-time where-we-are-at)
                                         (conj akk ["" (cell-constraint cell-width)])) 
                             ;; last cell
                             (= where-we-are-at (next-nth-time endday cells-for-deadline))
                             (conj akk ["" (last-cell-constraint cell-width)]) 


                             (utils/greater-than-string-time where-we-are-at endday)
                             ;; no borders on these
                             (thisfnname (next-time where-we-are-at)
                                         (conj akk [" " (cell-constraint cell-width)]))

                             :else
                             (let [next (next-time where-we-are-at)]
                               (thisfnname next
                                           (conj akk [(top-cell) (cell-constraint cell-width)])))))]
    (run-through-grid timeindex [])))




(defn generate-todos-row
  [todos labels start-string]
  (if (not (empty? todos))
    (let [iter (fn thisfn
                 [items akk ti]
                 (if (empty? items)
                   {:current-row akk :time ti}
                   (let [f (first items)
                         id (if (= (:type f) "ToDo") (:item_id f) (:id f))
                         relevant-labels (get labels (keyword id))
                         item-in-labels (utils/find-if #(= f (:item %)) relevant-labels)
                         row-item (generate-row-item f item-in-labels relevant-labels ti)
                         cells-row-item (:cells row-item)
                         ntime (:time row-item)]
                     (thisfn
                       (rest items)
                       (utils/append akk cells-row-item)
                       ntime))))
          first-part (iter todos [["" (cell-constraint date-width)]]  (previous-time start-string))
          row (:current-row first-part)
          ti (:time first-part)
          cells-after (generate-empty-cells-to-end ti)]

      (utils/append
        row
        cells-after))
    []))


(defn generate-meeting-row 
  [items labels startday]
  (let [endday (:endday (db/get-preferences))
        iter (fn fnname
               [is akk unscheduled timeindex]
               (if (empty? is)
                 {:row akk :unscheduled unscheduled :time timeindex}
                 (let [f (first is)
                       from (utils/readable-time (:from f) (:offset f))
                       to (utils/readable-time (:to f) (:offset f))
                       overlaps (or (and (utils/strictly-greater-than-string-time timeindex from)
                                         (not (= timeindex (previous-time startday))))
                                    (utils/greater-than-string-time timeindex (next-time endday)))
                       ]
                   (cond
                     overlaps (fnname (rest is) akk (conj unscheduled f) timeindex)
                     :else
                     (let [id (if (= (:type f) "ToDo") (:item_id f) (:id f))
                           relevant-labels (get labels (keyword id))
                           item-in-labels (utils/find-if #(= f (:item %)) relevant-labels)
                           row-item (generate-row-item f item-in-labels relevant-labels timeindex)
                           cells-row-item (:cells row-item)
                           newtime (:time row-item)]
                       (fnname
                         (rest is)
                         (utils/append akk cells-row-item)
                         unscheduled
                         newtime))))))
        first-part (iter items [["" (cell-constraint date-width)]] [] (previous-time startday))
        row (:row first-part)
        unscheduled (:unscheduled first-part)
        ntime (:time first-part)
        cells-after (generate-empty-cells-to-end ntime)]

    {:row (utils/append row cells-after) :unscheduled unscheduled}))



(defn generate-meetings-rows
  [meetings labels start-day-string-time]
  (let [iter (fn fnname [items akk]
               (if (empty? items)
                 akk
                 (let [processed-meetings (generate-meeting-row items labels start-day-string-time)
                       row (:row processed-meetings)
                       unscheduled (:unscheduled processed-meetings)
                       new-akk (into akk row)]
                   (if (empty? unscheduled)
                     new-akk
                     (fnname unscheduled
                             new-akk)))))]
    (iter meetings [])))




(defn generate-deadline-row
  [item-in-labels relevant-labels startday]
  (let [row (generate-row-item (:item item-in-labels)
                               item-in-labels
                               relevant-labels
                               (previous-time startday))
        cells-before (:cells row)
        ti (:time row)
        cells-after (generate-empty-cells-to-end ti)]

    (utils/append
      [["" (cell-constraint date-width)]]
      cells-before
      cells-after)))


(defn generate-deadlines-rows
  "each deadline gets his own row."
  [deadlines labels start-day-string-time]
  (let [iter (fn fnname [items akk]
               (if (empty? items)
                 akk
                 (fnname 
                   (rest items)
                   (let [f (first items)
                         id (:id f)
                         relevant-labels (get labels (keyword id))
                         item-in-labels (utils/find-if #(= f (:item %)) relevant-labels)]
                     (into akk
                           (generate-deadline-row item-in-labels relevant-labels start-day-string-time))))))]
    (iter deadlines [])))


(defn generate-rows-for-day
  [day-items labels start-day-string-time cell-width total-cells]
  (let [todos (filter #(= (:type %) "ToDo") day-items)
        meetings (filter #(= (:type %) "Meeting") day-items)
        deadlines (filter #(= (:type %) :deadline) day-items)]
    (utils/append
      (generate-todos-row todos labels start-day-string-time)
      (generate-meetings-rows meetings labels start-day-string-time)
      (generate-deadlines-rows deadlines labels start-day-string-time))))

(defn start-time-cell
  [start-date]
  (label :text start-date :foreground lookfeel/timecolor :font (lookfeel/sh-time-font)))



(defn generate-empty-row-no-borders
  [cell-width total-cells startday]
  (let [endday (:endday (db/get-preferences))
        iter (fn thisfnname [n previous s akk]
               (cond 
                 (<= n 1) (into akk [["" (last-cell-constraint cell-width)]])
                 (= n (- total-cells 1)) (thisfnname (- n 1)
                                                     s
                                                     (next-time s)
                                                     (into akk 
                                                           (if (display-on-grid? (next-time s) startday endday)
                                                             [["" (cell-constraint cell-width)]
                                                              [(start-time-cell (next-time s))
                                                               (str "h " height-cell "!, align center, span 2")]]
                                                             [[(start-time-cell s)
                                                               (str "h " height-cell "!, align center, span 2")]]
                                                             )))
                 :else
                 (thisfnname (- n 1)
                             s
                             (next-time s)
                             (into akk
                                   (if (display-on-grid? (next-time s) startday endday)
                                     [[(start-time-cell (next-time s))
                                       (str "h " height-cell "!, align center, span 2")]]
                                     [])))))
        ]
    (iter (- total-cells 1) "" startday [["" (cell-constraint date-width)]])))


(defn generate-empty-row
  []
  (let [startday (:startday (db/get-preferences))]
    (utils/append
      [["" (cell-constraint date-width)]]
      (generate-empty-cells-to-end (previous-time startday)))))

(defn day-layout [day previous-day day-items labels]
  (let [start-day (:startday (db/get-preferences))
        total-cells (total-numbers-of-cells)
        ;        is-it-first-after-free-day? (new-week-display day previous-day)
        panel (mig-panel
                :constraints ["gapy 0, gapx 0" "" ""]
                :background :white
                :items (utils/append
                         ;                         (if is-it-first-after-free-day?
                         ;                           (utils/append [[(day-title-panel day previous-day)]] ; (date-cell-constraint date-width)]["" (last-cell-constraint cell-width)]]
                         ;                                         (generate-line-cells-to-end (previous-time start-day)))
                         [[(day-title-panel day previous-day)(date-cell-constraint date-width)]["" (last-cell-constraint cell-width)]] ;)
                         (generate-empty-row-no-borders cell-width total-cells start-day)
                         (generate-empty-row)
                         (generate-rows-for-day day-items labels start-day cell-width total-cells)))]
    panel))


(defn empty-schedule-information-box 
  [past-or-present]
  (let [deadline-button (lookfeel/sh-button :add-a-deadline-button-docu "Add a Task" "deadline.png") 
        meeting-button (lookfeel/sh-button :add-a-meeting-button-docu "Add an Event " "meeting.png")
        previous-button (lookfeel/sh-button :previous-button-docu "Past" "previous.png") 
        next-button (lookfeel/sh-button :next-button-docu "Upcoming" "next.png") 
        preferences-button (lookfeel/sh-button :preferences-button-docu "Update preferences" "preferences.png") 
        refresh-button (lookfeel/sh-button :refresh-schedule-button-docu "Refresh schedule" "refresh.png")
        foreign-button (lookfeel/sh-button :refresh-schedule-button-docu "Get External Events" "foreign.png")
        deadline-button-pressed-action (fn [a]
                                         (lookfeel/dim-the-lights (to-root a))
                                         (lock/lock-refreshing)
                                         (invoke-now ;; block everything else
                                                     (-> (tasks/make-task-create-frame (to-root a) nil)
                                                       (add-behavior-task-panel (to-root a) nil)
                                                       pack!
                                                       (lookfeel/apply-lookfeel-frame (to-root a))
                                                       show!)))

        meeting-button-pressed-action (fn [a]
                                        (lookfeel/dim-the-lights (to-root a))
                                        (lock/lock-refreshing)
                                        (invoke-now
                                          (-> (meetings/make-meeting-create-frame (to-root a) nil)
                                            (add-behavior-meeting-panel (to-root a) nil)
                                            pack!
                                            (lookfeel/apply-lookfeel-frame (to-root a))
                                            show!)))

        refresh-button-pressed-action (fn [a] (scheduler-action nil (to-root a)))

        foreign-button-pressed-action (fn [a] 
                                        (let [
                                              prefs (db/get-preferences)
                                              foreigntype (:foreigntype prefs)
                                              email (:email prefs)]
                                          (cond 
                                            (or (not foreigntype) (= foreigntype "no external calendar")) (lookfeel/sh-alert (to-root a) (lookfeel/no-external))
                                            (or (not email) (= email "")) (lookfeel/sh-alert (to-root a) (lookfeel/no-email))
                                            :else
                                            (do 
                                              (lookfeel/dim-the-lights (to-root a))
                                              (lock/lock-refreshing)
                                              (invoke-now
                                                (-> (ms_exchange/ms-exchange-input-create-frame (to-root a))
                                                  (add-behavior-ms_exchange-panel (to-root a))
                                                  pack!
                                                  (lookfeel/apply-lookfeel-frame (to-root a))
                                                  show!))))))



        previous-button-pressed-action (fn [a] (do
                                                 (lock/lock-past)
                                                 (show-card! (select (to-root a) [:#card-panel]) :past)))
        next-button-pressed-action (fn [a] (do
                                             (lock/unlock-past)
                                             (show-card! (select (to-root a) [:#card-panel]) :today)))

        preferences-button-pressed-action (fn [a]
                                            (lookfeel/dim-the-lights (to-root a))
                                            (invoke-now
                                              (-> (preferences/make-preferences-frame (to-root a))
                                                (add-behavior-preferences-panel (to-root a))
                                                pack!
                                                (lookfeel/apply-lookfeel-frame (to-root a))
                                                show!)))
        deadline-label (rounded/rounded-label :text "deadlines" :foreground lookfeel/clouds :font (lookfeel/sh-normal-font))
        todo-label (rounded/rounded-label :text "scheduled time for completing a task" :foreground lookfeel/clouds :font (lookfeel/sh-normal-font))
        meeting-label (rounded/rounded-label :text "events" :foreground lookfeel/clouds :font (lookfeel/sh-normal-font))
        foreign-label (rounded/rounded-label :text "external events" :foreground lookfeel/todopl-logo-background :font (lookfeel/sh-italic-font))
        any-label (rounded/rounded-label :foreground lookfeel/clouds :font (lookfeel/sh-normal-font))
        any-label2 (rounded/rounded-label :foreground lookfeel/clouds :font (lookfeel/sh-normal-font))

        panel   (if (= past-or-present :past)
                  [[(label 
                      :text "<html><i>There is nothing in your past.</i></html>"
                      :font (lookfeel/sh-normal-font)
                      :foreground lookfeel/todopl-logo-background) "span, gap top 50px, gap bottom 25px, wrap"]]
                  [
                   ;                   [(label 
                   ;                      :text "<html><center><h1>Welcome to Todopl!</h1></center></html>" 
                   ;                      :font (lookfeel/sh-huge-title-font)
                   ;                      :foreground lookfeel/todopl-logo-background) "center, span, gap top 50px, gap bottom 40px, wrap"]
                   ;

                   ["" "gap right 25px"]
                   [(label 
                      :text "<html><h3>Add a Task</h3> You know by when you want it done, but not when to do it? <br/> Click this button to enter your deadline and let Todopl schedule it for you.</html>"
                      :font (lookfeel/sh-normal-font)
                      :foreground lookfeel/todopl-logo-background)
                    "gap right 20px,  top"]
                   [deadline-button "wrap, top"]

                   ["" "gap right 25px"]
                   [(label
                      :text " "
                      :border (create-bottom-border 2)) "span, grow, wrap"]

                   ["" "gap right 25px"]
                   [(label 
                      :text "<html><h3>Add an Event</h3> You know exactly when it needs to happen, <br/>how long it takes and where it needs to go on your schedule? <br/> Click this button to schedule it yourself.</html>"
                      :font (lookfeel/sh-normal-font)
                      :foreground lookfeel/todopl-logo-background)
                    "gap right 20px,  top"]
                   [meeting-button "wrap, top"]

                   ["" "gap right 25px"]
                   [(label
                      :text " "
                      :border (create-bottom-border 2)) "span, grow, wrap"]

                   ["" "gap right 25px"]
                   [(label 
                      :text "<html><h3>Past Schedule</h3> Click this button to see your past deadlines and events.</html>"
                      :font (lookfeel/sh-normal-font)
                      :foreground lookfeel/todopl-logo-background)
                    "gap right 20px,  top"]
                   [previous-button "wrap, top"]

                   ["" "gap right 25px"]
                   [(label
                      :text " "
                      :border (create-bottom-border 2)) "span, grow, wrap"]

                   ["" "gap right 25px"]
                   [(label 
                      :text "<html><h3>Upcoming Schedule</h3> Click this button to see your upcoming schedule.</html>"
                      :font (lookfeel/sh-normal-font)
                      :foreground lookfeel/todopl-logo-background)
                    "gap right 20px,  top"]
                   [next-button "wrap, top"]

                   ["" "gap right 25px"]
                   [(label
                      :text " "
                      :border (create-bottom-border 2)) "span, grow, wrap"]

                   ["" "gap right 25px"]
                   [(label 
                      :text "<html><h3>Update Preferences</h3>Click this button to go to your preferences and update<br/> the days and times you want Todopl to use when scheduling your tasks.</html>"
                      :font (lookfeel/sh-normal-font)
                      :foreground lookfeel/todopl-logo-background)
                    "gap right 20px,  top"]
                   [preferences-button "wrap, top"]

                   ["" "gap right 25px"]
                   [(label
                      :text " "
                      :border (create-bottom-border 2)) "span, grow, wrap"]

                   ["" "gap right 25px"]
                   [(label 
                      :text "<html><h3>Refresh Schedule</h3> Click this button to let Todopl schedule your tasks and help you <br/>meet your deadlines without missing your events.</html>"
                      :font (lookfeel/sh-normal-font)
                      :foreground lookfeel/todopl-logo-background)
                    "gap right 20px,  top"]
                   [refresh-button "wrap, top"]

                   ["" "gap right 25px"]
                   [(label
                      :text " "
                      :border (create-bottom-border 2)) "span, grow, wrap"]

                   ["" "gap right 25px"]
                   [(label 
                      :text "<html><h3>Get External Events</h3> Click this button to ask Todopl to get events from an external calendar.</html>"
                      :font (lookfeel/sh-normal-font)
                      :foreground lookfeel/todopl-logo-background)
                    "gap right 20px,  top"]
                   [foreign-button "wrap, top"]

                   ["" "gap right 25px"]
                   [(label
                      :text " "
                      :border (create-bottom-border 2)) "span, grow, wrap"]



                   ["" "gap right 25px"]
[(label 
   :text "<html><h3>And Finally...</h3>These are the different types of items you will see on your schedule:</html>"
   :font (lookfeel/sh-normal-font)
   :foreground lookfeel/todopl-logo-background)
 "gap right 20px, gap bottom 20px,  top"]
["" "wrap"]

["" "gap right 25px"][(do 
                        (lookfeel/apply-label-deadline-styling! deadline-label true)
                        deadline-label)
                      (str "h " height-cell "!, w 300!, wrap, top")]
["" "gap right 25px"][(do 
                        (lookfeel/apply-label-todo-styling! todo-label true)
                        todo-label)
                      (str "h " height-cell "!, w 300!, wrap, top")]
["" "gap right 25px"][(do 
                        (lookfeel/apply-label-meeting-styling! meeting-label nil true)
                        meeting-label)
                      (str "h " height-cell "!, w 300!, wrap, top")]

["" "gap right 25px"][(do 
                        (lookfeel/apply-label-foreign-styling! foreign-label true)
                        foreign-label)
                      (str "h " height-cell "!, w 300!, wrap, top")]

["" "gap right 25px"][(do 
                        (lookfeel/apply-label-past-styling! any-label true)
                        (config! any-label :icon "empty.png")
                        (config! any-label :text "any past item") 
                        any-label)
                      (str "h " height-cell "!, w 300!, wrap, top")]
["" "gap right 25px"][(do 
                        (lookfeel/apply-label-not-in-schedule-styling! any-label2 true)
                        (config! any-label2 :icon "empty.png")
                        (config! any-label2 :text "any yet to be scheduled item") 
                        any-label2)
                      (str "h " height-cell "!, w 300!, wrap, top")]

])]

(if (not (= past-or-present :past))
  (do 
    (listen deadline-button :action deadline-button-pressed-action)
    (listen meeting-button :action meeting-button-pressed-action)
    (listen previous-button :action previous-button-pressed-action)
    (listen next-button :action next-button-pressed-action)
    (listen preferences-button :action preferences-button-pressed-action)
    (listen refresh-button :action refresh-button-pressed-action)
    (listen foreign-button :action foreign-button-pressed-action)))

panel))



(defn days-in-panel [past-or-today grouped-schedule labels]
  (if (empty? grouped-schedule)
    (empty-schedule-information-box past-or-today)
    (let [iter (fn thisfnname [sched akk previous-day]
                 (if (not (empty? sched))
                   (let [first-grouping (first sched)
                         day (first first-grouping)
                         day-items (second first-grouping)]
                     (thisfnname (rest sched)
                                 (into akk 
                                       [[(day-layout day previous-day day-items labels) "span, wrap"]])
                                 day))
                   akk))]
      (iter grouped-schedule [] nil))))


(defn days-in-schedule-layout [past-or-today grouped-schedule labels]
  (let [scroll (scrollable (mig-panel 
                             :id :mig-days-in-schedule
                             :constraints ["center"]
                             :background :white
                             :items (days-in-panel past-or-today grouped-schedule labels))
                           :border 0)]
    ;; increase the speed of scrolling
    (.setUnitIncrement (.getVerticalScrollBar scroll) 16)
    scroll))

(defn days-in-schedule-layout-migonly [past-or-today grouped-schedule labels]
  (mig-panel 
    :id :mig-days-in-schedule
    :constraints ["center"]
    :background :white
    :items (days-in-panel past-or-today grouped-schedule labels)))


(defn history-plus-today-and-later-container
  [before-part labels-before after-part labels-after]
  (do 
    ;(LinkHandler/clearWeakReferencesNow)
    (let [pane1 (days-in-schedule-layout :past before-part labels-before)
          pane2 (days-in-schedule-layout :today after-part labels-after)]
      (dispose-mig-and-redraw-it pane1 pane2)
      (card-panel :id :card-panel :items [ [@layoutpast :past] [@layouttoday :today]]))))




;; (defn history-plus-today-and-later-container
;;   [before-part labels-before after-part labels-after]
;;   (let [pane1 (task-pane :title "Past" 
;;                          :id :past-pane
;;                          :items [ (days-in-schedule-layout :past before-part labels-before) ]
;;                          :collapsed? true
;;                          :animated? true)
;;         pane2 (task-pane :title "Today and beyond..."
;;                          :id :today-pane
;;                          :animated? true
;;                          :items [ (days-in-schedule-layout :today after-part labels-after)])
;;         content-pane1 (.getContentPane pane1)
;;         content-pane2 (.getContentPane pane2)]
;;     (.setBorder content-pane1 (BorderFactory/createEmptyBorder))
;;     (.setBackground content-pane1 (color :white))
;;     (.setBorder content-pane2 (BorderFactory/createEmptyBorder))
;;     (.setBackground content-pane2 (color :white))
;;     (scrollable (task-pane-container
;;                   :background "#FFFFFF"
;;                   :id :pane-container-past-today
;;                   :items [pane1 pane2]))))
;; 

(defn create-label
  [day-item]
  (let [from (:from day-item)
        to (:to day-item)
        due_day (:due_day day-item)
        due_time (:due_time day-item)
        where (:where day-item)
        offset (:offset day-item)
        due (and due_day due_time (utils/readable-time (utils/convert-due-day-due-time-to-duedate due_day due_time offset) offset))
        from (and from (utils/readable-time from offset))
        to (and to (utils/readable-time to offset))
        type (:type day-item)
        subtype (:subtype day-item)]
    (rounded/rounded-label 
      :border (create-border 1)
      :foreground (if (= subtype :foreign)
                    lookfeel/todopl-logo-background
                    lookfeel/clouds)
      :font (if (= subtype :foreign)
              (lookfeel/sh-italic-font)
              (lookfeel/sh-normal-font))
      :tip (str (if (and (= type "Meeting") (:allday day-item))
                  "all day:"
                  (case type
                    :deadline (str "@ " due)
                    (str from " - " to)))
                "  "
                (:title day-item)
                (case type
                  "Meeting" (if (and where (not (= where "")))
                              (str " @ " where)
                              "")
                  "")))))




(defn corresponding-labels [grouped-schedule]
  ;;   "This is the shape of the result: {:f765917f-0be3-4655-88c4-288937afe857 ({:item {:from #inst "2013-05-30T15:00:00.000-00:00", :from_time 08:00, :to #inst "2013-05-30T23:00:00.000-00:00", :to_time 16:00, :offset -420, :title test meeting, :type Meeting, :from_day #inst "2013-05-30T07:00:00.000-00:00", :id f765917f-0be3-4655-88c4-288937afe857}, :label #<JLabel$0 todopl.rounded_label.proxy$javax.swing.JLabel$0[,0,0,0x0,invalid,alignmentX=0.0,alignmentY=0.0,border=,flags=8388608,maximumSize=,minimumSize=,preferredSize=,defaultIcon=,disabledIcon=,horizontalAlignment=LEADING,horizontalTextPosition=TRAILING,iconTextGap=4,labelFor=,text=,verticalAlignment=CENTER,verticalTextPosition=CENTER]>}), :dd0cbc11-b469-4eba-b8a0-f11a010d9c24 ({:item {:months-emfc 0, :after [nil nil], :offset -420, :minutes-emfc 0, :uwm 60, :hours-emfc 1, :emfc 60, :title testing, :due_day #inst "2013-05-07T07:00:00.000-00:00", :type :deadline, :minutes-uwm 0, :due_time 09:30, :hours-uwm 1, :duedate #inst "2013-05-07T16:30:00.000-00:00", :id dd0cbc11-b469-4eba-b8a0-f11a010d9c24, :days-emfc 0}, :label #<JLabel$0 todopl.rounded_label.proxy$javax.swing.JLabel$0[,0,0,0x0,invalid,alignmentX=0.0,alignmentY=0.0,border=,flags=8388608,maximumSize=,minimumSize=,preferredSize=,defaultIcon=,disabledIcon=,horizontalAlignment=LEADING,horizontalTextPosition=TRAILING,iconTextGap=4,labelFor=,text=,verticalAlignment=CENTER,verticalTextPosition=CENTER]>} {:item {:title testing, :from #inst "2013-05-07T15:00:00.000-00:00", :to #inst "2013-05-07T16:00:00.000-00:00", :type ToDo, :item_id dd0cbc11-b469-4eba-b8a0-f11a010d9c24, :offset -420}, :label #<JLabel$0 todopl.rounded_label.proxy$javax.swing.JLabel$0[,0,0,0x0,invalid,alignmentX=0.0,alignmentY=0.0,border=,flags=8388608,maximumSize=,minimumSize=,preferredSize=,defaultIcon=,disabledIcon=,horizontalAlignment=LEADING,horizontalTextPosition=TRAILING,iconTextGap=4,labelFor=,text=,verticalAlignment=CENTER,verticalTextPosition=CENTER]>})}"
  (let [iter (fn thisfnname [l akk]
               (if (empty? l)
                 akk
                 (let [first-grouping (first l)
                       day (first first-grouping)
                       day-items (second first-grouping)
                       iter-inner (fn thisfnnameinner [items akkinner]
                                    (if (empty? items)
                                      akkinner
                                      (let [i (first items)
                                            item_id (:item_id i)
                                            id (:id i)
                                            due_day (:due_day i)
                                            due_time (:due_time i)
                                            offset (:offset i)
                                            to (:to i)
                                            type (:type i)
                                            event-is-in-the-past-p (or (and due_day 
                                                                            due_time
                                                                            (utils/java-date-is-in-the-past-p (utils/convert-due-day-due-time-to-duedate due_day due_time offset) offset))
                                                                       (and to (utils/java-date-is-in-the-past-p to offset)))
                                            not-in-schedule-p (db/is-not-in-schedule-p id)]

                                        (thisfnnameinner (rest items)
                                                         (let [new-item {:item i 
                                                                         :label (create-label i)
                                                                         :past event-is-in-the-past-p
                                                                         :not-in-schedule (if (= type "ToDo") false not-in-schedule-p)}]
                                                           (case type
                                                             "Meeting" (assoc akkinner
                                                                              (keyword id)
                                                                              (conj (get akkinner (keyword id))
                                                                                    new-item))
                                                             "ToDo" (assoc akkinner
                                                                           (keyword item_id)
                                                                           (conj (get akkinner (keyword item_id))
                                                                                 new-item))
                                                             :deadline (assoc akkinner
                                                                              (keyword id)
                                                                              (conj (get akkinner (keyword id))
                                                                                    new-item))))))))]
                   (thisfnname (rest l)
                               (iter-inner day-items akk)))))]
    (iter grouped-schedule nil)))



(defn make-viewer-panel []
  (let [grouped-schedule (scheduler/get-schedule-by-day)
        before-part (:before grouped-schedule)
        after-part (:today-and-after grouped-schedule)
        corresponding-labels-before (corresponding-labels before-part)
        corresponding-labels-after (corresponding-labels after-part)
        card-panel (history-plus-today-and-later-container before-part corresponding-labels-before after-part corresponding-labels-after)
        panel (border-panel :id :viewer-panel
                            :north (upper-buttons-panel card-panel)
                            :center card-panel
                            :south (lower-buttons-panel)
                            ) ]
    ;; default is today of card-panel
    (show-card! card-panel :today)

    panel))




;;;
;;; Reducing the estimated time on deadlines based on past tasks
;;;


(defn duration-schedule-item [schedule-item]
  (let [jdate1 (:from schedule-item)
        jdate2 (:to schedule-item)
        we-are-in-this-item (utils/java-date-is-in-the-past-p jdate1 (:offset schedule-item))] ;; because jdate2 is not in the past if called correctly
    (if we-are-in-this-item
      ;; you want the duration to always be a multiple of 15, so 63 minutes
      ;; should result in 60, 19 minutes in 15 minutes etc.
      ;; 
      (do
        (* (+ (quot (utils/duration-in-minutes (utils/java-now) jdate2)
                    15)
              1)
           15))
      (utils/duration-in-minutes jdate1 jdate2))))

(defn get-new-emfc-for-deadline
  "Get the new emfc in minutes for the particular deadline id by looking only at tasks that are not in the past"
  [id]
  (let [iter (fn thisfnname [minutes l]
               (if (empty? l)
                 minutes
                 (let [f (first l)
                       mi (duration-schedule-item f)]
                   (thisfnname (+ mi minutes) (rest l)))))
        scheduled-items-for-id (filter #(= (:item_id %) id) (db/get-schedule))
        schedule-to-consider (filter #(not (utils/java-date-is-in-the-past-p (:to %) (:offset %)))
                                     scheduled-items-for-id)]
    (cond
      ;; the task with id was not scheduled yet, so then keep the old emfc
      (empty? scheduled-items-for-id) false
      ;; the task was scheduled but all scheduled items are in the past: set it
      ;; to 0
      (empty? schedule-to-consider) 0
      :else (iter 0 schedule-to-consider))))


;; now go through deadlines in db/get-tasks and set the daedlines fine.

(defn update-emfc-deadlines
  []
  (locking db/global_lock
    (let [tasks (filter #(and 
                           (not (utils/java-date-is-in-the-past-p (:duedate %) (:offset %)))
                           (not (= (:emfc %) 0)))
                        (db/get-tasks))
          daylength (preferences/get-minutes-in-a-day)
          iter (fn thisfnname 
                 [ts]
                 (if (empty? ts)
                   true
                   (let [f (first ts)
                         id (:id f)
                         new-emfc (get-new-emfc-for-deadline id)
                         new-emfc (cond
                                    ;; it's nil; cause there was no schedule item
                                    ;; to calculate it; just take the old one
                                    ;; then
                                    (not new-emfc) (:emfc f)
                                    ;; it got smaller than than the uwm
                                    ;(< new-emfc (:uwm f)) 0
                                    :else new-emfc)]
                     (if (not (= new-emfc (:emfc f)))
                       (let [other-estimates (utils/transform-mins-to-months-days-hours-minutes new-emfc daylength)
                             updated-task (assoc f
                                                 :emfc new-emfc
                                                 ; :months-emfc (:months other-estimates)
                                                 :days-emfc (:days other-estimates)
                                                 :hours-emfc (:hours other-estimates)
                                                 :minutes-emfc (:minutes other-estimates))]
                         (db/update-task updated-task)
                         (thisfnname (rest ts)))
                       (thisfnname (rest ts))))))]
      (iter tasks))))




;;;
;;; Globabl behavior panel
;;;

(defn add-behavior-viewer-panel [root]
  ;; immediately try to update the emfc deadlines
  (update-emfc-deadlines) ;; set them to correct time given the scheduled tasks
  (let [today-schedule (:today-and-after (scheduler/get-schedule-by-day))
        deadline-button-pressed-action (fn [a]
                                         (lookfeel/dim-the-lights root)
                                         (lock/lock-refreshing)
                                         (invoke-now ;; block everything else
                                                     (-> (tasks/make-task-create-frame root nil)
                                                       (add-behavior-task-panel root nil)
                                                       pack!
                                                       (lookfeel/apply-lookfeel-frame root)
                                                       show!)))

        meeting-button-pressed-action (fn [a]
                                        (lookfeel/dim-the-lights root)
                                        (lock/lock-refreshing)
                                        (invoke-now
                                          (-> (meetings/make-meeting-create-frame root nil)
                                            (add-behavior-meeting-panel root nil)
                                            pack!
                                            (lookfeel/apply-lookfeel-frame root)
                                            show!)))

        refresh-button-pressed-action (fn [a] (scheduler-action nil root))

        foreign-button-pressed-action (fn [a] 
                                        (let [
                                              prefs (db/get-preferences)
                                              foreigntype (:foreigntype prefs)
                                              email (:email prefs)]
                                          (cond 
                                            (or (not foreigntype) (= foreigntype "no external calendar")) (lookfeel/sh-alert root (lookfeel/no-external))
                                            (or (not email) (= email "")) (lookfeel/sh-alert root (lookfeel/no-email))
                                            :else
                                            (do 
                                              (lookfeel/dim-the-lights root)
                                              (lock/lock-refreshing)
                                              (invoke-now
                                                (-> (ms_exchange/ms-exchange-input-create-frame root)
                                                  (add-behavior-ms_exchange-panel root)
                                                  pack!
                                                  (lookfeel/apply-lookfeel-frame root)
                                                  show!))))))


        previous-button-pressed-action (fn [a] (do
                                                 (lock/lock-past)
                                                 (show-card! (select root [:#card-panel]) :past)))
        next-button-pressed-action (fn [a] (do
                                             (lock/unlock-past)
                                             (show-card! (select root [:#card-panel]) :today)))

        preferences-button-pressed-action (fn [a]
                                            (lookfeel/dim-the-lights root)
                                            (invoke-now
                                              (-> (preferences/make-preferences-frame root)
                                                (add-behavior-preferences-panel root)
                                                pack!
                                                (lookfeel/apply-lookfeel-frame root)
                                                show!)))]


    (listen (select root [:#add-a-deadline-button]) :action deadline-button-pressed-action)
    (listen (select root [:#add-a-meeting-button]) :action meeting-button-pressed-action)
    (listen (select root [:#refresh-schedule-button]) :action refresh-button-pressed-action)
    (listen (select root [:#foreign-button]) :action foreign-button-pressed-action)
    (listen (select root [:#previous-button]) :action previous-button-pressed-action)
    (listen (select root [:#next-button]) :action next-button-pressed-action)
    (listen (select root [:#preferences-button]) :action preferences-button-pressed-action)

    ;; finally some background behavior (refresh the schedule every minute and
    ;; make sure the emfcs of deadlines get results for scheduled items that are
    ;; on deadlines.
    ;(let [pool @db/background_pool]
    ;; refresh every minute BUT ONLY after the previous one completed!
    (overtone/interspaced 60000 #( ;; only refresh when not locked (calculating
                                      ;; task and meetings from an edit locks it --
                                      ;; avoid updating item in background while still
                                      ;; editing)
                                      if (and (not (lock/is-refreshing-locked?))
                                              (not (lock/is-past-locked?)))
                                      (do
                                        (refresh-viewer-panel root)
                                        (update-emfc-deadlines)
                                        ;(println "done periodic refresh at " (utils/java-now))
                                        ))
                          db/background_pool 
                          :initial-delay 30000)
    ;; synch with files every 5 minutes (in case someone leaves todopl
    ;; open; this will not be a problem)
    (overtone/interspaced 300000 #(if (not (lock/is-refreshing-locked?))
                                    (maintenance/persist-to-files))
                          db/background_pool
                          :initial-delay 60000)

    ))



(defn dispose-items [items]
  (if (not (empty? items))
    (let [f (first items)]
      (do 
        (print "disposing items")
        (dispose! f)
        (dispose-items (rest items))))))



(defn get_migpanel_from_scrollpanel [scrollpanel]
  ;;  JViewport viewport = scrollPane.getViewport(); 
  ;;  JEditorPane editorPane = (JEditorPane)viewport.getView(); 
  (let [viewport (.getViewport scrollpanel)
        miglayout (.getView viewport)]
    miglayout))

(defn set-viewport [scrollable new-viewport]
  (.removeAll scrollable)
  (.removeAll scrollable)
  (.setViewportView scrollable new-viewport)
  (.revalidate scrollable)
  (.repaint scrollable)
  (.setVisible scrollable true)
  (show! scrollable)
  scrollable)


(defn remove-all-components-in-card-panel [card-panel]
  (let [iter (fn fnname [comps]
               (if (not (empty? comps))
                 (let [f (first comps)]
                   (do 
                     (.remove card-panel f)
                     (fnname (rest comps))))))]
    (iter (.getComponents card-panel))))


(defn config-refresh-on-edt [root before-part labels-before after-part labels-after]
  (config! root :title (db/title-main-frame))
  (dispose-mig-and-redraw-it 
    (days-in-schedule-layout :past before-part labels-before) 
    (days-in-schedule-layout :today after-part labels-after))

  (remove-all-components-in-card-panel (select root [:#card-panel]))  
  ;; set the card panel to the new stuff
  (config! (select root [:#card-panel])
           :items [[@layoutpast :past]
                   [@layouttoday :today]])

  (.revalidate (select root [:#card-panel])) 
  (if (lock/is-past-locked?)
    (show-card! (select root [:#card-panel]) :past)
    (show-card! (select root [:#card-panel]) :today)))

(defn refresh-viewer-panel-body [root]
  (locking db/global_lock
    (let [grouped-schedule (scheduler/get-schedule-by-day)
          before-part (:before grouped-schedule)
          after-part (:today-and-after grouped-schedule)
          labels-before (corresponding-labels before-part)
          labels-after (corresponding-labels after-part)]
      (if (SwingUtilities/isEventDispatchThread)
        (config-refresh-on-edt root before-part labels-before after-part labels-after)
        ;; otherwise throw it on the edt:
        (invoke-later 
          (config-refresh-on-edt root before-part labels-before after-part labels-after))))))



(defn refresh-viewer-panel [root]
  (if (SwingUtilities/isEventDispatchThread)
    ;; then get away from EDT first for initial calculations of
    ;; grouped-schedule:
    (do 
      (future
        (refresh-viewer-panel-body root)))
    (do 
      (refresh-viewer-panel-body root))))


