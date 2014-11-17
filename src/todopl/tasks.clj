(ns todopl.tasks
  (:require [todopl.jcalendar :as cal])
  (:require [todopl.database :as db])
  (:require [todopl.preferences :as preferences])
  (:require [todopl.scheduler :as scheduler])
  (:require [todopl.lookfeel :as lookfeel])
  (:require [todopl.rounded-label :as rounded])
  (:require [todopl.utilities :as utils])
  (:use seesaw.mig)
  (:use seesaw.color)
  (:use seesaw.core))


;; make a widget for allowing to enter the undisturbed working minutes

(defn make-uwm-panel
  []
  (let [hour-panel
        (let [hours-to-select-from (utils/create-list-of-hours-to-max 0 (preferences/get-hours-in-a-day))]
          (combobox :id :hours-uwm :model hours-to-select-from))
        minute-panel
        (combobox :id :minutes-uwm :model [0 15 30 45])]
    ;; set the an example value (this will determine the width)
    (.setPrototypeDisplayValue hour-panel "23")
    [["" ""] ["" ""]
     [(horizontal-panel :background lookfeel/lighttodo :items [hour-panel " hours"]) ""]
     [(horizontal-panel :background lookfeel/lighttodo :items [minute-panel " minutes"]) "wrap"]]))


(defn make-emfc-panel
  []
  (let [; month-panel 
        ; (combobox :id :months-emfc :model (utils/create-list-of-hours-to-max 0 12))
        day-panel
        (combobox :id :days-emfc :model (utils/create-list-of-hours-to-max 0 5))
        hour-panel
        (let [hours-to-select-from (utils/create-list-of-hours-to-max 0 23)]
          (combobox :id :hours-emfc :model hours-to-select-from))
        minute-panel
        (combobox :id :minutes-emfc :model [0 15 30 45])]
    ;; set the an example value (this will determine the width)
    (.setPrototypeDisplayValue hour-panel "23")
    [; [(horizontal-panel :background lookfeel/lighttodo :items [month-panel (label :text " months" :foreground lookfeel/clouds)]) ""]
     [(horizontal-panel :background lookfeel/lighttodo :items [day-panel (label :text " days" :foreground lookfeel/clouds)]) ""]
     [(horizontal-panel :background lookfeel/lighttodo :items [hour-panel (label :text " hours" :foreground lookfeel/clouds)]) ""]
     [(horizontal-panel :background lookfeel/lighttodo :items [minute-panel (label :text " minutes" :foreground lookfeel/clouds)]) "wrap"]
     ])) 


(defn make-task-date-panel
  []
  (let [
        hours-to-select-from (preferences/get-sub-from-preferences)
        combo (combobox :id :due_time :model hours-to-select-from)]

    (border-panel
      :id :task-date-panel
      :background lookfeel/lighttodo
      :center
      (horizontal-panel
        :background lookfeel/lighttodo
        :items [
                (cal/jdatechooser :id :due_day :background lookfeel/lighttodo) 
                combo]))))

(defn render-item-in-after-box
  [renderer item]
  (let [v (:value item)
        id (first v)
        type (second v)
        item (db/get-item id type)
        text (if item (:title item) "--")]
    (cond (= type "Meeting") (config! renderer :text text  :background lookfeel/darkmeeting)
          (= type "ToDo") (config! renderer :text text :background lookfeel/darkdeadline)
          :else (config! renderer :text text))))

(defn comes-after-panel
  [id]
  (let [items (db/get-items-except-id id)
        empty-item [nil nil]]
    (combobox :id :after :model (into [empty-item] items) :renderer render-item-in-after-box)))



(defn unvalid-uwm-too-big-p
  [uwm]
  (let [max (preferences/get-minutes-in-a-day)
        diff (- uwm max)]
    (if (> diff 0)
      diff
      nil)))

(defn unvalid-uwm-too-small-p
  [uwm]
  (if (= uwm 0)
    0
    nil))

(defn unvalid-emfc-too-small-p
  [emfc]
  (= emfc 0))



(defn make-task-edit-panel
  [id]
  (let [title-label (rounded/rounded-label :text "")
        gpanel  (mig-panel 
                  :id :task-edit-panel
                  :constraints ["" "[right]20[left]" "[]30[]10[]10[]10[]10[]"]
                  :background lookfeel/lighttodo
                  :items (utils/append
                           [ 
                            [(label :icon "deadline.png") "h 40!"]
                            [ (do (lookfeel/apply-label-todo-styling! title-label true) (config! title-label :icon nil) title-label) "w 600!, h 30!, span, wrap"]
                            [(label :text "task" :tip "Describe your task" :foreground lookfeel/clouds) ""] [(text  :id :title) "w 600!, span, wrap"]
                            [(label :text "by when" :tip "When is your task due?" :foreground lookfeel/clouds) ""] [(make-task-date-panel) "w 600!,span, wrap"]
                            [(label :text "estimated time" :tip "How much time do you need to complete this task?" :foreground lookfeel/clouds) ""]]
                           (make-emfc-panel)
                           ; [[(label :text "work undisturbed" :tip "How long do you want to work undisturbed on each scheduled task for this deadline?") ""]]
                           ; (make-uwm-panel)
                           [[(label :text "comes after" :tip "What other tasks or events have to be completed before you can start to work on this task?" :foreground lookfeel/clouds)  ""] [(comes-after-panel id) "w 600!,span, wrap"]]
                           ))
        warning-panel-estimated-time-became-0 (mig-panel
                                                :id :warning-panel
                                                :background lookfeel/clouds
                                                :constraints ["" "[center]" ""]
                                                :items [ [(label :text "<html>This task is no longer scheduled as the
                                                                   estimated time became 0.<br> In other words, due to
                                                                         time passing, we assume you are done with it.<br> If
                                                                         that's not the case, please adjust the estimated time.</html>"
                                                                         :foreground
                                                                         lookfeel/pomegranate)]])
      warning-panel-unscheduled (mig-panel
                                                :id :warning-panel
                                                :background lookfeel/clouds
                                                :constraints ["" "[center]" ""]
                                                :items [ [(label :text "<html>This task is not scheduled.<br> Try the above right button to add and schedule it.</html>"
                                                                         :foreground
                                                                         lookfeel/pomegranate)]])
        warning-panel (let [task (db/is-existing-task? id)]
                        (if (and task (db/is-not-in-schedule-p id))
                          (if (db/not-in-schedule-because-estimated-time-became-0 task)
                            warning-panel-estimated-time-became-0
                            warning-panel-unscheduled)))
        task-buttons
                (mig-panel 
                  :id :edit-tasks-buttons
                  :constraints ["" "" ""]
                  :background lookfeel/lighttodo
                  :items [
                          ["" "width 50%"]
                          [(lookfeel/sh-button :just-add-task-button "Just add it" "add.png") ""]
                          [(lookfeel/sh-button :calculate-and-add-task-button "Add it and calculate schedule" "add_with_refresh.png") ""]
                          ["" "width 50%"]])
        delete-and-close-panel
               (mig-panel
                  :constraints ["" "[]push[]"]
                  :background lookfeel/lighttodo
                  :items [
                          (if id [(lookfeel/sh-hyperlink "Delete this task" :task-delete) ""] ["" ""])
                          [(lookfeel/sh-hyperlink "Close" :cancel-add-task-button) "wrap"]
                          ])]


    (border-panel
      :id :task-edit-panel-with-buttons
      :background lookfeel/lighttodo
      :center
      (vertical-panel
        :background lookfeel/lighttodo
        :items (if warning-panel
                 [
                  gpanel
                  task-buttons
                  warning-panel
                  delete-and-close-panel
                  ]
                 [
                  gpanel
                  task-buttons
                  delete-and-close-panel
                  ]
                 )))))


(defn make-task-create-frame 
  [root id]
  (let [dialog
        (custom-dialog
          :title "Create Deadline"
          :on-close :dispose
          :minimum-size [700 :by 350]
          :parent root
          :modal? true ;; this makes this a blocking dialog
          :content (border-panel
                     :border 5
                     :hgap 5
                     :vgap 5
                     :center (make-task-edit-panel id)
                     ))]
    ; to put it the left top corner in middle
    ;(.setLocationRelativeTo dialog root)
    (.setUndecorated dialog true)
    (.setAlwaysOnTop dialog true)
    dialog))




(defn make-task-edit-frame 
  [root id]
  (let [dialog 
        (custom-dialog
          :title "Edit Deadline"
          :on-close :dispose
          :modal? true
          :parent root
          :minimum-size [700 :by 350]
          :content (border-panel
                     :border 5
                     :hgap 5
                     :vgap 5
                     :center (make-task-edit-panel id)
                     ))]
    ;(.setLocationRelativeTo dialog root)
    (.setUndecorated dialog true)
    (.setAlwaysOnTop dialog true)
    dialog))


