(ns todopl.meetings
  (:require [todopl.jcalendar :as cal])
  (:require [todopl.database :as db])
  (:require [todopl.utilities :as utils])
  (:require [todopl.lookfeel :as lookfeel])
  (:require [todopl.rounded-label :as rounded])
  (:require [todopl.preferences :as preferences])
  (:use seesaw.mig)
  (:use seesaw.core))


; (defn make-from-date-panel
;   []
;   (let [
;         prefs (db/get-preferences)
;         hours-to-select-from (preferences/get-all-times)
;         frombox (combobox :id :from_time :model hours-to-select-from) 
;         tobox (combobox :id :to_time :model hours-to-select-from)]
; 
;     (selection! frombox (:startday prefs))
;     (selection! tobox (:endday prefs))
; 
;     (mig-panel
;       :id :from-date-panel
;       :constraints ["" "[left][center][center][center]"]
;       :background lookfeel/lightmeeting
;       :items [
;               [(cal/jdatechooser :id :from_day :background lookfeel/lightmeeting) ""]
;               [ frombox ""]
;               [(label " to ") ""]
;               [ tobox "wrap"]])))

(defn make-from-date-panel
  []
  (let [
        prefs (db/get-preferences)
        hours-to-select-from (preferences/get-all-times)
        frombox (combobox :id :from_time :model hours-to-select-from) 
        tobox (combobox :id :to_time :model hours-to-select-from)]

    (selection! frombox (:startday prefs))
    (selection! tobox (:endday prefs))


    (border-panel
      :id :from-date-panel
      :background lookfeel/lightmeeting
      :center
      (horizontal-panel
       :background lookfeel/lightmeeting
       :items  [
              (cal/jdatechooser :id :from_day :background lookfeel/lightmeeting)
              frombox
              (label :text " to " :foreground lookfeel/clouds)
              tobox]))))



(defn make-meeting-edit-panel
  [id]
  (let [title-label (rounded/rounded-label :text "")
        gpanel  (mig-panel 
                  :id :meeting-edit-panel
                  :constraints ["" "[right]20[left]" "[]30[]10[]"]
                  :background lookfeel/lightmeeting
                  :items [[(label :icon "meeting.png") "h 40!"]
                          [ (do (lookfeel/apply-label-meeting-styling! title-label nil true) (config! title-label :icon nil) title-label) "w 600!, h 30!, wrap"]
                          [(label :text "title" :tip "Describe your event" :foreground lookfeel/clouds) ""] [(text  :id :title) "w 600!, wrap"]
                          [(label :text "when" :tip "When is your event?" :foreground lookfeel/clouds) ""] [(make-from-date-panel) "w 600!,  wrap"]
                          [(label :text "where" :tip "Where is your event?" :foreground lookfeel/clouds) ""] [(text :id :where) "w 600!,  wrap"]
                          [(label :text "who" :tip "Who participates in your event?" :foreground lookfeel/clouds) ""] [(text :id :who) "w 600!,  wrap"]
                          ])]
    (border-panel
      :id :meeting-edit-panel-with-buttons
      :background lookfeel/lightmeeting
      :center
      (vertical-panel
        :items [gpanel
                (mig-panel 
                  :id :edit-meetings-buttons
                  :constraints ["" "" ""]
                  :background lookfeel/lightmeeting
                  :items [
                          ["" "width 50%"]
                          [(lookfeel/sh-button :just-add-meeting-button "Just add it" "add.png") ""]
                          [(lookfeel/sh-button :calculate-and-add-meeting-button "Add it and calculate schedule" "add_with_refresh.png") ""]
                          ["" "width 50%"]])
                (mig-panel
                  :constraints ["" "[]push[]"]
                  :background lookfeel/lightmeeting
                  :items [
                          (if id [(lookfeel/sh-hyperlink "Delete this event" :meeting-delete) ""] ["" ""])
                          [(lookfeel/sh-hyperlink "Close" :cancel-add-meeting-button) "wrap"]
                          ])]))))





(defn make-meeting-create-frame 
  [root id]
  (let [dialog
        (custom-dialog
          :title "Create Event"
          :on-close :dispose
          :modal? true
          :parent root
          :minimum-size [700 :by 350]
          :content (border-panel
                     :border 5
                     :hgap 5
                     :vgap 5
                     :center (make-meeting-edit-panel id)
                     ))]
    ;(.setLocationRelativeTo dialog root)
    (.setUndecorated dialog true)
    (.setAlwaysOnTop dialog true)
    dialog))

(defn make-meeting-edit-frame 
  [root id]
  (let [dialog 
        (custom-dialog
          :title "Edit Meeting"
          :on-close :dispose
          :modal? true
          :parent root
          :minimum-size [700 :by 350]
          :content (border-panel
                     :border 5
                     :hgap 5
                     :vgap 5
                     :center (make-meeting-edit-panel id)
                     ))]
    ; (.setLocationRelativeTo dialog root)
    (.setUndecorated dialog true)
    (.setAlwaysOnTop dialog true)
    dialog))



