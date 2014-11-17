(ns todopl.preferences
  (:require [todopl.database :as db])
  (:require [todopl.maintenance :as maintenance])
  (:require [todopl.lookfeel :as lookfeel])
  (:require [todopl.utilities :as utils])
  (:use seesaw.core)
  (:use seesaw.mig)
  )



;;
;; Backend
;;


(defn get-hours-in-a-day
  []
  (let [prefs (db/get-preferences)
        start (:startday prefs)
        end (:endday prefs)
        diff (utils/minus-string-times end start)]
    (:hours diff)))

(defn get-minutes-in-a-day
  []
  (let [prefs (db/get-preferences)
        start (:startday prefs)
        end (:endday prefs)
        diff (utils/minus-string-times end start)]
    (+ (* 60 (:hours diff))
       (:minutes diff))))



(defn get-all-times
  []
  (map
    #(utils/convert-number-and-quarter-to-string-time
       (first %)
       (second %))
    (utils/create-list-of-hours-to-max-with-quarters 0 24)))


(defn get-sub-from-preferences
  []
  (let [
        prefs (db/get-preferences)
        start (:startday prefs)
        end (:endday prefs)
        all-times (get-all-times)
        iter (fn thisfnname [list akk keep-p]
               (cond (empty? list) (reverse akk)
                     (= (first list) end) (reverse (conj akk (first list)))
                     (= (first list) start) (thisfnname (rest list) (conj akk (first list)) true)
                     keep-p (thisfnname (rest list) (conj akk (first list)) true)
                     (not keep-p) (thisfnname (rest list) akk false)))]
    (iter all-times nil false)))


;;
;; UI
;;

(defn make-preferences-panel []
  (let [prefs (db/get-preferences)
        data-location-field (horizontal-panel
                                :background lookfeel/todopl-logo-background
                                :items [
                                (text :id :data :text (:data prefs))
                                (button :text "Select" :id :browse-preferences-directory-button)])
        hours-to-select-from (get-all-times)
        startday-box (let [box (combobox :id :startday :model hours-to-select-from)]
                       (selection! box (:startday prefs))
                       box)

        endday-box (let [box (combobox :id :endday :model hours-to-select-from)]
                     (selection! box (:endday prefs))
                     box)

        foreign-box (let [box (combobox :id :foreigntype :model ["no external calendar" "MS Exchange Server"])]
                      (selection! box (:foreigntype prefs)))

        gpanel  (mig-panel 
                  :id :preferences-edit-panel
                  :constraints ["" "[right]20[left]" "[]30[]10[]10[]10[]10[]10[]10[]"]
                  :background lookfeel/todopl-logo-background
                  :items [[(label :icon "preferences.png") "h 40!"]
                          [ ""  "w 600!, h 38!, span, wrap"]

                          [(label :text "data location" :tip "This field contains the directory where your data will be saved; and we're on the cloud. 'Look Ma, without hands!'" :foreground lookfeel/clouds) ""]  [data-location-field "w 600!,span, wrap"]
                          [(label :text "start of day" :tip "Choose the time your working days should start. No task will start before that time." :foreground lookfeel/clouds) ""] [startday-box "span, wrap"]
                          [(label :text "end of day" :tip "Choose the time your working days should end. No task will end after that time." :foreground lookfeel/clouds) "" ] [endday-box "span, wrap"]
                          [(label :text "exclude days" :tip "Tick the days on which you will not be working. No tasks will be scheduled on those days." :foreground lookfeel/clouds) ""] [(checkbox :background lookfeel/todopl-logo-background :id :mon :text "Monday" :selected? (:mon prefs)) ""] [(checkbox :background lookfeel/todopl-logo-background :id :tue :text "Tuesday" :selected? (:tue prefs)) ""] [(checkbox :background lookfeel/todopl-logo-background :id :wed :text "Wednesday" :selected? (:wed prefs)) ""] [(checkbox :background lookfeel/todopl-logo-background :id :thu :text "Thursday" :selected? (:thu prefs)) "wrap"]
                          ["" ""] [(checkbox :background lookfeel/todopl-logo-background :id :fri :text "Friday" :selected? (:fri prefs)) ""] [(checkbox :background lookfeel/todopl-logo-background :id :sat :text "Saturday" :selected? (:sat prefs)) ""] [(checkbox :background lookfeel/todopl-logo-background  :id :sun :text "Sunday" :selected? (:sun prefs)) "wrap"]
                  
                          [(label :text "external calendar" :tip "Select a external calendar that you would like to integrate in Todopl." :foreground lookfeel/clouds) ""]
                          [foreign-box "w 300!, span, wrap"]
                          [(label :text "email" :tip "Fill in your email address associated with this external calendar" :foreground lookfeel/clouds) ""] [(text  :id :email :text (:email prefs)) "w 300!, span, wrap"]
                          ]
                          )]
    (border-panel
      :id :preferences-panel
      :background lookfeel/todopl-logo-background
      :center
      (vertical-panel
        :background lookfeel/todopl-logo-background
        :items [
                gpanel
                (mig-panel 
                  :id :edit-preferences-buttons
                  :constraints ["" "" ""]
                  :background lookfeel/todopl-logo-background
                  :items [
                          ["" "width 50%"]
                          [(lookfeel/sh-button :update-preferences-button "Update preferences" "add.png") ""]
                          [(lookfeel/sh-button :update-schedule-and-preferences-button "Update preferences and calculate schedule" "add_with_refresh.png") ""]
                          ["" "width 50%"]])
                (mig-panel
                  :constraints ["" "[]push[]"]
                  :background lookfeel/todopl-logo-background
                  :items [
                          ["" ""]
                          [(lookfeel/sh-hyperlink "Close" :cancel-preferences-button) "wrap"]
                          ])]))))



(defn make-preferences-frame
  [root]
  (let [dialog (custom-dialog
                 :title "Preferences"
                 :on-close :dispose
                 :modal? true
                 :parent root
                 :minimum-size [700 :by 350]
                 :content (border-panel
                            :border 5
                            :hgap 5
                            :vgap 5
                            :center (make-preferences-panel)
                            ))]
    (.setUndecorated dialog true)
    (.setAlwaysOnTop dialog true)
    dialog))


;;
;;


(defn gracefully-revert-to-new-data
  [new-preferences]
  (let [old-dir (:data (db/get-preferences)) ;; so call this only before you do a create-preferences which will destroy the old preferences.
        old-externals (:foreigntype (db/get-preferences))
        new-externals (:foreigntype new-preferences)
        new-dir (:data new-preferences)]
    (when (not (= old-dir new-dir))
      ;; the data directory changed, dump what you have in memory to the old
      ;; file and bootstrap from new directory
        (do 
          (maintenance/persist old-dir)
          ;; first clear the in-memory (the below will namely be added)
          (db/clear-all)
          (maintenance/bootstrap-tasks new-dir)
          (maintenance/bootstrap-meetings new-dir))
          ; do *not* bootstrap the schedule (that will go terribly wrong if you
          ; reduced the time for example)
          ;(maintenance/bootstrap-schedule new-dir))
      )
    ;; the external details where changed
    (when (not (= old-externals new-externals))
      (do 
        (db/delete-all-foreign-meetings)
        (maintenance/persist-meetings new-dir)))))


