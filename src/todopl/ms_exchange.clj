(ns todopl.ms_exchange
  (:require [todopl.utilities :as utils])
  (:use (clj-time [core :exclude (extend)]))
  (:require [todopl.database :as db])
  (:use seesaw.mig)
  (:use seesaw.swingx)
  (:require [todopl.rounded-label :as rounded])
  (:require [todopl.lookfeel :as lookfeel])
  (:use (clj-time [coerce :exclude (extend)]))
  (:use seesaw.core)
  (:import [com.todopl.foreign.msexchange MSExchange]))

(defn get_msexchange_events
  [version from to username password email]
  (try
    (let [result (MSExchange/getEvents version from to username password email)
          version (.getLeft result)
          appointments (.getItems (.getRight result))
           iter (fn fnname [items]
               (if (not (empty? items))
                 (let [f (first items)
                       r (rest items)]
                   (do 
                     (.load f)
                     (db/add-foreign-meeting
                       (.getSubject f)
                       (.getStart f)
                       (.getEnd f)
                       (.getLocation f)
                       (.toString (.getId f))
                       (.getIsAllDayEvent f))
                     (fnname r))
                   :success)))]
      (db/delete-all-foreign-meetings)
      (db/add-exchange-version-to-preferences version)
      (iter appointments))
    (catch Exception e (do
                         (println "Was not able to get MS Exchange appointments")
                         ;; todo throw out dialog with error
                         (println (.getMessage e))
                         :failure))))

(defn clean-up-tasks-after-new-foreign-events []
  ;; (remove :after links from tasks; actually this just seems to work)
  ;; TODO
  ;; PROBABLY just do this with a clean delete meeting on the ID when getting
  ;; the ms_exchange events
  ;;
  ;; go through tasks and remove the :after ids that are set to ids that are no
  ;; longer present in the foreign items.
  )



(defn make-msexchange-panel []
  (let [title-label (rounded/rounded-label :text "")
        gpanel  (mig-panel 
                  :id :msexchange-panel
                  :constraints ["" "[right]20[left]" "[]30[]10[]"]
                  :background lookfeel/externaleventcolor
                  :items [[(label :icon "foreign.png") "h 40!"]["" "wrap"]
                          [(label :text "username" :tip "Enter your username" :foreground lookfeel/todopl-logo-background) ""] [(text  :id :username) "w 300!, wrap"]
                          [(label :text "password" :tip "Enter your password" :foreground lookfeel/todopl-logo-background) ""] [(password  :id :password) "w 300!, wrap"]
                          ])]
    (border-panel
      :id :msexchange-panel-with-buttons
      :background lookfeel/externaleventcolor
      :center
      (vertical-panel
        :items [gpanel
                (mig-panel 
                  :id :msexchange-buttons
                  :constraints ["" "" ""]
                  :background lookfeel/externaleventcolor
                  :items [
                          ["" "width 50%"]
                          [(lookfeel/sh-button :submit-button "Submit" "add.png") ""]
                          [(busy-label :id :foreignaccess :text "" :busy? false) ""]
                          ["" "width 50%"]])
                (mig-panel
                  :constraints ["" "[]push[]"]
                  :background lookfeel/externaleventcolor
                  :items [
                          ["" ""]
                          [(lookfeel/sh-hyperlink-dark "Close" :cancel-username-button) "wrap"]
                          ])]))))


(defn ms-exchange-input-create-frame 
  [root]
  (let [dialog
        (custom-dialog
          :title "Get MS Exchange Events"
          :on-close :dispose
          :modal? true
          :parent root
          :minimum-size [300 :by 200]
          :content (border-panel
                     :border 5
                     :hgap 5
                     :vgap 5
                     :center (make-msexchange-panel)
                     ))]
    (.setUndecorated dialog true)
    (.setAlwaysOnTop dialog true)
    dialog))


