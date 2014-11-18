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

(ns todopl.common
  (:require [todopl.lookfeel :as lookfeel])
  (:require [todopl.rounded-label :as rounded])
  (:use seesaw.core)
  (:use seesaw.mig)
  (:import [java.awt Desktop])
  (:import [java.net URI]))


;;; Help frame
;;;
;;;

(def height-cell 36)

(defn make-help-panel
  []
  (let [deadline-button (lookfeel/sh-button :add-a-deadline-button-docu "Add a task" "deadline.png") 
        meeting-button (lookfeel/sh-button :add-a-meeting-button-docu "Add an event" "meeting.png")
        previous-button (lookfeel/sh-button :previous-button-docu "Past" "previous.png") 
        next-button (lookfeel/sh-button :next-button-docu "Upcoming" "next.png") 
        preferences-button (lookfeel/sh-button :preferences-button-docu "Update preferences" "preferences.png") 
        refresh-button (lookfeel/sh-button :refresh-schedule-button-docu "Refresh schedule" "refresh.png")
        deadline-label (rounded/rounded-label :text "a deadline" :foreground lookfeel/clouds :font (lookfeel/sh-normal-font))
        todo-label (rounded/rounded-label :text "a task toward accomplishing a deadline" :foreground lookfeel/clouds :font (lookfeel/sh-normal-font))
        meeting-label (rounded/rounded-label :text "a meeting" :foreground lookfeel/clouds :font (lookfeel/sh-normal-font))
        any-label (rounded/rounded-label :foreground lookfeel/clouds :font (lookfeel/sh-normal-font))
        any-label2 (rounded/rounded-label :foreground lookfeel/clouds :font (lookfeel/sh-normal-font))

        panel   [
                 ["" "gap right 25px"] [deadline-button "gap right 20px"] [(label :text "Click this button to add a deadline.") "wrap"]
                 ["" "gap right 25px"][meeting-button "gap right 20px"] [(label :text "Click this button to add a meeting.") "wrap"]
                 ["" "gap right 25px"][previous-button "gap right 20px"] [(label :text "Click this button to see your past deadlines and meetings.") "wrap"]
                 ["" "gap right 25px"][next-button "gap right 20px"] [(label :text "Click this button to see your upcoming schedule.") "wrap"]
                 ["" "gap right 25px, gap bottom 50px"][preferences-button "gap right 20px, gap bottom 50px"] [(label :text "<html>Click this button to go to your preferences and update<br> the time you plan to work toward deadlines.</html>") "gap bottom 50px, wrap"]
                 [(label :text "Once you have added something, try this button:") "span, gap bottom 25px, wrap"]
                 ["" "gap right 25px"][refresh-button "gap right 20px, gap bottom 50px"] [(label :text "<html>It will schedule your tasks and help you meet your deadlines<br> without missing your meetings.</html>") "wrap"]
                 ;; now some colors!
                 [(label :text "Your schedule will then contain different types of items:") "span, gap bottom 25px, wrap"]
                 ["" "gap right 25px"]["" ""][(do 
                                                (lookfeel/apply-label-deadline-styling! deadline-label true)
                                                deadline-label)
                                              (str "h " height-cell "!, w 300!, wrap")]
                 ["" "gap right 25px"]["" ""][(do 
                                                (lookfeel/apply-label-todo-styling! todo-label true)
                                                todo-label)
                                              (str "h " height-cell "!, w 300!, wrap")]
                 ["" "gap right 25px"]["" ""][(do 
                                                (lookfeel/apply-label-meeting-styling! meeting-label nil true)
                                                meeting-label)
                                              (str "h " height-cell "!, w 300!, wrap")]
                 ["" "gap right 25px"]["" ""][(do 
                                                (lookfeel/apply-label-past-styling! any-label true)
                                                (config! any-label :icon "empty.png")
                                                (config! any-label :text "any item that is in the past") 
                                                any-label)
                                              (str "h " height-cell "!, w 300!, wrap")]
                 ["" "gap right 25px"]["" ""][(do 
                                                (lookfeel/apply-label-not-in-schedule-styling! any-label2 true)
                                                (config! any-label2 :icon "empty.png")
                                                (config! any-label2 :text "any item that is not (yet) scheduled") 
                                                any-label2)
                                              (str "h " height-cell "!, w 300!, wrap")]
                 ]]
    panel))

(declare menus-at-help-frame) ;; mutually recursive
(defn make-help-frame 
  [root]
  (let [dialog
        (custom-dialog
          :title "Help"
          :on-close :dispose
          :minimum-size [700 :by 350]
          :modal? true ;; this makes this a blocking dialog
;          :menubar menus-at-help-frame
          :content (scrollable
                     (mig-panel
                       :id :help-panel
                       :constraints ["center"]
                       :background :white
                       :items (make-help-panel))))]
;    (.setUndecorated dialog true)
    dialog))



;;; Constructing the menus

(defn goto
  [url]
  (let [uri (URI/create url)
        desktop (Desktop/getDesktop)]
    (.browse desktop uri)))

(defn a-ack
  [e]
  (let [url "http://www.todopl.com/acknowledgements.html"]
    (goto url)))

(defn a-license
  [e]
  (let [url "http://www.gnu.org/licenses/gpl.html"]
    (goto url)))

(defn a-sheldy
  [e]
  (let [url "http://www.todopl.com/"]
    (goto url)))

(defn a-twitter
  [e]
  (let [url "https://twitter.com/Todopl"]
    (goto url)))

(defn a-google
  [e]
  (let [url "https://plus.google.com/+Todopl"]
    (goto url)))

(defn a-facebook
  [e]
  (let [url "https://facebook.com/Todopl"]
    (goto url)))



(defn a-help
  [e]
  (let [url "http://www.todopl.com/start.html"]
    (goto url)))
;;  (lookfeel/dim-the-lights (to-root e))
;  (invoke-now
;    (-> (make-help-frame (to-root e))
;      pack!
;      (lookfeel/apply-lookfeel-frame (to-root e))
;      show!)))
;

(def menus-at-help-frame
  (let [a-help (action :handler a-help :name "Quick Overview" :tip "Some tips to start...")
        a-ack (action :handler a-ack :name "Acknowledgements" :tip "Go to the acknowledgements page...")
        a-license (action :handler a-license :name "License" :tip "Go to the License page..")]
    (menubar
      :items [(menu :text "Help" :items [a-help a-ack a-license])])))


