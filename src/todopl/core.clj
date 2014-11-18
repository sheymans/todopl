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


(ns todopl.core
  (:gen-class) ;; for standalone application
  (:require [todopl.view :as view])
  (:require [todopl.maintenance :as maintenance])
  (:require [todopl.lookfeel :as lookfeel])
  (:require [todopl.database :as db])
  (:require [todopl.common :as common])
  (:import [java.awt GraphicsEnvironment Frame Desktop])
  (:import [java.net URI])
  (:use seesaw.core)
  (:require [todopl.ms_exchange :as exchange])
  (:use seesaw.swingx)
  (:use seesaw.keystroke)
  (:use seesaw.border))

;; Make sure SWING adapts to the local environment (e.g., Mac OS X)
(native!)


;; FONTS and COLORS

(def normal-font (lookfeel/sh-normal-font))
(def title-font (lookfeel/sh-big-title-font))
;(def divider-color "#aaaaaa")

;; WIDGETS

(def menus-at-core-frame
  (let [a-help (action :handler common/a-help :name "Quick Overview" :tip "Some tips to start...")
        a-ack (action :handler common/a-ack :name "Acknowledgements" :tip "Go to the acknowledgements page...")
        a-license (action :handler common/a-license :name "License" :tip "Go to the License page..")]
    (menubar
      :items [(menu :text "Help" :items [a-help a-ack a-license])])))



;(defn maximize-frame!
;  [f]
;  (let [ge (GraphicsEnvironment/getLocalGraphicsEnvironment)]
;    (.setExtendedState f (bit-or (.getExtendedState f) Frame/MAXIMIZED_BOTH))
;    (.setSize f (. (. ge getMaximumWindowBounds) getSize))))

(defn maximize-frame!
  [f]
  (.setVisible f true)
  (.setExtendedState f (Frame/MAXIMIZED_BOTH)))

(defn make-frame 
  []
  (let [f (frame
            :title (db/title-main-frame)
            :size  [1000 :by 1000]
            :on-close :exit
            ;:icon "sheld-48.png"
            :icons lookfeel/sheld-icons
            ;:menubar menus-at-core-frame
            :content (view/make-viewer-panel))]
    (.setLocationRelativeTo f nil)
    (maximize-frame! f)
    f))


;; Behavior of the app
;;

(defn add-behaviors
  [root]
  (view/add-behavior-viewer-panel root)
  root)



;; Launch the app:


(defn -main [& args]
  (maintenance/startup)
  (invoke-later 
    (->
      (make-frame)
      (lookfeel/uimanager-behavior) 
      add-behaviors
      show!))
  (.addShutdownHook (Runtime/getRuntime) (Thread. maintenance/shutdown)))


