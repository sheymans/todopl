;  Copyright (c) Dave Ray, 2012. All rights reserved.

;   The use and distribution terms for this software are covered by the
;   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;   which can be found in the file epl-v10.html at the root of this
;   distribution.
;   By using this software in any fashion, you are agreeing to be bound by
;   the terms of this license.
;   You must not remove this notice, or any other, from this software.

(ns todopl.rounded-label
  (:use [seesaw.core :only [label label-options]]
        [seesaw.options :only [apply-options]]
        [seesaw.options :only [apply-options option-map default-option]]
        [seesaw.widget-options :only [WidgetOptionProvider]]
        [seesaw.graphics :only [anti-alias]]))

(defn- rounded-label-proxy [paint]
  (proxy [javax.swing.JLabel] []
    (isOqaque [] false)
    (setOpaque [v])
    (paintComponent [g]
      (anti-alias g)
      (if paint (paint this g))
      (proxy-super paintComponent g))))

(def ^{:private true} RoundedLabel (class (rounded-label-proxy nil)))

(defn rounded-label
  "Create a label whose background is a rounded rectangle

  Supports all the same options as (seesaw.core/label).

  See:
    (seesaw.core/label)
  "
  [& opts]
  (let [radius 15
        paint (fn [^javax.swing.JLabel c ^java.awt.Graphics g]
                (doto g
                  (.setColor (.getBackground c))
                  (.fillRoundRect
                    0 0 (dec (.getWidth c)) (dec (.getHeight c)) 
                    radius radius)))
        widget (rounded-label-proxy paint)]
    (apply-options widget opts)))

(defn super-round-label
  [& opts]
  (let [radius 60
        paint (fn [^javax.swing.JLabel c ^java.awt.Graphics g]
                (doto g
                  (.setColor (.getBackground c))
                  (.fillRoundRect
                    0 0 (dec (.getWidth c)) (dec (.getHeight c)) 
                    radius radius)))
        widget (rounded-label-proxy paint)]
    (apply-options widget opts)))


