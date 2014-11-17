(ns todopl.lookfeel
  (:use seesaw.core)
  (:use seesaw.color)
  (:require [todopl.utilities :as utils])
  (:use seesaw.font)
  (:import [java.awt.event MouseAdapter MouseMotionAdapter])
  (:import [javax.swing.plaf ColorUIResource])
  (:import [javax.swing JOptionPane UIManager]))



;;; flat design colors

(def todopl-logo-background (color 68 68 68 255))
(def midnightblue "#2c3e50")
(def peterriver "#3498db")
(def belizehole "#2980b9")
(def emerald "#2ecc71")
(def nephritis "#27ae60")
(def carrot "#e67e22")
(def pumpkin "#d35400")
(def alizarin "#e74c3c")
(def pomegranate "#c0392b")
(def clouds "#ecf0f1")
(def wetasphalt "#34495e")
(def greensea "#16a085")
(def turquoise "#1abc9c")
(def amethyst "#9b59b6")
(def wisteria "#8e44ad")
(def asbestos "#7f8c8d")
(def lightorange "#f4c69d")
(def lightgrey "#bababa")

;;; using them
;;;
(def upper-buttons-color todopl-logo-background)

(def darkmeeting pumpkin)
(def lightmeeting carrot)
(def lightnotinschedule peterriver)
(def darknotinschedule belizehole)
(def lightoverdue alizarin)
(def darkoverdue pomegranate)
(def lighttodo midnightblue)
(def darktodo nephritis)
(def lightdeadline wetasphalt)
(def darkdeadline nephritis)
(def externaleventcolor "#EEEEEE")

(def halfhourborder "#EEEEEE")
;(def timecolor "#BCBCBC")
(def timecolor "#a9a9a9")
(def fatcellcolor "#444444")


;;; FONTS

(defn sh-day-title-font [] (font :name "LUCIDA" :style :bold :size 18))
(defn sh-normal-font [] (font :name "LUCIDA" :style :plain :size 12))
(defn sh-italic-font [] (font :name "LUCIDA" :style :italic :size 12))
(defn sh-big-title-font [] (font :name "LUCIDA" :style :bold :size 13))
(defn sh-huge-title-font [] (font :name "LUCIDA" :style :bold :size 20))
(defn sh-link-font [] (font :name "LUCIDA" :style :italic :size 10))
(defn sh-time-font [] (font :name "LUCIDA" :style :plain :size 10))



;;; Set global look and feel for stuff:
;;;

;; Tooltips

(defn uimanager-behavior
  [frame]
  (UIManager/put "ToolTip.foreground" (color fatcellcolor))
  (UIManager/put "ToolTip.background" (color clouds))
  ;; Default text of labels should be darky grey
  (UIManager/put "Label.foreground" (ColorUIResource. (color fatcellcolor)))
  frame
  )

; http://www.apl.jhu.edu/~hall/java/Swing-Tutorial/Swing-Tutorial-JButton.html
(defn sh-button
  [id tip icon]
  (let [b (button :tip tip :id id :icon icon :background todopl-logo-background)]
    (.setFocusPainted b false)
    ;; next 2 lines for windows (avoid their fucked up gradient and what not on
    ;; the buttons:
    (.setBorderPainted b false) 
    (.setContentAreaFilled b false)
    b))


(defn sh-hyperlink
  [text id]
  (let [l (label :text text :id id :cursor :hand)]
    (config! l :font (sh-link-font) :foreground clouds)
    l))

(defn sh-hyperlink-dark
  [text id]
  (let [l (label :text text :id id :cursor :hand)]
    (config! l :font (sh-link-font) :foreground todopl-logo-background)
    l))


(defn sh-alert
  [source message]
  (let [icon (icon "alert.png")]
    (JOptionPane/showMessageDialog (to-widget source) message "Todopl Error" JOptionPane/ERROR_MESSAGE icon)))


(defn apply-label-foreign-styling! [label light]
  (let [old-icon (config label :icon)]
    (config! label :icon "foreign-small-background.png")
    (when (and old-icon
               (not (= (.toString old-icon)
                       (.toString (config label :icon)))))
      (config! label :icon old-icon))

    (if light
      (config! label :background externaleventcolor)
      (config! label :background darkmeeting))))



(defn apply-label-meeting-styling! [label subtype light]
  (if (= subtype :foreign)
    (apply-label-foreign-styling! label light)
  (let [old-icon (config label :icon)]
    (config! label :icon "meeting-small-white.png")
    (when (and old-icon
               (not (= (.toString old-icon)
                       (.toString (config label :icon)))))
      (config! label :icon old-icon))

    (if light
      (config! label :background lightmeeting)
      (config! label :background darkmeeting)))))


(defn apply-label-todo-styling! [label light]
  (config! label :icon "empty.png")
  (if light
    (do 
      (config! label :background lighttodo))
    (do 
      (config! label :background darktodo))
    ))

(defn apply-label-deadline-styling! [label light]
  (config! label :icon "deadline-small-white.png")
  (if light
    (do 
      (config! label :background lightdeadline)
      ; (config! label :icon "deadline-small.png")
      ;(config! label :foreground fatcellcolor)
      )
    (do 
      (config! label :background darkdeadline)
      ;      (config! label :icon "deadline-small-white.png")
      ;      (config! label :foreground clouds)
      )
    ))

(defn apply-label-past-styling! [label light]
  (if light
    (do 
      (config! label :background lightoverdue))
    (do 
      (config! label :background darkoverdue))
    ))

(defn apply-label-not-in-schedule-styling! [label light]
  (if light
    (do 
      (config! label :background lightnotinschedule)
      )
    (do 
      (config! label :background darknotinschedule))
    ))

(defn apply-label-styling! [label type subtype past-p not-in-schedule-p light]
  (case type
    "ToDo" (apply-label-todo-styling! label light)
    "Meeting" (apply-label-meeting-styling! label subtype light)
    :deadline (apply-label-deadline-styling! label light))
  (cond 
    (and past-p (not (= subtype :foreign))) (apply-label-past-styling! label light)
    (and not-in-schedule-p
        (not (= type "ToDo"))
        (not (= subtype :foreign)))  (apply-label-not-in-schedule-styling! label light)
    :else true))

(defn ind-title
  [title]
  (let [title (str " " title)]
    (if (> (count title) 75)
      (str (subs title 0 75) "...")
      title)))

;; For glasspane darkening:
;; from
;; http://stackoverflow.com/questions/8048416/turning-a-jframe-black-when-opening-another-window

(defn- dark-component
  []
  (proxy [javax.swing.JComponent] []
    (paintComponent [g]
      (.setColor g (color 0 0 0 200))
      (.fillRect g 0 0 (.getWidth this) (.getHeight this))
      (proxy-super paintComponent g)
      )))

(defn- mouseadapter [] (proxy [MouseAdapter] []))
(defn- mousemotionadapter [] (proxy [MouseMotionAdapter] []))

;; as in
;; http://www.java-tips.org/java-se-tips/javax.swing/how-to-block-mouse-and-key-events-in-an-applic.html
(defn ignore-all-events-on-glasspane
  [glasspane]
 (doto glasspane
   (.addMouseListener (mouseadapter))
   (.addMouseMotionListener (mousemotionadapter))))

(defn dim-the-lights 
  [frame]
  (let [root-pane (.getRootPane frame)]
    (.setGlassPane root-pane (dark-component))
    (ignore-all-events-on-glasspane (.getGlassPane root-pane))
    (.setVisible (.getGlassPane root-pane) true)))


(defn switch-on-the-lights
  [frame]
  (.setVisible (.getGlassPane (.getRootPane frame)) false))

(defn apply-lookfeel-frame
  [frame parent]
  (.setLocationRelativeTo frame parent)
  (config! (config frame :content) :background :white)
  frame)

(defn center
  [frame parent]
  (.setLocationRelativeTo frame parent)
  frame)



;;; for converting set of icons to appropriate
;(defn- ^java.awt.Image frame-icon-converter-tmp [value]
;  (cond
;    (instance? java.awt.Image value) value
;    :else (let [^javax.swing.ImageIcon i (make-icon value)]
;            (.getImage i))))
;

;(defn- ^java.awt.Image frame-icon-converter-tmp [value]
;  (cond
;    (instance? java.awt.Image value) value
;    :else (let [^javax.swing.ImageIcon i (icon value)]
;            (.getImage i))))
;
(def sheld-icons ["icons/sheld-48.png" "icons/sheld-32.png" "icons/sheld-24.png" "icons/sheld-16.png"])

;(defn set-icons-on-frame
;  [window]
;    (.setIconImages window (map frame-icon-converter-tmp sheld-icons))
;    window)
;


;;;; Resource Strings used
;;;;

(defn uwm-too-big-string
  [too-big]
  (str "Your working day is too short to fit your undisturbed working time.\n" "Please reduce your it with " too-big " minutes."))

(defn uwm-too-small-string
  []
  (str "Please increase your undisturbed working time with at least 15 minutes."))


(defn due-date-in-past-string
  []
  (str "You indicated that this task is due in the past.\nThat's not going to work (reminder: it is now " (utils/java-now) ")"))

(defn no-email
  []
  (str "You did not fill in an email address that matches the external calendar.\n Please go to your preferences."))

(defn no-external
  []
  (str "You did not specify an external calendar.\n Please go to your preferences."))

(defn emfc-too-small-string
  []
  (str "Your estimated time seems not realistic."))


(defn emfc-smaller-than-uwm-string
  []
  (str "The time you estimate this deadline to consume is less than the time you want to spend undisturbed on it."))

(defn today-before-from-string
  []
  (str "Your event seems to end before it really started."))

(defn data-directory-not-exists-string
  []
  (str "The data directory you entered does not exist. Try the `Select' button."))


(defn time_limit_exceeded_string
  []
  (str "We did not manage to calculate a schedule within a reasonable amount of time.\nConsider reducing the estimated time or extending the deadline of one or more tasks."))

(defn no_schedule_found_string
  []
  (str "No schedule exists that fits your current tasks and events."))

(defn unknown_exception_string
  []
  (str "Something happened.\nWe do not know what.\nPlease file a bug report."))

(defn too-far-in-future-string
  []
  (str "We only support due dates that lie less than a year from now."))

(defn ms_exchange_error
  []
  (str "The MS Exchange Server did not play nice.\n Are you online? Are you normally behind a VPN?\n If so consider retrying your username and password."))






