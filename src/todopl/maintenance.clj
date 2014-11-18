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


(ns todopl.maintenance
  (:require [todopl.database :as db])
  (:require [todopl.utilities :as utils])
  (:require [todopl.locking :as lock])
  (:use (overtone [at-at :exclude (now) :as overtone])) ;; for scheduling processes at certain intervals
  (:import [java.lang Runtime])
  (:require [me.raynes.conch.low-level :as conch]))

;; to store the prolog process in
(def prolog_process (atom nil))


(defn get-tasks-meetings-dir []
    (:data (db/get-preferences)))

(defn get-backup-dir []
  (let [backupdir (str (get-tasks-meetings-dir) "/backup")
        filedir (java.io.File. backupdir)]
    (if (not (.exists filedir))
      (.mkdir filedir))
    backupdir))

(defn bootstrap-tasks [dir]
  (let [
        file (java.io.File. (str dir "/tasks.db"))]
    (when (.exists file)
      (with-open [r (java.io.PushbackReader. 
                      (java.io.FileReader. file))] 
        (let [rec (read r)]
          (when rec (db/add-tasks rec)))))))

(defn persist-tasks [dir]
  (let [
        file (java.io.File. (str dir "/tasks.db"))
        tasks (db/get-tasks)]
    (with-open [w (java.io.FileWriter. file)] 
      (binding [*out* w *print-dup* true] (prn tasks)))))

(defn bootstrap-meetings [dir]
  (let [
        file (java.io.File. (str dir "/meetings.db"))]
    (when (.exists file)
      (with-open [r (java.io.PushbackReader. 
                      (java.io.FileReader. file))] 
            (let [rec (read r)]
              (when rec (db/add-meetings rec)))))))



(defn persist-meetings [dir]
  (let [
        file (java.io.File. (str dir "/meetings.db"))
        meetings (db/get-meetings)]
    (with-open [w (java.io.FileWriter. file)] 
      (binding [*out* w *print-dup* true] (prn meetings)))))

(defn bootstrap-schedule [dir]
  (db/clear-schedule)
  (let [
        file (java.io.File. (str dir "/schedule.db"))]
    (when (.exists file)
      (with-open [r (java.io.PushbackReader. 
                      (java.io.FileReader. file))] 
        (let [rec (read r)]
          (when rec (db/add-schedule rec)))))))



(defn persist-schedule [dir]
  (let [
        file (java.io.File. (str dir "/schedule.db"))
        schedule (db/get-schedule)]
    (with-open [w (java.io.FileWriter. file)] 
      (binding [*out* w *print-dup* true] (prn schedule)))))



(defn persist-preferences []
  (let [prefs (db/get-preferences)
        file (java.io.File. (str (System/getProperty "user.home") "/.todopl/preferences.db"))]
    (with-open [w (java.io.FileWriter. file)] 
      (binding [*out* w *print-dup* true] (prn prefs)))))

(defn bootstrap-preferences []
  (let [dir-string (str (System/getProperty "user.home") "/.todopl")
        dir (java.io.File. dir-string)
        file (java.io.File. (str dir-string "/preferences.db"))]
    (if (and (.exists dir) (.exists file))
      (with-open [r (java.io.PushbackReader. 
                      (java.io.FileReader. file))] 
        (try 
          (let [rec (read r)]
            (if rec 
              (db/add-preferences rec)))
          ;; backward compatibility (no foreigntype in earlier versions)
          (if (not (:foreigntype (db/get-preferences)))
            (reset! db/preferences (assoc (db/get-preferences) :foreigntype "no external calendar")))

          (catch Exception e (do
                               (println "Failed to boostrap preferences; recreating...")
                               (db/add-preferences {:data dir-string
                     :startday "08:00" 
                     :offset (utils/get-offset) 
                     :endday "16:00"
                     :foreigntype "no external calendar"
                     :mon false
                     :sat true 
                     :thu false 
                     :wed false 
                     :fri false 
                     :tue false 
                     :sun true})))))

      (do 
        (if (not (.exists dir)) (.mkdir dir))
        (let [prefs {:data dir-string
                     :startday "08:00" 
                     :offset (utils/get-offset) 
                     :endday "16:00"
                     :foreigntype "no external calendar"
                     :mon false
                     :sat true 
                     :thu false 
                     :wed false 
                     :fri false 
                     :tue false 
                     :sun true}]
          (db/add-preferences prefs))))))



(defn is-windows [osname]
  (>= (.indexOf osname "win") 0))

(defn is-mac [osname]
  (>= (.indexOf osname "mac") 0))

(defn is-linux [osname]
  (>= (.indexOf osname "linux") 0))

(defn get-os-name []
  (.toLowerCase (System/getProperty "os.name")))

(defn kill-already-running []
  ;; SYSTEM-DEPENDENT CRAP
  (let [os (get-os-name)
        rt (Runtime/getRuntime)]
    (cond
      (is-windows os) (do 
                        (println "Making sure to kill already running background servers using taskkill command")
                        (let [winproc (.exec rt "taskkill /f /t /im todopl_server")]
                          ;; under windows you have to wait otherwise the
                          ;; subsequent start of the prolog server also gets
                          ;; killed
                          (println (.waitFor winproc))))
      (is-mac os) (do
                    (println "Making sure to kill already running background servers using killall command")
                    (.exec rt "killall todopl_server"))
      (is-linux os) (do
                      (println "Making sure to kill already running background servers using killall command")
                      (.exec rt "killall todopl_server"))
      :else (println "Cannot do a local precautionary kill of todopl_server as I do not know which system you are running on."))))

(defn start-prolog-server []
  (let [ server-location (str (System/getProperty "user.dir") "/todopl_server")]
    ;; start it and set the process object
    (kill-already-running)
    (reset! prolog_process (conch/proc server-location :env {"DYLD_FALLBACK_LIBRARY_PATH" (str (System/getProperty "user.dir"))}))))




(defn shutdown-prolog-server []
  ;; get the prolog process from the global variable
  (let [process @prolog_process]
    ;; destroy it
    (conch/destroy process)
    ;; and set the global variable back to nil
    (reset! prolog_process nil)))

(defn bootstrap []
  (println "boostrapping preferences...")
  (bootstrap-preferences)
  ;; after having bootstrapped the preference you know the data file
  (let [dir (get-tasks-meetings-dir)]
    (try 
      (do 
      (println "boostrapping tasks...")
      (bootstrap-tasks dir)
      (println "boostrapping meetings...")
      (bootstrap-meetings dir)
      (println "boostrapping schedule...")
      (bootstrap-schedule dir))
      (catch Exception e (do 
                           (println (str "Failed to bootstrap; trying backup: " (.getMessage e) ))
                           (bootstrap-tasks (get-backup-dir))
                           (bootstrap-meetings (get-backup-dir))
                           (bootstrap-schedule (get-backup-dir)))))))


(defn persist [dir]
  (locking db/global_lock
  (if (not (lock/is-file-write-locked?))
    (do 
      (lock/lock-file-write)
      (println "Preparing for persist to " dir)
      (println "persisting preferences...")
      (persist-preferences)
      (println "persisting tasks...")
      (persist-tasks dir)
      (println "persisting meetings...")
      (persist-meetings dir)
      (println "persisting schedule...")
      (persist-schedule dir)
      (lock/unlock-file-write))
    (println "Failed to persist to" dir))))

(defn startup []
  ;; bootstrap data
  (println "boostrapping data...")
  (bootstrap)
  ;; start up prolog engine
  (println "starting up prolog...")
  (start-prolog-server)
  (println "I'm alive. Hi."))

(defn shutdown []
  (let [dir (get-tasks-meetings-dir)]
    ;; shut down the prolog server
    (println "shutting down prolog...")
    (shutdown-prolog-server)
    (overtone/show-schedule db/background_pool)
    (println "ask background processes to finish...")
    (locking db/global_lock
    (overtone/stop-and-reset-pool! db/progressbar_pool)
    (overtone/stop-and-reset-pool! db/background_pool))
    (overtone/show-schedule db/background_pool)
    ;; persist all
    (println "persisting all data...")
    (persist dir)
    (println "I'm dead. Bye.")))


(def write_to_backup (atom nil))

(defn persist-to-files []
  (let [backupdir (get-backup-dir)
        normaldir (get-tasks-meetings-dir)]
    (if @write_to_backup
      (do 
        (println "Writing to backup") 
        (persist backupdir)
        (reset! write_to_backup nil))
      (do
        (println "Writing to normal dir")
        (persist normaldir)
        (reset! write_to_backup true)))))


