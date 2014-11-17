(ns todopl.scheduler
  (:require [todopl.database :as db])
  (:require [todopl.utilities :as utils])
  (:require [clj-http.client :as client]) ;; for doing http posts and gets from prolog scheduler
  (:require [cheshire.core :refer :all :as json])
  (:use (clj-time [core :exclude (extend)]))
  (:use (clj-time [coerce :exclude (extend)]))
  (:use seesaw.core)
  (:use seesaw.table)
  (:use seesaw.mig)
  (:use seesaw.swingx)
  ) ;; for generating and parsing json/

;;
;; Backend Stuff
;;

(defn deadline-is-in-past-p
  [deadline]
  (utils/java-date-is-in-the-past-p (utils/convert-due-day-due-time-to-duedate (:due_day deadline)
                                                                               (:due_time deadline)
                                                                               (:offset deadline))
                                    (:offset deadline)))

(defn remove-tasks-in-past
  [tasks]
  (remove deadline-is-in-past-p tasks))


(defn remove-tasks-with-emfc-0
  [tasks]
  (remove #(= (:emfc %) 0) tasks))

(defn remove-with-just-add
  [items]
  "Get rid of items that have :just_add field true."
  (remove #(= (:just_add %) true) items))

(defn get-json-tasks
  []
  ;; also do not include tasks that are in past or that have emfc 0 (we do not
  ;; want to schedule those)
  (let [jtasks (do 
                 (db/sort-tasks) 
                 (remove-tasks-in-past (db/get-tasks)))
        jtasks (remove-tasks-with-emfc-0 jtasks)
        jtasks (remove-with-just-add jtasks)]
    (generate-string jtasks)))

(defn get-json-meetings
  []
  (let [jmeetings (db/get-meetings)
        jmeetings (remove-with-just-add jmeetings)]
    (generate-string jmeetings)))

(defn get-json-preferences
  []
  (let [jprefs (db/get-preferences)]
    (generate-string jprefs)))

(defn transform-schedule-to-keys
  [sched]
  (if (empty? sched)
    nil
    (let [f (first sched)
          offset (f "offset")
          item_id (f "item_id")
          type (f "type")]
  (into [ {:title (if (= type "ToDo")
                    (db/get-task-title item_id)
                    (db/get-meeting-title item_id))
           :from (utils/create-date (f "from") offset)
           :to (utils/create-date (f "to") offset)
           :type (f "type")
           :item_id item_id
           :offset (f "offset")}]
        (transform-schedule-to-keys (rest sched))))))




;; Calling Prolog Server to Schedule the Tasks:

(defn prolog-schedule [tasks preferences meetings]
  ;; note that tasks is already in json format (see get-json-tasks in
  ;; database/core.clj)
  (let [json_input (str "{ \"json\": { \"tasks\":" tasks ", \"preferences\":" preferences ", \"meetings\":" meetings "}}")
        answer_prolog (client/post "http://localhost:5000/prolog_schedule"
                                   {:body json_input  
                                    :content-type :json
                                    :accept :json})]
    answer_prolog))

(defn valid-schedule-p [sched]
  (let [parsed ((json/parse-string sched) "json")]
    (cond 
      (= parsed "time_limit_exceeded") nil
      (= parsed "no_schedule_found") nil
      (= parsed "unknown_exception") nil
      :else true)))


(defn scheduler
  []
  (let [tasks (get-json-tasks)
        meetings (get-json-meetings)
        preferences (get-json-preferences)
        sched (:body (prolog-schedule tasks preferences meetings)) 
        valid-sched-p (valid-schedule-p sched)
        psched (if 
                 valid-sched-p 
                 (transform-schedule-to-keys ((json/parse-string sched) "json"))
                 ;; if it is not a valid schedule just return the exception
                 ((json/parse-string sched) "json")) 
        ]
    (if 
      valid-sched-p
      ;; schedule is valid so go and update stuff
      (do 
        (db/clear-schedule)
        (db/add-schedule psched)
        ;; return schedule in the end
        psched)
      psched)))

(defn split-up-map-into-before-and-today-and-after
  [coll]
  (let [iter (fn thisfnname [items akk]
                         (if (empty? items)
                           {:before akk :today-and-after items}
                           (let [f (first items)
                                 id (first f)
                                 grouped (second f)
                                 ]
                             (if (utils/normalized-java-date-before-today? id)
                                 (thisfnname (rest items) (conj akk f))
                               {:before akk :today-and-after items}))))]
    (iter coll (sorted-map))))


(defn get-schedule-by-day []
  (let [sched (remove #(= (:type %) "Meeting")  (db/get-schedule)) ;; don't keep the meetings
        deadlines (map #(assoc % :type :deadline)
                       (db/get-tasks))
        meetings (db/get-meetings) ;; we do not want the meetings from the schedule (they are only used for scheduling purposes, we also want to show meetings that were not scheduled)
        all-stuff (sort-by #(to-long (from-date (or (:from %) (when (:due_day %)
                                                                (utils/convert-due-day-due-time-to-duedate (:due_day %) (:due_time %) (utils/get-offset))))))  (into sched 
                                                                                                                                                                     (into deadlines meetings)))
        ;; note that we convert the map returned by group-by to a sorted-map
        ;; (from a certain size on, maps are NOT guaranteed to be sorted on
        ;; size; actualy hash-maps are used when the map becomes too big (when
        ;; the size is small is enough Arrays are used and they stay sorted)
        grouped (into (sorted-map) (group-by #(utils/normalize-date (or (:from %) 
                                                     (:due_day %))
                                                 (utils/get-offset))
                         all-stuff))
        before-and-after-split (split-up-map-into-before-and-today-and-after grouped)
        ]
    before-and-after-split))


