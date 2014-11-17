(defproject todopl "1.1.2-SNAPSHOT"
            :description "Todopl: It' about time"
            :dependencies [[org.clojure/clojure "1.5.1"]
                           [seesaw "1.4.4"]
                           [clj-time "0.5.1"]
                           [clj-http "0.7.5"] ;; for posting/getting a service (in our case prolog server)
                           [cheshire "5.2.0"]
                           [com.toedter/jcalendar "1.4"] ;; for date chooser and spinner (Jspinfield)
                           [me.raynes/conch "0.5.0"] ;; killing processes
                           [overtone/at-at "1.1.1"]
                           ;; for msexchange:
                           [org.apache.commons/commons-lang3 "3.1"]
                           [commons-codec/commons-codec "1.8"]
                           [commons-httpclient/commons-httpclient "3.1"]
                           [commons-logging/commons-logging "1.1.3"]
                           [jcifs "1.3.17"]
                           [com.todopl.foreign/msexchange "0.1"]
                           ]
            ;:repositories [["informatics" "http://informatics.mayo.edu/maven/content/groups/public/"]["project" "file:repo"]]
            :repositories [["informatics" "http://informatics.mayo.edu/maven/content/groups/public/"]]
            ;; next 2 lines as in
            ;; https://github.com/technomancy/leiningen/blob/stable/doc/TUTORIAL.md
            :main todopl.core
            :aot [todopl.core])

