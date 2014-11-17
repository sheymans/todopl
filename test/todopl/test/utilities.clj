(ns todopl.test.utilities
  (:use [todopl.utilities])
  (:use [clojure.test]))


(deftest conversion-time-strings-to-numbers
         (let [{:keys [hours minutes]} (convert-string-time-to-numbers "17:15")]
           (is (and (= hours 17) (= minutes 15))))
         (let [{:keys [hours minutes]} (convert-string-time-to-numbers "07:15")]
           (is (and (= hours 7) (= minutes 15))))
         (let [{:keys [hours minutes]} (convert-string-time-to-numbers "07:00")]
           (is (and (= hours 7) (= minutes 0)))))


