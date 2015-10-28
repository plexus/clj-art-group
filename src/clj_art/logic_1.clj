(ns clj-art.logic-1
  (:require [clojure.core.logic :as l]
            [clojure.core.logic.pldb :as pldb :refer [db-rel db]]
            [clojure.core.logic.fd :as fd]))


(pldb/db-rel man p)
(pldb/db-rel woman p)
(pldb/db-rel likes p1 p2)
(pldb/db-rel fun p)

(def facts0
  (pldb/db
   [man 'Bob]
   [man 'John]
   [man 'Ricky]

   [woman 'Mary]
   [woman 'Martha]
   [woman 'Lucy]

   [likes 'Bob 'Mary]
   [likes 'John 'Martha]
   [likes 'Ricky 'Lucy]))

(def facts1 (pldb/db-fact facts0
                          fun 'Lucy))

(pldb/with-db facts1
  (l/run* [q]
    (l/fresh [x y]
      (fun y)
      (likes x y)
      (l/== q [x y]))))


(l/run* [q]
  (l/== q true))
