(ns resourceplan.core
  (:require [clojure.math.combinatorics :as comb])
  (:require [clojure.set :as set])
  (:gen-class))

(def resources {:a 70 :b 95 :c 89})

(def tasks {:p1 {:a 18 :b 13 :c 33}
            :p2 {:a 11 :b 20 :c 24}
            :p3 {:a 42 :b 55 :c 55}
            :p4 {:a 20 :b 25 :c 12}
            :p5 {:a 33 :b 22 :c 12}
            :p6 {:a 11 :b 23 :c 24}
            :p7 {:a 32 :b 44 :c 23}
            :p8 {:a 15 :b 22 :c 8}
            :p9 {:a 19 :b 45 :c 34}
            :p10 {:a 43 :b 22 :c 11}
            ;:p11 {:a 10 :b 11 :c 12}
            ;:p12 {:a 4 :b 8 :c 9}
            })

(defn more-resource? [resource task]
  (every? pos? (vals (merge-with - resource task))))

(defn calc-resource [resource task  task-list]
  (loop [t task-list
         sub-res (get task (first t))
         result #{}]
    (if-not (more-resource? resource sub-res)
      result
      (recur (rest t)
             (merge-with + sub-res (get task (first (rest t))))
             (conj result (first t))))
    ))

(defn calc-tasks-f [r t]
  (fn [coll]
    (reduce
     (fn [tl tp]
       (conj tl (sort (calc-resource r t tp))))
     #{}
     coll)))

(def calc-f (calc-tasks-f resources tasks))

(defn calc-pmap [r t]
  (let [p (comb/permutations (keys t))
        n (int (/ (count p) 4))
        parts (partition n n nil p)]
    (reduce set/union (pmap calc-f parts))
    ))

(defn -main [& args]
  (time (calc-pmap resources tasks))
  (println "END")
)
