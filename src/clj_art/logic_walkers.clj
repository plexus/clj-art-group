(ns clj-art.logic-walkers
  (:require [quil.core :refer :all]
            [quil.middleware :as m]
            [clojure.core.logic :as l]
            ;;[clojure.core.logic.pldb :as pldb :refer [db-rel db]]
            [clojure.core.logic.fd :as fd]))

(def screen-w 500)
(def screen-h 500)

(def walkers-num 15)

(def img-url "http://www.caudata.org/forum/attachments/f46-beginner-newt-salamander-axolotl-help-topics/f48-axolotls-ambystoma-mexicanum/f62-axolotl-gallery/31545d1371588413-cute-axolotl-pictures-thread-2-jpg-1-.jpg")

(defn make-palette [image]
  (let [pixel-count (count (pixels image))
        step-size (/ pixel-count walkers-num)
        pixel-indexes (range 0 pixel-count step-size)]
    (mapv (fn [i] (nth (pixels image) i)) pixel-indexes)))

(defn setup []
  (smooth)
  (background 255)
  (frame-rate 50000)
  (color-mode :hsb)
  (def axolotl (load-image img-url))
  (def color-palette (make-palette axolotl))
  (mapv (fn [i] {:x (rand-int screen-w)
                 :y (rand-int screen-h)
                 :color (nth color-palette i)}) (range walkers-num)))

                                        ; Our initial state looks like this:
                                        ; [{:x :y} {:x :y} …]

(defn update-state [old-state]
                                        ; For each i (=element in old-state), either (=randomly) the x or the y coordinate
                                        ; is either incremented or decremented (by one)
  (mapv (fn [i] (update-in i [(rand-nth [:x  :y])] (rand-nth [inc dec]))) old-state))

(defn coord-candidates [old-x old-y]
  (let [max-diff 4]
    ;; the results will be the possible values of the logic variable q
    (l/run* [q]

      ;; we also introduce two "temporary" logic variables, x and y
      (l/fresh [x y]

        ;; x and y are screen coordinates, so between zero and width/height
        (fd/in x (fd/interval 0 (dec screen-w)))
        (fd/in y (fd/interval 0 (dec screen-h)))

        ;; x and y can not be further than max-diff from the previous positions
        ;; "conde" is a logical conjuction (OR)
        ;; (conde [x AND y] OR [z AND a])
        (l/conde
         [;; fd/eq is a macro that takes an equation and turns it into constraints
          (fd/eq (< (- old-x x) max-diff))]
         [(fd/eq (< (- x old-x) max-diff))])

        (l/conde
         [(fd/eq (< (- old-y y) max-diff))]
         [(fd/eq (< (- y old-y) max-diff))])

        ;; the possible new coordinates need to be different from the old ones
        (l/!= [x y] [old-x old-y])

        ;; "unify" q and [x y] to "collect" the results
        (l/== q [x y])))))

(defn update-state-logic [old-state]
  (mapv (fn [{old-x :x old-y :y color :color}]
          (let [[new-x new-y]
                (rand-nth (seq (set (coord-candidates old-x old-y))))]
            {:x new-x :y new-y :color color})) old-state))

(defn key-pressed [old-state event]
  (let [ev    		(:key event)
        change 		 {:up 	 {:dir :y :val dec}
                          :down  {:dir :y :val inc}
                          :left  {:dir :x :val dec}
                          :right {:dir :x :val inc}}]

    (mapv
                                        ; mapv iterates over all the walker locations (pairs of coordinates), stored as maps inside a vector,
                                        ; see line 14-15
                                        ; mapv takes a function and a vector (in our case, the vector contains maps).
                                        ; The function determines how the vector is to be modified.
                                        ; The function that we pass to mapv is anonymous. Its first argument is a temporary variable inside []s.
                                        ; The elements of the vector old-state get assigned to i one after the other.

     (fn [i] (update-in i [(get-in change [ev :dir])] (get-in change [ev :val])))
     old-state)))

(defn draw [state]
                                        ;(background 255)
                                        ;(ellipse 100 100 30 30)
  (doseq [x state]
    (stroke-int (:color x))
    (point (:x x) (:y x))))

(defsketch example
  :title "Walker"
  :setup setup
  :draw draw
  :update update-state-logic
  :size [screen-w screen-h]
  :middleware [m/fun-mode]
  :key-pressed key-pressed)

(defn -main [& args])
