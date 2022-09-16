(ns com.scarytom.langton.core
  (:require [quil.core :as q]
            [quil.middleware :as m]))

(def grid-size 60)
(def colours [[0 0 0] [204 102 0] [0 128 255] [64 64 64] [192 192 192]])
(def max-iterations 10500)

(def behaviours {:original [1 -1]
                 :chaotic [1 -1 1]
                 :symmetric [-1 -1 1 1]
                 :square [-1 1 1 1 1 1 -1 -1 1]
                 :convoluted-highway [-1 -1 1 1 1 -1 1 -1 1 -1 -1 1]
                 :moving-triangle [1 1 -1 -1 -1 1 -1 -1 -1 1 1 1]})

(def behaviour (behaviours :chaotic))

(def state {:iteration     0
            :ant-direction 0
            :ant-position  (int (+ (/ grid-size 2) (* grid-size (int (/ grid-size 2)))))
            :matrix        (vec (repeatedly (* grid-size grid-size) #(identity 0)))})

(defn setup []
  (q/frame-rate 128)
  (q/color-mode :hsb)
  (q/no-stroke)
  (q/background 255)
  (Thread/sleep 3000)
  state)

(defn update-direction [dir colour]
  (mod (+ dir (behaviour colour))
       4))

(defn update-position [pos dir]
  (mod (case dir
         0 (- pos grid-size)
         1 (inc pos)
         2 (+ pos grid-size)
         3 (dec pos))
       (* grid-size grid-size)))

(defn update-state [{:keys [iteration ant-position ant-direction matrix] :as state}]
  (let [colour-under-ant (get matrix ant-position)
        new-colour (mod (inc colour-under-ant) (count behaviour))
        new-direction (update-direction ant-direction colour-under-ant)
        new-position (update-position ant-position new-direction)]
    (if (< iteration max-iterations)
      (assoc state :iteration (inc iteration)
                   :ant-previous-position ant-position
                   :ant-position new-position
                   :ant-direction new-direction
                   :matrix (assoc matrix ant-position new-colour))
      state)))

(defn draw-state [{:keys [ant-previous-position matrix] :as _state}]
  (when ant-previous-position
    (let [cell-size (quot (q/width) grid-size)
          multiplier (int (/ ant-previous-position grid-size))
          x (* cell-size (- ant-previous-position (* multiplier grid-size)))
          y (* cell-size multiplier)]
      (apply q/fill
        (colours (matrix ant-previous-position)))
      (q/rect x y cell-size cell-size))))

(q/defsketch langton
  :title "Langdon's Ant"
  :size [2000 2000]
  ; setup function called only once, during sketch initialization.
  :setup setup
  ; update-state is called on each iteration before draw-state.
  :update update-state
  :draw draw-state
  ;:features [:keep-on-top]
  ; This sketch uses functional-mode middleware.
  ; Check quil wiki for more info about middlewares and particularly
  ; fun-mode.
  :middleware [m/fun-mode])
