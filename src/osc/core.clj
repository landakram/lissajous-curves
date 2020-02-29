(ns lissajous-curves.core
  (:require [quil.core :refer :all]
            [quil.middleware :as m]
            [clojure.pprint :refer [pprint]]
            [plotter-utils.quil :as pl]
            [quil.core :as q]))

(defn cool-values []
  [{:angle-x 0 :angle-y 0 :speed-x 0.020 :speed-y 0.081}
   {:angle-x 0 :angle-y 0 :speed-x 0.022 :speed-y 0.089}
   {:angle-x 0 :angle-y 0 :speed-x 0.0223 :speed-y 0.089}
   {:angle-x 0 :angle-y 0 :speed-x 0.0224 :speed-y 0.089}
   {:angle-x 0 :angle-y 0 :speed-x 0.0225 :speed-y 0.089}
   {:angle-x 0 :angle-y 0 :speed-x 0.0226 :speed-y 0.089}
   {:angle-x 0 :angle-y 0 :speed-x 0.05 :speed-y 0.056}
   {:angle-x 0 :angle-y 0 :speed-x 0.05 :speed-y 0.05713}
   {:angle-x 0 :angle-y 0 :speed-x 0.05 :speed-y 0.05725}
   {:angle-x 0, :angle-y 0, :speed-x 0.050845243, :speed-y 0.0101919}
   {:angle-x 0, :angle-y 0, :speed-x 0.09812994, :speed-y 0.034261096}
   {:angle-x 0, :angle-y 0, :speed-x 0.08524015, :speed-y 0.07206908}
   {:angle-x 0, :angle-y 0, :speed-x 0.09595477, :speed-y 0.04840931}
   {:angle-x 0, :angle-y 0, :speed-x 0.03965899, :speed-y 0.02002182}
   {:angle-x 0, :angle-y 0, :speed-x 0.06923711, :speed-y 0.06866061}
   {:angle-x 0, :angle-y 0, :speed-x 0.04433445, :speed-y 0.03167829}])

(defonce idx (atom 0))
;; (def idx (atom 0))

(defn setup []
  (frame-rate 30)
  (color-mode :hsb)

  (let [state (nth (cool-values) @idx)]
    (println @idx)
    (swap! idx inc)
    state))

(defn update-state [{:keys [angle-x angle-y speed-x speed-y] :as state}]
  (-> state
      (update :angle-x #(+ speed-x %))
      (update :angle-y #(+ speed-y %))))

(defn lissajous-curve [w h angle-x angle-y speed-x speed-y]
  (let [x-angles (iterate #(+ % speed-x) angle-x)
        y-angles (iterate #(+ % speed-y) angle-y)]
    (for [[ang-x ang-y] (partition 2 (interleave x-angles y-angles))]
      (let [x (+ (/ w 2) (* (sin ang-x) w 0.4))
            y (+ (/ h 2) (* (sin ang-y) h 0.4))]
        [x y]))))

(defn draw-curve [w h {:keys [angle-x angle-y speed-x speed-y]}]
  (no-fill)
  (begin-shape)
  (doseq [[x y] (take 2500 (lissajous-curve w h angle-x angle-y speed-x speed-y))]
    (vertex x y))
  (end-shape))

(defn draw-state [{:keys [angle-x angle-y speed-x speed-y] :as state}]
  (background 255)
  (pprint state)

  (pl/record-many
   (cool-values)
   (width)
   (height)
   (fn [state out]
     (println "Recording to " out "...")
     (draw-curve (width) (height) state)
     (println "Done.")))

  #_(draw-curve (width) (height) state)
  (no-loop))

(defsketch lissajous-curves
  :title "lissajous-curves"
  :size [800 600]
  :settings (fn [] (smooth 8))
  :setup setup
  :update update-state
  :draw draw-state
  :middleware [m/fun-mode])
