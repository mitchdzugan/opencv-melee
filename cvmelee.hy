(import [toolz.itertoolz [*]])
(import [toolz.functoolz [*]])
(import [toolz.dicttoolz [*]])
(import [pprint [pprint]])
(import numpy)
(import cv2)

(require [hy.contrib.anaphoric [*]])

(def CV_CAP_PROP_POS_MSEC 0)
(def CV_CAP_PROP_POS_FRAMES 1)
(def CV_CAP_PROP_POS_AVI_RATIO 2)
(def CV_CAP_PROP_FRAME_WIDTH 3)
(def CV_CAP_PROP_FRAME_HEIGHT 4)
(def CV_CAP_PROP_FPS 5)
(def CV_CAP_PROP_FOURCC 6)
(def CV_CAP_PROP_FRAME_COUNT 7)
(def CV_CAP_PROP_FORMAT 8)
(def CV_CAP_PROP_MODE 9)
(def CV_CAP_PROP_BRIGHTNESS 10)
(def CV_CAP_PROP_CONTRAST 11)
(def CV_CAP_PROP_SATURATION 12)
(def CV_CAP_PROP_HUE 13)
(def CV_CAP_PROP_GAIN 14)
(def CV_CAP_PROP_EXPOSURE 15)
(def CV_CAP_PROP_CONVERT_RGB 16)
(def CV_CAP_PROP_WHITE_BALANCE 17)
(def CV_CAP_PROP_RECTIFICATION 18)

(defn read-frame
  [capture index]
  (.set capture CV_CAP_PROP_POS_FRAMES index)
  (. (.read capture) [1]))

(defn remove-neighbors
  [dist l]
  (if (> (len l) 0)
    (list (reduce (fn [acc v]
                    (if (< (abs (- v (first acc)))
                           dist)
                      acc
                      (list (cons v acc)))) (drop 1 l) [(first l)]))
    []))

(defn line-selector
  [capture-details primary secondary dimension detected-lines]
  (let [normalizer (fn [[xlow ylow xhigh yhigh]]
                     {:x [xlow xhigh]
                      :y [ylow yhigh]})]
    (->> detected-lines
         (map normalizer)
         (filter (fn [{[plow phigh] primary [slow shigh] secondary}]
                   (and (= plow phigh)
                        (> (abs (- slow shigh)) (/ (dimension capture-detauks) 10)))))
         (map #(-> % primary first))
         (concatv (map #(+ % -24) (range 0 25 3)))
         (concatv (map #(+ % (dimension capture-details)) (range 0 25 3)))
         sorted
         (remove-neighbors 3))))

(defn get-capture
  [filename]
  (let [capture (.VideoCapture cv2 filename)]
    (merge {:capture capture}
           (->> [[:width CV_CAP_PROP_FRAME_WIDTH]
                 [:height CV_CAP_PROP_FRAME_HEIGHT]
                 [:frames CV_CAP_PROP_FRAME_COUNT]
                 [:fps CV_CAP_PROP_FPS]]
                (map (fn [[k prop]]
                       {k (->> prop
                               (.get capture)
                               round
                               int)}))
                (reduce merge)))))

(defn sample-frames
  [sample-count {:keys [frames capture]}]
  (->> (range 1 (:frames capture-details) (// (- (:frames capture-details) 1) sample-count))
       (map (xi ->
                (:capture capture-details)
                ((fn [f] (pprint (/ (* x1 sample-count) (:frames capture-details)))
                   f))
                (read-frame x1)))))

(defn get-possible-rois
  [sample-count capture-details sampled-frames]
  (let [average-image (->> sampled-frames
                           (map (fn [frame]
                                  (/ frame sample-count)))
                           (reduce cv2.add)
                           ((fn [f] (.astype f (. numpy uint8)))))
        detected-lines (-> average-image
                           (cv2.cvtColor (. cv2 COLOR_BGR2GRAY))
                           (cv2.Canny 100 300)
                           (cv2.HoughLinesP 1 (/ numpy.pi 180) 100)
                           (. [0]))
        x-lines (line-selector capture-details :x :y :height detected-lines)
        y-lines (line-selector capture-details :y :x :width detected-lines)
        {xlows true xhighs false} (group-by (% < (/ (:width  capture-details) 2)) x-lines)
        {ylows true yhighs false} (group-by (% < (/ (:height capture-details) 2)) y-lines)]
    (for [ylow ylows
          yhigh yhighs
          xlow xlows
          xhigh xhighs]
      {:xlow xlow
       :xhigh xhigh
       :ylow ylow
       :yhigh yhigh})))

(defn process-vod
  [filename]
  (let [sample-count 40
        capture-details (get-capture filename)
        sampled-frames (sample-frames sample-count capture-details)
        ]))

(process-vod "vods/test.mp4")
