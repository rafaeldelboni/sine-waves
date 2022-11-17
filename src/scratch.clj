(ns scratch
  (:import [java.awt
            BasicStroke
            Color
            Graphics2D
            RenderingHints]
           [java.awt.geom GeneralPath]
           [java.awt.image BufferedImage]
           [java.io File]
           [javax.imageio IIOImage ImageIO ImageWriter]
           [javax.imageio.stream FileImageOutputStream]))

(set! *warn-on-reflection* true)

(defn ^:private get-png-imagewriter
  "Return an ImageWriter for PNG images"
  []
  (let [^java.util.Iterator iterator (ImageIO/getImageWritersBySuffix "png")]
    (when-not (.hasNext iterator)
      (throw (Exception. "No image writer for PNG")))
    (.next iterator)))

(defn ^:private write-image
  "Write in the ImageWriter the image buffered bytes and its metadata"
  [^ImageWriter writer ^BufferedImage image]
  (let [^IIOImage iio-image (IIOImage. image nil nil)]
    (.write writer nil iio-image nil)))

(defn write-image-file!
  [^FileImageOutputStream output
   ^BufferedImage buffered-image]
  (let [^ImageWriter imagewriter (get-png-imagewriter)]
    (.setOutput imagewriter output)
    (write-image imagewriter buffered-image)
    (.flush output)
    (.close output)
    (.dispose imagewriter)))

(defn color [[r g b]]
  (Color. ^Long r ^Long g ^Long b))

(defn points->general-path ^GeneralPath [points]
  (let [general-path ^GeneralPath. (GeneralPath.)]
    (.moveTo general-path (-> points first :x double) (-> points first :y double))
    (doseq [point points
            :let [x (-> point :x double)
                  y (-> point :y double)]]
      (.lineTo general-path x y))
    general-path))

(defn sine-wave-calc
  [size amplitude frequency]
  (reduce
   (fn [acc _]
     (let [{:keys [x]} (last acc)
           new-item {:x (inc x)
                     :y (* amplitude (Math/sin (* 2 Math/PI frequency x)))}]
       (conj acc new-item)))
   [{:x 0 :y 0}]
   (range size)))

(defn draw! ^Graphics2D
  [width height amplitude frequency ^BufferedImage buffered-image]
  (let [points (sine-wave-calc width amplitude frequency)
        axis-color [220 220 220]
        bg-color [85 85 85]
        line-color [10 255 10]]
    (doto ^Graphics2D (.createGraphics buffered-image)
      (.setRenderingHint RenderingHints/KEY_ANTIALIASING RenderingHints/VALUE_ANTIALIAS_ON)
      (.setRenderingHint RenderingHints/KEY_RENDERING RenderingHints/VALUE_RENDER_QUALITY)
      ; background
      (.setPaint (color bg-color))
      (.fillRect 0 0 width height)
      ; move the cursor to center
      (.translate 0 (int (/ height 2)))
      ; axis
      (.setPaint (color axis-color))
      (.drawString "chaos" 235 -200)
      (.drawString "boredom" 225 200)
      (.draw (java.awt.geom.Line2D$Double. 0.0 0.0 500.0 0.0))
      ; line thickness
      (.setStroke (BasicStroke. 3))
      ; lines
      (.setPaint (color line-color))
      (.draw (points->general-path points)))))

(defn generate! ^String
  ([width height amplitude frequency]
   (generate! width height amplitude frequency (File/createTempFile "generated-image" ".png")))
  ([width height amplitude frequency ^File file-output]
   (let [^FileImageOutputStream output (FileImageOutputStream. file-output)
         ^BufferedImage buffered-image (BufferedImage. width height BufferedImage/TYPE_INT_ARGB)]
     (draw! width height amplitude frequency buffered-image)
     (write-image-file! output buffered-image))))

(defn -main
  "Invoke me with clojure -M -m scratch"
  [& _args]
  (let [sine-wave (File. "sine-wave.png")
        ideal-sine-wave (File. "ideal-sine-wave.png")]
    (generate! 500 500 15 (/ 1 200) ideal-sine-wave)
    (generate! 500 500 150 (/ 1 450) sine-wave))
  (println "images generated!"))
