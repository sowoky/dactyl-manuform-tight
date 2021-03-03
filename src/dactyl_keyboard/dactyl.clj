(ns dactyl-keyboard.dactyl
  (:refer-clojure :exclude [use import])
  (:require [clojure.core.matrix :refer [array matrix mmul]]
            [scad-clj.scad :refer :all]
            [scad-clj.model :refer :all]))

(defn deg2rad [degrees]
  (* (/ degrees 180) pi))

(defn debug [shape]
  (color [0.5 0.5 0.5 0.5] shape))

(def WHI [255/255 255/255 255/255 1])
(def RED [255/255 0/255 0/255 1])
(def ORA [220/255 128/255 0/255 1])
(def YEL [220/255 255/255 0/255 1])
(def GRE [0/255 255/255 0/255 1])
(def CYA [0/255 255/255 255/255 1])
(def BLU [0/255 128/255 255/255 1])
(def NBL [0/255 0/255 255/255 1])
(def PUR [127/255 0/255 255/255 1])
(def PIN [255/255 0/255 255/255 1])
(def MAG [255/255 0/255 127/255 1])
(def BRO [102/255 51/255 0/255 1])
(def BLA [0/255 0/255 0/255 1])
(def _1U [220/255 163/255 163/255 1])
(def _2U [127/255 159/255 127/255 1])
(def _1_5U [240/255 223/255 175/255 1])


(def use_hotswap true)
(def left-half false)
(def north_facing true)
(def LED-holder false)
(def row-web false) ; not recommended for high stagger, keycaps could hit it. 
(def inset-plate true); plate inside case or under it

;;;;;;;;;;;;;;;;;;;;;;
;; Wall thicknesses ;;
;;;;;;;;;;;;;;;;;;;;;;
(def switch-x-wall-thickness 1.5) ; still needs more work before can change
(def switch-y-wall-thickness 1.5) ; needs more considerations with hotswap holder... 
(def bottom-plate-thickness 2) 

;;;;;;;;;;;;;;;;;;;;;;
;; Shape parameters ;;
;;;;;;;;;;;;;;;;;;;;;;

(def nrows 4)
(def ncols 5)
(def extra-col-three true) ;whether to add extra key at bottom of col 3 (ring finger)
(def extra-col-four true) ;whether to add extra key at bottom of col 3 (pinky finger) 
(def extra-col-five false) ;whether to add extra key at bottom of col 3 (pinky finger) 
(def column-curvature (deg2rad 20))                         ; 15                        ; curvature of the columns
(def row-curvature (deg2rad (if (> nrows 4) 1 8)))                             ; 5                   ; curvature of the rows
(def centerrow (case nrows    
    6 3.1
    5 2.1 
    4 1.75))    ; front tilt
(def centercol 3)                                          ; controls left-right tilt / tenting (higher number is more tenting)
(def tenting-angle (deg2rad 15))                            ; or, change this for more precise tenting control
(def column-style :standard)
(defn column-offset [column] (cond
                               (= column 0) [0 -4.5 0]
                               (= column 2) [0 5.5 -4.5]
                               (= column 3) [0 -4.5 -0.5]
                               (= column 4) [0 -14 5]
                               (>= column 5) [0 -15 6]
                               :else [0 0 0]))
(def thumb-offsets (if (> nrows 4) [8 -5 8] [9 -5 2.5]))
(def keyboard-z-offset (case nrows 
    6 20
    5 10.5 
    4 7))                                   ; controls overall height; original=9 with centercol=3; use 16 for centercol=2
(def extra-width 2)                                       ; extra space between the base of keys; original= 2
(def extra-height 1.7)                                      ; original= 0.5
(def bottom-height 2)                                    ; plexiglass plate or printed plate
(def wall-z-offset -5)                                      ; -5                ; original=-15 length of the first downward-sloping part of the wall (negative)
(def wall-z-offset2 0)                                      ; used where switch turns towards case 
(def wall-xy-offset 1)                                   ;how far out wall goes 
(def wall-thickness 1)                                      ; wall thickness parameter; originally 5

;;;;;;;;;;;;;;;;;;;;;;;
;; Tolerances        ;;
;;;;;;;;;;;;;;;;;;;;;;;

(def keyswitch-height 14.1)                                   ;; Was 14.1, then 14.25
(def keyswitch-width 14.1)
(def switch-center-peg-diameter 4.35)
(def switch-side-peg-diameter 2.15)
(def switch-pin-diameter 3.6)
;;;;;;;;;;;;;;;;;;;;;;
;; General variables ;;
;;;;;;;;;;;;;;;;;;;;;;;

(def lastrow (dec nrows))
(def cornerrow (dec lastrow))
(def lastcol (dec ncols))

;;;;;;;;;;;;;;;;;
;; Switch Hole ;;
;;;;;;;;;;;;;;;;;


(def plate-thickness 5)
(def retention-tab-thickness 1.5)
(def retention-tab-hole-thickness (- plate-thickness retention-tab-thickness))
(def mount-width (+ keyswitch-width (* 2 switch-x-wall-thickness)))
(def mount-height (+ keyswitch-height (* 2 switch-y-wall-thickness)))

;for the bottom
(def filled-plate
  (->> (cube mount-height mount-width plate-thickness)
  ;(->> (cube keyswitch-width keyswitch-height 1); plate-thickness)
       (translate [0 0 (/ plate-thickness 2)])
       ))

(def holder-x mount-width)
(def holder-thickness    (/ (- holder-x keyswitch-width) 2))
(def holder-y            (+ keyswitch-height (* holder-thickness 2)))
(def swap-z              3)
(def web-thickness (+ plate-thickness 0.85))
(def row-web-thickness (if use_hotswap (+ plate-thickness swap-z) plate-thickness))
(def keyswitch-below-plate (- 8 row-web-thickness))           ; approx space needed below keyswitch

(def square-led-size     6)
(def mirror-internals left-half) ; lazy way to re-generate left side with correct hotswap holder orientation

(def switch-teeth-cutout
  (let [
        ; cherry, gateron, kailh switches all have a pair of tiny "teeth" that stick out
        ; on the top and bottom, this gives those teeth somewhere to press into
        teeth-x        4.5
        teeth-y        0.75
        teeth-z        1.75
        teeth-x-offset 0
        teeth-y-offset (+ (/ keyswitch-height 2) (/ teeth-y 2.01))
        teeth-z-offset (- plate-thickness 1.95)
       ]
      (->> (cube teeth-x teeth-y teeth-z)
           (translate [teeth-x-offset teeth-y-offset teeth-z-offset])
      )
  )
)

(def hotswap-y1          4.3) ;first y-size of kailh hotswap holder
(def hotswap-y2          6.2) ;second y-size of kailh hotswap holder
(def hotswap-y3          (+ 3.7 switch-y-wall-thickness)) ;space on other side of switch plate to cut to holder thickness
(def hotswap-z           (+ swap-z 0.5));thickness of kailn hotswap holder + some margin of printing error (0.5mm)
(def hotswap-cutout-z-offset -2.6)
(def hotswap-cutout-1-y-offset 4.95)
(def hotswap-holder
  (let [
        
        ;irregularly shaped hot swap holder
        ; ___________
        ;|_|_______| |  hotswap offset from out edge of holder with room to solder
        ;|_|_O__  \ _|  hotswap pin
        ;|      \O_|_|  hotswap pin
        ;|  o  O  o  |  fully supported friction holes
        ;|    ___    |  
        ;|    |_|    |  space for LED under SMD or transparent switches
        ;
        ; can be described as having two sizes in the y dimension depending on the x coordinate
        
        swap-x              holder-x
        swap-y              holder-y;(if (or (> 11.5 holder-y) LED-holder) holder-y 11.5) ; should be less than or equal to holder-y
        
        swap-offset-x       0
        swap-offset-y       (/ (- holder-y swap-y) 2)
        swap-offset-z       (* (/ swap-z 2) -1) ; the bottom of the hole. 
        swap-holder         (->> (cube swap-x swap-y swap-z)
                                 (translate [swap-offset-x 
                                             swap-offset-y
                                             swap-offset-z]))
        hotswap-x           holder-x ;cutout full width of holder instead of only 14.5mm
        hotswap-x2          (* (/ holder-x 3) 1.95)
        hotswap-x3          (/ holder-x 4)

        hotswap-cutout-1-x-offset 0.01
        hotswap-cutout-2-x-offset (* (/ holder-x 4.5) -1)
        hotswap-cutout-3-x-offset (- (/ holder-x 2) (/ hotswap-x3 2))
        hotswap-cutout-4-x-offset (- (/ hotswap-x3 2) (/ holder-x 2))
        hotswap-cutout-led-x-offset 0
        hotswap-cutout-1-y-offset 4.95
        hotswap-cutout-2-y-offset 4
        hotswap-cutout-3-y-offset (/ holder-y 2)
        hotswap-cutout-led-y-offset -6
        
        hotswap-cutout-1    (->> (cube hotswap-x hotswap-y1 hotswap-z)
                                 (translate [hotswap-cutout-1-x-offset 
                                             hotswap-cutout-1-y-offset 
                                             hotswap-cutout-z-offset]))
        hotswap-cutout-2    (->> (cube hotswap-x2 hotswap-y2 hotswap-z)
                                 (translate [hotswap-cutout-2-x-offset 
                                             hotswap-cutout-2-y-offset 
                                             hotswap-cutout-z-offset]))
        hotswap-cutout-3    (->> (cube hotswap-x3 hotswap-y1 hotswap-z)
                                 (translate [ hotswap-cutout-3-x-offset
                                              hotswap-cutout-3-y-offset
                                              hotswap-cutout-z-offset]))
        hotswap-cutout-4    (->> (cube hotswap-x3 hotswap-y1 hotswap-z)
                                 (translate [ hotswap-cutout-4-x-offset
                                              hotswap-cutout-3-y-offset
                                              hotswap-cutout-z-offset]))
        hotswap-cutout-5    (->> (cube (+ holder-x 1) hotswap-y3 hotswap-z)
                                 (translate [ hotswap-cutout-led-x-offset
                                              hotswap-cutout-led-y-offset
                                              hotswap-cutout-z-offset]))
        hotswap-led-cutout  (->> (cube square-led-size square-led-size hotswap-z)
                                 (translate [ hotswap-cutout-led-x-offset
                                              hotswap-cutout-led-y-offset
                                              hotswap-cutout-z-offset]))
        ; for the main axis
        main-axis-hole      (->> (cylinder (/ switch-center-peg-diameter 2) 10)
                                 (with-fn 12))
        plus-hole           (->> (cylinder (/ switch-pin-diameter 2) 10)
                                 (with-fn 8)
                                 (translate [-3.81 2.54 0]))
        minus-hole          (->> (cylinder (/ switch-pin-diameter 2) 10)
                                 (with-fn 8)
                                 (translate [2.54 5.08 0]))
        friction-hole       (->> (cylinder (/ switch-side-peg-diameter 2) 10)
                                 (with-fn 8))
        friction-hole-right (translate [5 0 0] friction-hole)
        friction-hole-left  (translate [-5 0 0] friction-hole)
       ]
      (difference swap-holder
                  main-axis-hole
                  plus-hole
                  minus-hole
                  friction-hole-left
                  friction-hole-right
                  hotswap-cutout-1
                  hotswap-cutout-2
                  hotswap-cutout-3
                  hotswap-cutout-4
				  hotswap-cutout-5
				  ;hotswap-led-cutout
                  ;(if LED-holder hotswap-led-cutout)
		)
  )
)

(def single-plate
  (let [top-wall (->> (cube (+ keyswitch-height 3) switch-y-wall-thickness plate-thickness)
                      (translate [0
                                  (+ (/ switch-y-wall-thickness 2) (/ keyswitch-height 2))
                                  (/ plate-thickness 2)]))
        left-wall (->> (cube switch-x-wall-thickness (+ keyswitch-height 3) plate-thickness)
                       (translate [(+ (/ switch-x-wall-thickness 2) (/ keyswitch-width 2))
                                   0
                                   (/ plate-thickness 2)]))
        plate-half (difference (union top-wall left-wall) 
                               switch-teeth-cutout)
        plate (union plate-half
                  (->> plate-half
                       (mirror [1 0 0])
                       (mirror [0 1 0]))
                  (if use_hotswap 
                      (if north_facing
                          (->> hotswap-holder
                               (mirror [1 0 0])
                               (mirror [0 1 0])
                          )
                          hotswap-holder
                      )
                  )
              )
       ]
    (->> (if mirror-internals
           (->> plate (mirror [1 0 0]))
           plate
         )
    )
  )
)

;amoeba is 16 mm high
(def switch-bottom
  (translate [0 0 (/ keyswitch-below-plate -2)] (cube 16 keyswitch-width keyswitch-below-plate)))

(def hotswap-case-cutout
  (translate [0 (* hotswap-cutout-1-y-offset -1) hotswap-cutout-z-offset] (cube (+ keyswitch-width 3) hotswap-y1 hotswap-z)))

;;;;;;;;;;;;;;;;
;; SA Keycaps ;;
;;;;;;;;;;;;;;;;

(def sa-length 18.25)
(def sa-height 12.5)

(def sa-key-height-from-plate 7.39)
(def sa-cap-bottom-height (+ sa-key-height-from-plate plate-thickness))
(def sa-cap-bottom-height-pressed (+ 3 plate-thickness))

(def sa-double-length 37.5)
(def sa-cap {1   (let [bl2 (/ sa-length 2)
                       m 8.25
                       key-cap (hull (->> (polygon [[bl2 bl2] [bl2 (- bl2)] [(- bl2) (- bl2)] [(- bl2) bl2]])
                                          (extrude-linear {:height 0.1 :twist 0 :convexity 0})
                                          (translate [0 0 0.05]))
                                     (->> (polygon [[m m] [m (- m)] [(- m) (- m)] [(- m) m]])
                                          (extrude-linear {:height 0.1 :twist 0 :convexity 0})
                                          (translate [0 0 6]))
                                     (->> (polygon [[6 6] [6 -6] [-6 -6] [-6 6]])
                                          (extrude-linear {:height 0.1 :twist 0 :convexity 0})
                                          (translate [0 0 sa-height])))]
                   (union 
                     (->> key-cap
                          (translate [0 0 sa-cap-bottom-height])
                          (color _1U))
                     (debug (->> key-cap
                          (translate [0 0 sa-cap-bottom-height-pressed])))
                   )
                 )
             2   (let [bl2 sa-length
                       bw2 (/ sa-length 2)
                       key-cap (hull (->> (polygon [[bw2 bl2] [bw2 (- bl2)] [(- bw2) (- bl2)] [(- bw2) bl2]])
                                          (extrude-linear {:height 0.1 :twist 0 :convexity 0})
                                          (translate [0 0 0.05]))
                                     (->> (polygon [[6 16] [6 -16] [-6 -16] [-6 16]])
                                          (extrude-linear {:height 0.1 :twist 0 :convexity 0})
                                          (translate [0 0 sa-height])))]
                   (->> key-cap
                        (translate [0 0 (+ 5 plate-thickness)])
                        (color _2U)))
             1.5 (let [bl2 (/ sa-length 2)
                       bw2 (/ 27.94 2)
                       key-cap (hull (->> (polygon [[bw2 bl2] [bw2 (- bl2)] [(- bw2) (- bl2)] [(- bw2) bl2]])
                                          (extrude-linear {:height 0.1 :twist 0 :convexity 0})
                                          (translate [0 0 0.05]))
                                     (->> (polygon [[11 6] [-11 6] [-11 -6] [11 -6]])
                                          (extrude-linear {:height 0.1 :twist 0 :convexity 0})
                                          (translate [0 0 sa-height])))]
                   (->> key-cap
                        (translate [0 0 (+ 5 plate-thickness)])
                        (color _1_5U)))})

;;;;;;;;;;;;;;;;;;;;;;;;;
;; Placement Functions ;;
;;;;;;;;;;;;;;;;;;;;;;;;;

(def columns (range 0 ncols))
(def rows (range 0 nrows))

(def row-radius (+ (/ (/ (+ mount-height extra-height) 2)
                      (Math/sin (/ column-curvature 2)))
                   sa-cap-bottom-height))
(def column-radius (+ (/ (/ (+ mount-width extra-width) 2)
                         (Math/sin (/ row-curvature 2)))
                      sa-cap-bottom-height))
(def column-x-delta (+ -1 (- (* column-radius (Math/sin row-curvature)))))

(defn apply-key-geometry [translate-fn rotate-x-fn rotate-y-fn column row shape back-left-case]
  (let [column-angle (* row-curvature (- centercol column))
        placed-shape (->> shape
                          (translate-fn [0 0 (- row-radius)])
                          (rotate-x-fn (* column-curvature (- centerrow row)))
                          (translate-fn [0 0 row-radius])
                          (translate-fn [0 0 (- column-radius)])
                          (rotate-y-fn column-angle)
                          (translate-fn [0 0 column-radius])
                          (translate-fn (column-offset (if back-left-case 2 column))))
        column-z-delta (* column-radius (- 1 (Math/cos column-angle)))
        placed-shape-ortho (->> shape
                                (translate-fn [0 0 (- row-radius)])
                                (rotate-x-fn (* column-curvature (- centerrow row)))
                                (translate-fn [0 0 row-radius])
                                (rotate-y-fn column-angle)
                                (translate-fn [(- (* (- column centercol) column-x-delta)) 0 column-z-delta])
                                (translate-fn (column-offset column)))]

    (->> (case column-style
           :orthographic placed-shape-ortho
           placed-shape)
         (rotate-y-fn tenting-angle)
         (translate-fn [0 0 keyboard-z-offset]))))

(defn key-place [column row shape]
  (apply-key-geometry translate
                      (fn [angle obj] (rotate angle [1 0 0] obj))
                      (fn [angle obj] (rotate angle [0 1 0] obj))
                      column row shape false))
(defn key-place-back-left [column row shape]
  (apply-key-geometry translate
                      (fn [angle obj] (rotate angle [1 0 0] obj))
                      (fn [angle obj] (rotate angle [0 1 0] obj))
                      column row shape true))
(defn rotate-around-x [angle position]
  (mmul
    [[1 0 0]
     [0 (Math/cos angle) (- (Math/sin angle))]
     [0 (Math/sin angle) (Math/cos angle)]]
    position))

(defn rotate-around-y [angle position]
  (mmul
    [[(Math/cos angle) 0 (Math/sin angle)]
     [0 1 0]
     [(- (Math/sin angle)) 0 (Math/cos angle)]]
    position))


(defn rotate-around-z [angle position]
  (mmul
    [[(Math/cos angle) (- (Math/sin angle)) 0]
     [(Math/sin angle) (Math/cos angle) 0]
     [0 0 1]]
    position))

(defn key-position [column row position]
  (apply-key-geometry (partial map +) rotate-around-x rotate-around-y column row position false))

(defn key-places [shape]
  (apply union
         (for [column columns
               row rows
               :when (or (.contains [2 (if extra-col-three 3) (if extra-col-five 5)(if extra-col-four 4)] column)
                         (not= row lastrow))]
           (->> shape
                (key-place column row)))))
(def key-holes
  (key-places single-plate))
(def key-fills
  (key-places filled-plate))
(def key-space-below
  (key-places switch-bottom))
(def caps
  (key-places (sa-cap 1)))

;;;;;;;;;;;;;;;;;;;;
;; Web Connectors ;;
;;;;;;;;;;;;;;;;;;;;

; posts are located at the inside corners of the key plates.
; the 'web' is the fill between key plates.
;

(def post-size 0.1)
(def web-post (->> (cube post-size post-size web-thickness )
                   (translate [0 0 (+ (/ web-thickness -2) plate-thickness)])))

(def post-adj (/ post-size 2))
(def web-post-tr (translate [(- (/ mount-width 2) post-adj) (- (/ mount-height 2) post-adj) 0] web-post))
(def web-post-tl (translate [(+ (/ mount-width -2) post-adj) (- (/ mount-height 2) post-adj) 0] web-post))
(def web-post-bl (translate [(+ (/ mount-width -2) post-adj) (+ (/ mount-height -2) post-adj) 0] web-post))
(def web-post-br (translate [(- (/ mount-width 2) post-adj) (+ (/ mount-height -2) post-adj) 0] web-post))

; fat web post for very steep angles between thumb and finger clusters
; this ensures the walls stay somewhat thicker
(def fat-post-size 1.0)
(def fat-web-post (->> (cube fat-post-size fat-post-size web-thickness)
                       (translate [0 0 (+ (/ web-thickness -2)
                                          plate-thickness)])))

(def fat-post-adj (/ fat-post-size 2))
(def fat-web-post-tr (translate [(- (/ mount-width 2) fat-post-adj) (- (/ mount-height 2) fat-post-adj) 0] fat-web-post))
(def fat-web-post-tl (translate [(+ (/ mount-width -2) fat-post-adj) (- (/ mount-height 2) fat-post-adj) 0] fat-web-post))
(def fat-web-post-bl (translate [(+ (/ mount-width -2) fat-post-adj) (+ (/ mount-height -2) fat-post-adj) 0] fat-web-post))
(def fat-web-post-br (translate [(- (/ mount-width 2) fat-post-adj) (+ (/ mount-height -2) fat-post-adj) 0] fat-web-post))


; wide posts for 1.5u keys in the main cluster

(defn triangle-hulls [& shapes] (apply union (map (partial apply hull) (partition 3 1 shapes))))

(defn piramid-hulls [top & shapes] (apply union (map (partial apply hull top) (partition 2 1 shapes))))

(def connectors
  (apply union
         (concat
           ;; Row connections
            (if row-web 
			  (for [column (range 0 (dec ncols))
                     row (range 0 lastrow)]
               (triangle-hulls
                (key-place (inc column) row web-post-tl)
                (key-place column row web-post-tr)
                (key-place (inc column) row web-post-bl)
                (key-place column row web-post-br)))
            )
           ;; Column connections
           (for [column columns
                 row (range 0 cornerrow)]
             (triangle-hulls
               (key-place column row web-post-bl)
               (key-place column row web-post-br)
               (key-place column (inc row) web-post-tl)
               (key-place column (inc row) web-post-tr)))

           ; Diagonal connections
           (if row-web 
			 (for [column (range 0 (dec ncols))
                  row (range 0 cornerrow)]
              (triangle-hulls
                (key-place column row web-post-br)
                (key-place column (inc row) web-post-tr)
                (key-place (inc column) row web-post-bl)
                (key-place (inc column) (inc row) web-post-tl)))
           ))))

;;;;;;;;;;;;
;; Thumbs ;;
;;;;;;;;;;;;


(def thumborigin
  (map + (key-position 1 cornerrow [(/ mount-width 2) (- (/ mount-height 2)) 0])
       thumb-offsets))

"need to account for plate thickness which is baked into thumb-_-place rotation & move values
plate-thickness was 2
need to adjust for difference for thumb-z only"
(def thumb-design-z 2)
(def thumb-z-adjustment (- (if (> plate-thickness thumb-design-z)
                                 (- thumb-design-z plate-thickness)
                                 (if (< plate-thickness thumb-design-z)
                                       (- thumb-design-z plate-thickness) 
                                       0)) 
                           -1.1))
(def thumb-x-rotation-adjustment (if (> nrows 4) -12 10))
(defn thumb-place [rot move shape]
  (->> shape
       
       (translate [0 0 thumb-z-adjustment])                   ;adapt thumb positions for increased plate
       (rotate (deg2rad thumb-x-rotation-adjustment) [1 0 0]) ;adjust angle of all thumbs to be less angled down towards user since key is taller

       (rotate (deg2rad (nth rot 0)) [1 0 0])
       (rotate (deg2rad (nth rot 1)) [0 1 0])
       (rotate (deg2rad (nth rot 2)) [0 0 1])
       (translate thumborigin)
       (translate move)))

; convexer
(defn thumb-r-place [shape] (thumb-place [14 -40 10] [-15 -10 5] shape)) ; right
;used for wall
(defn thumb-r-place2 [shape] (thumb-place [14 -40 10] [-13 -10 5] shape)) ; right
(defn thumb-m-place [shape] (thumb-place [10 -23 20] [-33 -15 -6] shape)) ; middle
(defn thumb-l-place [shape] (thumb-place [6 -5 35] [-52.5 -25.5 -11.5] shape)) ; left

(defn thumb-layout [shape]
  (union
    (thumb-r-place shape)
    (thumb-m-place shape)
    (thumb-l-place shape)
    ))

(def thumbcaps (thumb-layout (sa-cap 1)))
(def thumb (thumb-layout single-plate))
(def thumb-fill (thumb-layout filled-plate))
(def thumb-space-below (thumb-layout switch-bottom))
(def thumb-space-hotswap (thumb-layout hotswap-case-cutout))
;;;;;;;;;;
;; Case ;;
;;;;;;;;;;

(defn bottom [height p]
  (->> (project p)
       (extrude-linear {:height height :twist 0 :convexity 0})
       (translate [0 0 (- (/ height 2) 10)])))
(defn top [height p]
  (->> (project p)
       (extrude-linear {:height height :twist 0 :convexity 0})
       (translate [0 0 (- (/ height 2) 10)])))

(defn bottom-hull [& p]
  (hull p (bottom 0.001 p)))

(defn full-hull [& p]
  (hull p (top 0.001 p)))

;top of wall
(defn wall-locate1 [dx dy] [(* dx wall-thickness)                    (* dy wall-thickness)                    0])
(defn wall-locate2 [dx dy] [(* dx wall-xy-offset)                    (* dy wall-xy-offset)                    wall-z-offset])
(defn wall-locate2b [dx dy] [(* dx wall-xy-offset)                    (* dy wall-xy-offset)                    wall-z-offset2])
;(defn wall-locate3 [dx dy] [(* dx (+ wall-xy-offset wall-thickness)) (* dy (+ wall-xy-offset wall-thickness)) wall-z-offset])
	
	
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;	
;;;;;;;;START MISC TOP WEBBING;;;;;;;;;;;;;  

(def thumb-connectors

;;;;; web between tops of 3 thumb keys  ;;;;;;;;;
  (union
     (->> (triangle-hulls                                         ; top two
       (thumb-m-place web-post-tr)
       (thumb-m-place web-post-br)
       (thumb-r-place web-post-tl)
       (thumb-r-place web-post-bl)) (color RED))
     (->> (triangle-hulls                                         ; top two
       (thumb-m-place web-post-tl)
       (thumb-l-place web-post-tr)
       (thumb-m-place web-post-bl)
       (thumb-l-place web-post-br)
       (thumb-m-place web-post-bl)) (color ORA))
	   
;;;;;; Triangle mesh above thumb cluster. Reference colors to see which is which ;;;;;;;;;;;
    (->> (triangle-hulls  
      (key-place 0 cornerrow (translate (wall-locate1 -1 0) web-post-bl))
      (thumb-m-place web-post-tr)
      (thumb-m-place web-post-tl)) (color BLA))
    (->> (triangle-hulls
      (key-place 0 cornerrow (translate (wall-locate1 -1 0) web-post-bl))
      (key-place 0 cornerrow web-post-br)
	  (thumb-m-place web-post-tr) ) (color GRE))
   (->> (triangle-hulls
	  (thumb-r-place web-post-tl)
	  (thumb-m-place web-post-tr) 
      (key-place 0 cornerrow web-post-br)	  ) (color YEL))
   (->> (triangle-hulls
	  (thumb-r-place web-post-tr)
      (key-place 0 cornerrow web-post-br)
      (key-place 1 cornerrow web-post-bl)	  ) (color PIN))
    (->> (triangle-hulls 
      (thumb-r-place web-post-tr)
      (thumb-r-place web-post-tl)                     
	  (key-place 0 cornerrow web-post-br))   (color NBL))	  
    (->> (triangle-hulls
      (key-place 1 cornerrow web-post-bl)
      (key-place 1 cornerrow web-post-br)
      (thumb-r-place web-post-tr)
      (thumb-r-place web-post-tl))   (color ORA))	  

;;;;;;; Now the non thumb stuff... ;;;;;;

;;connect above col 0
	(->> (triangle-hulls
       (key-place 0 0 web-post-tl)
       (key-place 0 0 web-post-tr)
       (key-place-back-left 0 0 (translate (wall-locate1 0 1) web-post-tr))
       (key-place-back-left 0 0 (translate (wall-locate1 0 1) web-post-tl))
       (key-place 0 0 web-post-tl)
      ) (color RED))	   
	(->> (triangle-hulls
       (key-place 1 0 web-post-tl)
       (key-place 1 0 web-post-tr)
       (key-place-back-left 1 0 (translate (wall-locate1 0 1) web-post-tr))
       (key-place-back-left 1 0 (translate (wall-locate1 0 1) web-post-tl))
       (key-place 1 0 web-post-tl)
      ) (color BLA))
;above other cols	  
(for [x (range 2 (dec ncols))]
	  (->> (triangle-hulls
       (key-place x 0 web-post-tr)
       (key-place (inc x) 0 web-post-tl)
       (key-place (inc x) 0 web-post-tr)
      ; (key-place-back-left 0 0 (translate (wall-locate1 0 1) web-post-tr))
      ; (key-place-back-left 0 0 (translate (wall-locate1 0 1) web-post-tl))
      ; (key-place 0 0 web-post-tl)
      ) (color CYA))	   
)	  
;; connecting bottom key of column 3
    (if extra-col-three (->> (triangle-hulls
       (if row-web (key-place 2 lastrow web-post-br))
       (if row-web (key-place 3 lastrow web-post-bl))
       (if row-web (key-place 2 lastrow web-post-tr))
       (key-place 3 lastrow web-post-tl)
       (key-place 3 cornerrow web-post-bl)
       (key-place 3 lastrow web-post-tr)
       (key-place 3 cornerrow web-post-br)
       (if row-web (key-place 4 cornerrow web-post-bl))
      ) (color BLA)))


;; connecting bottom key of column 2	  
    (->> (triangle-hulls
       ;(key-place 1 cornerrow web-post-br)
      (key-place 2 lastrow web-post-tl)
      (key-place 2 cornerrow web-post-bl)
      (key-place 2 lastrow web-post-tr)
      (key-place 2 cornerrow web-post-br)
      ;(key-place 3 cornerrow web-post-bl)
      ) (color GRE))
	  
    (->> (triangle-hulls
      ((if row-web thumb-r-place2 thumb-r-place) web-post-br)     
      (key-place 2 lastrow web-post-bl)
      (key-place 2 lastrow web-post-br)
      (if extra-col-three (key-place 3 lastrow web-post-bl))
      ((if row-web thumb-r-place2 thumb-r-place) web-post-br) 
     ) (color PIN))
	     (->> (triangle-hulls     
      (key-place 3 lastrow web-post-bl)
      (key-place 3 lastrow web-post-br)
      (key-place 4 lastrow web-post-bl)
     ) (color BLU))
;;;;; Fill open side of inner thumb key ;;;;
  (if row-web (
    (->> (piramid-hulls
      (thumb-r-place2 web-post-tr)
      (thumb-r-place web-post-tr)
      (thumb-r-place web-post-br)
      (thumb-r-place2 web-post-br)
      ) (color CYA))
   (->> (piramid-hulls
      (key-place 2 lastrow web-post-tl)
      (key-place 1 cornerrow web-post-br)
      (thumb-r-place2 web-post-tr)
      (thumb-r-place2 web-post-br)
      (key-place 2 lastrow web-post-bl)
      (key-place 2 lastrow web-post-tl)
      ) (color WHI))
  ))))

;;;;;;;;END MISC TOP WEBBING;;;;;;;;;;;;  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  
  
; dx1, dy1, dx2, dy2 = direction of the wall. '1' for front, '-1' for back, '0' for 'not in this direction'.
; place1, place2 = function that places an object at a location, typically refers to the center of a key position.
; post1, post2 = the shape that should be rendered
(defn wall-brace [place1 dx1 dy1 post1 place2 dx2 dy2 post2]
  (union
    (->> (hull
      (place1 post1)
      (place1 (translate (wall-locate1 dx1 dy1) post1))
       (place1 (translate (wall-locate2 dx1 dy1) post1))
      (place2 post2)
      (place2 (translate (wall-locate1 dx2 dy2) post2))
       (place2 (translate (wall-locate2 dx2 dy2) post2))
      )
    (color BRO))
    (->> (bottom-hull
      (place1 (translate (wall-locate2 dx1 dy1) post1))
      (place2 (translate (wall-locate2 dx2 dy2) post2))
      )
     (color ORA))
  ))
(defn straight-wall-brace [place1 dx1 dy1 post1 place2 dx2 dy2 post2]
  (union
    (->> (hull
      (place1 post1)
       (place1 (translate (wall-locate1 dx1 dy1) post1))
       (place1 (translate (wall-locate2b dx1 dy1) post1))
      (place2 post2)
       (place2 (translate (wall-locate1 dx2 dy2) post2))
       (place2 (translate (wall-locate2b dx2 dy2) post2))
      )
    (color BRO))
    (->> (bottom-hull
      (place1 (translate (wall-locate2b dx1 dy1) post1))
      (place2 (translate (wall-locate2b dx2 dy2) post2))
      )
     (color ORA))
  ))
 (defn normal-to-straight-brace [place1 dx1 dy1 post1 place2 dx2 dy2 post2]
  (union
    (->> (hull
      (place1 post1)
      (place1 (translate (wall-locate1 dx1 dy1) post1))
       (place1 (translate (wall-locate2 dx1 dy1) post1))
      (place2 post2)
      (place2 (translate (wall-locate1 dx2 dy2) post2))
       (place2 (translate (wall-locate2b dx2 dy2) post2))
      )
    (color BRO))
    (->> (bottom-hull
      (place1 (translate (wall-locate2 dx1 dy1) post1))
      (place2 (translate (wall-locate2b dx2 dy2) post2))
      )
     (color ORA))
  ))

(defn wall-brace-back [place1 dx1 dy1 post1 place2 dx2 dy2 post2]
   (union
    (->> (hull (place1 post1) (place1 (translate (wall-locate1 dx1 dy1) post1))
                              (place1 (translate (wall-locate2 dx1 dy1) post1))
			   (place2 post2) (place2 (translate (wall-locate1 dx2 dy2) post2))
                               place2 (translate (wall-locate2 dx2 dy2) post2))
      ) (color BLU))
    (->> (bottom-hull
      (place1 (translate (wall-locate2 dx1 dy1) post1))
      (place2 (translate (wall-locate2 dx2 dy2) post2))
     ) (color RED)))

(defn key-wall-brace [x1 y1 dx1 dy1 post1 x2 y2 dx2 dy2 post2]
          (wall-brace (partial key-place x1 y1) dx1 dy1 post1
                      (partial key-place x2 y2) dx2 dy2 post2))

(defn key-wall-brace-back-left [x1 y1 dx1 dy1 post1 x2 y2 dx2 dy2 post2]
  (wall-brace 
              (partial key-place-back-left x1 y1) dx1 dy1 post1
              (partial key-place-back-left x2 y2) dx2 dy2 post2))
(defn key-wall-brace-back-left-transition [x1 y1 dx1 dy1 post1 x2 y2 dx2 dy2 post2]
  (wall-brace 
              (partial key-place-back-left x1 y1) dx1 dy1 post1
              (partial key-place x2 y2) dx2 dy2 post2))
			  			  
			  			  

(def last-extra-col (if extra-col-five 5 (if extra-col-four 4 (if extra-col-three 3 2))))
(def case-walls
  (union
 ;;;;;;; RIGHT WALL ;;;;;;;;;
	(->> (normal-to-straight-brace (partial key-place lastcol 0) 1 1 fat-web-post-tr (partial key-place lastcol 0) 1 1 fat-web-post-tr)(color WHI))
	(for [y (range 0 (inc lastrow))] (straight-wall-brace (partial key-place lastcol y)  1 0 fat-web-post-tr 
														  (partial key-place lastcol y) 1 0 fat-web-post-br))
    (for [y (range 1 (inc lastrow))] (straight-wall-brace (partial key-place lastcol (dec y)) 1 0 fat-web-post-br 
														  (partial key-place lastcol y) 1 0 fat-web-post-tr))
    ;(->> (normal-to-straight-brace (partial key-place lastcol cornerrow) 0 -1 fat-web-post-br 
	;							   (partial key-place lastcol cornerrow) 1 -1 fat-web-post-br)(color RED))	 
    
 ;;;;; BACK WALL ;;;;;;;;;;;
	;the back left corner is weird because it needs to extend to have a flat surface to fit the controller holder. 
	(->> (key-wall-brace-back-left-transition 0 0 0 1 web-post-tl    0 0 -1 0 web-post-tl) (color GRE))
	(key-wall-brace-back-left                 0 0 0 1 web-post-tl      2  0 0 1 web-post-tr)
	;(->> (key-wall-brace-back-left            1 0 0 1 web-post-tl      0  0 0 1 web-post-tr) (color BLU))
	;(key-wall-brace 1 0 0 1 web-post-tl            0 0 1 web-post-tr)
	;(key-wall-brace 2 0 0 1 web-post-tl          2  0 0 1 web-post-tr)
	;(key-wall-brace 1 0 0 1 web-post-tr          2  0 0 1 web-post-tl)
	;rest of back wall, smoother transitions
    ;(for [x (range 1 ncols)] (key-wall-brace x 0 0 1 web-post-tl          x  0 0 1 web-post-tr))     ;behind key
    (for [x (range 3 ncols)] (key-wall-brace x 0 0 1 web-post-tr (dec x) 0 0 1 web-post-tr)) ; connectors
	
 ;;;;;;; LEFT WALL ;;;;;;;;;;;
    (for [y (range 0 lastrow)] (key-wall-brace 0       y -1 0 web-post-tl 0 y -1 0 web-post-bl))
    (for [y (range 1 lastrow)] (key-wall-brace 0 (dec y) -1 0 web-post-bl 0 y -1 0 web-post-tl))
    (->> (wall-brace (partial   key-place 0 cornerrow)   -1 0 web-post-bl thumb-m-place 0 1 web-post-tl) (color WHI))
    
 ;;;;;; FRONT WALL ;;;;;;;;;;;;;;
	(if extra-col-four (->> (key-wall-brace 3 lastrow 1   -1 web-post-bl 4   lastrow 0 -1 web-post-bl) (color PUR))
	    (key-wall-brace 3 lastrow 1   -1 web-post-bl 3   lastrow 1 -1 web-post-br))
    (if extra-col-four (->> (key-wall-brace 4 lastrow 0   -1 web-post-bl 4   lastrow 1 -1 web-post-br)(color YEL)))
    ;(if extra-col-four (if extra-col-five (->> (key-wall-brace 4 lastrow 0   -1 web-post-br 5   lastrow 1 -1 web-post-br)(color RED)) 
	;					(->> (straight-wall-brace (partial key-place      last-extra-col  lastrow)   1 -1 web-post-br 
    ;                          (partial key-place (inc last-extra-col) cornerrow) 0 -1 fat-web-post-bl) (color CYA))
	;))
   
    (->> (normal-to-straight-brace (partial key-place last-extra-col  lastrow)   1 -1 fat-web-post-br 
	                               (partial key-place last-extra-col  lastrow)   1 -1 fat-web-post-br) (color MAG))
 ;front wall for outer two columns
 ;   (->> (for [x (range (+ last-extra-col 1) ncols)] (key-wall-brace x cornerrow   1 -1 fat-web-post-bl      
  ;                                                              x  cornerrow -1 -1 fat-web-post-br))  (color GRE))
 ;front wall between outer two columns
  ;  (->> (for [x (range (+ last-extra-col 2) ncols)] (key-wall-brace x  cornerrow 0 -1 fat-web-post-bl 
	;                                                       (dec x) cornerrow 0 -1 fat-web-post-br))(color YEL))
 
 ;;;;;;;;;; THUMB WALLS ;;;;;;;;;;;;
    ;first one for inner thumb key can block adjacent bottom row key
    (->> (straight-wall-brace thumb-r-place  0 -1     web-post-br thumb-r-place  0 -1     web-post-bl) (color ORA))
    (->> (straight-wall-brace thumb-m-place  0 -1     web-post-br thumb-m-place  0 -1     web-post-bl) (color YEL))
    (->> (straight-wall-brace thumb-l-place  0 -1     web-post-br thumb-l-place  0 -1     web-post-bl) (color GRE))
    (->> (wall-brace thumb-l-place           0  1     web-post-tr thumb-l-place  0  1     web-post-tl) (color CYA))
    (->> (straight-wall-brace thumb-l-place -1  0 fat-web-post-tl thumb-l-place -1  0 fat-web-post-bl) (color BLU))
    ; thumb corners
    (->> (straight-wall-brace thumb-l-place     -1 0     web-post-bl thumb-l-place   0 -1     web-post-bl) (color NBL))
    (->> (normal-to-straight-brace thumb-l-place 0 1 fat-web-post-tl thumb-l-place  -1  0 fat-web-post-tl) (color PUR))
    ; thumb tweeners
    (->> (straight-wall-brace thumb-r-place  0 -1 web-post-bl thumb-m-place  0 -1 web-post-br) (color PIN))
    (->> (straight-wall-brace thumb-m-place  0 -1 web-post-bl thumb-l-place  0 -1 web-post-br) (color MAG))
    (->> (wall-brace thumb-m-place           0  1 web-post-tl thumb-l-place  0  1 web-post-tr) (color BRO))
	;wall from thumb to finger out front 
    (->> (normal-to-straight-brace (partial key-place (if extra-col-three 3 2) lastrow) 1 -1 (if extra-col-three web-post-bl web-post-br)
                                                    thumb-r-place 0 -1 web-post-br)  (color WHI))
))
    

(def usb-holder 
                (mirror [-1 0 0]
                    (import "../things/usb_holder_w_reset.stl")
                )
)
(def usb-holder-cutout-height 30.3)
(def usb-holder-clearance 0.05)
(def usb-holder-bottom-offset 0.05)

(def usb-holder-offset-coordinates (case nrows 
    6 [-24.9 (if use_hotswap 67.3 70.9) usb-holder-bottom-offset]
    5 [-39 (if use_hotswap 57.3 55.5) usb-holder-bottom-offset]
    4 [-35.5 (if use_hotswap 55.28 50.9) usb-holder-bottom-offset]))
(def usb-holder (translate usb-holder-offset-coordinates usb-holder))
(def usb-holder-space
  (translate [0 0 (/ usb-holder-bottom-offset 2)]
  (extrude-linear {:height usb-holder-cutout-height :twist 0 :convexity 0}
                  (offset usb-holder-clearance
                          (project usb-holder))))
  )

(def model-right
  (difference
    (union
      key-holes
      connectors
      thumb
      thumb-connectors
      (difference case-walls usb-holder-space))
    (translate [0 0 -20] (cube 350 350 40))
    thumb-space-hotswap
    ))
;
(def model-left
  (mirror [-1 0 0] model-right)
)
(if left-half 
	  (spit "things/left.scad" (write-scad model-left))
	  (spit "things/right.scad" (write-scad model-right)))
	  
(def bottom-outline     (cut (translate [0 0 -0.1] case-walls)))
(def inner-thing        (translate [0 0 -0.1] (project (union (extrude-linear {:height 99 :scale  0.1 :center true} bottom-outline)
						(cube 50 50 bottom-plate-thickness)))))	
								 
(def bottom-plate (extrude-linear {:height bottom-plate-thickness} inner-thing))
(spit "things/plate.scad" (write-scad bottom-plate))

(def wall-shape (cut (translate [0 0 -0.1] (difference case-walls usb-holder-space))))
(def bottom-height-half (/ bottom-height 2))
(def bottom-plate-inset (translate [0 0 bottom-height-half] (extrude-linear {:height 2 :twist 0 :convexity 0}
                                                        (difference inner-thing (offset 0.1 wall-shape)))))


(spit "things/plate-inset.scad"   (write-scad bottom-plate-inset ))

(spit "things/plate-cut.scad" (write-scad (cut (translate [0 0 (- bottom-height)]  bottom-plate ))))
						 

						 
(spit "things/test.scad"
      (write-scad(difference (union
            model-right 
            caps
            thumbcaps
            (debug key-space-below)
            (debug thumb-space-below)
            (debug thumb-space-hotswap)
            (debug usb-holder)
			;(debug bottom-plate)
            )
          (translate [0 0 -20] (cube 350 350 40)))))

		  
;(spit "things/plate-test.scad"
;      (write-scad(difference (union
;            bottom-plate-inset
;            (debug bottom-plate)
;            )
;          (translate [0 0 -20] (cube 350 350 40)))))

