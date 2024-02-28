;;;;;;;;;;;;;;;;;;;;;
;## Starting code ;;;
;;;;;;;;;;;;;;;;;;;;;
	(ns dactyl-keyboard.dactyl
		(:refer-clojure :exclude [use import])
		(:require [clojure.core.matrix :refer [array matrix mmul]]
							[scad-clj.scad :refer :all]
							[scad-clj.model :refer :all]
							[unicode-math.core :refer :all])
		;(use (incanter core stats charts io)))
		)

;;;;;;;;;;;;;;;;;;;;;
;## Added methods ;;;
;;;;;;;;;;;;;;;;;;;;;

	(defmethod write-expr :fill [depth [form & block]]
		(concat
		(list (indent depth) "fill () {\n")
		(mapcat #(write-expr (inc depth) %1) block)
		(list (indent depth) "}\n")))

	(defn fill [& block]
		`(:fill ~block))

	(defmethod write-expr :offsetd [depth [form {:keys [delta]} & block]]
		(concat
		(list (indent depth) "offset (")
		(list "delta = " delta)
		(list ") {\n")
		(mapcat #(write-expr (inc depth) %1) block)
		(list (indent depth) "}\n")))

	(defn offsetd [delta & block]
		`(:offsetd {:delta ~delta} ~@block))

	(defn deg2rad [degrees]
		(* (/ degrees 180) pi))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Configuration parameters ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

	;;; Script controled variables
		;(def nrows 4)								; number of rows
		;(def ncols 6)								; number of columns
		;(def thumb-style "default")	; toggles between "default", "mini", "cf" and "tightly" thumb cluster
		;(def pinky-15u true)          ; controls whether the outer column uses 1.5u keys
		;(def extra-row true)					; adds an extra bottom row to the outer column
		;(def plate-outside false)			; if false then the plate will be inset (true for cool ligthning effect)
		;(def print-type "fdm")				; ("fdm" or "msla") type of printing to determine heat insert holes
	;;; Hand rest parameters
		(def hand-rest-width 70)
		(def hand-rest-length 100)
		(def hand-rest-height 40) ;30
		(def hand-rest-top 0.9) ; side angles of hand rest, smaller number, smaller top
		(def hand-rest-radius 3)
		(def hand-rest-x 15) ;14 front-back tilt
		(def hand-rest-y 8) ;7 left-right tilt
		(def hand-rest-lip-width 1)
		(def hand-rest-lip-height 5);5
		(def hand-rest-position-x 15)
		(def hand-rest-position-y -114)

		(def pad-off 6.5) ; pad offset from edge on hand-rest
		(def pad-r 5.25) 	; radius od pad
		(def pad-z 1.5) 	; pad depth on plate
		(def text-z 1.5)	; text depth
	;;; General parameters
		(def custom true) 						; Use this to write the keyboard to "things/custom"
		(def create-side-nubs? false)	; true for Cherry MX and Gateron; false for Kailh and simillar
		(def hot-swap true) 					; If you want hot swap sockets enable this
		(def show-caps false)					; Show keycaps on the keyboard (not working for all sizes)
		(def first-15u-row 0)					; controls which should be the first row to have 1.5u keys on the outer column
		(def end-15u-row 99) 					; (99 for all) controls which should be the last row to have 1.5u keys on the outer column
		(def α (/ π 12))              ; curvature of the columns
		(def β (/ π 36))              ; curvature of the rows
		(def centerrow (- nrows 3))   ; controls front-back tilt
		(def centercol 3)             ; controls left-right tilt / tenting (higher number is more tenting)
		(def tenting-angle (deg2rad 15))	; standard: (/ π 12) = 15 degrees ; change this for more precise tenting control
		(def inner-column false)      ; adds an extra inner column (two less rows than nrows)
		(def column-style :standard)
		
		(def keyboard-z-offset 15);15           ; controls overall height; original=9 with centercol=3; use 16 for centercol=2

		(def extra-width 3);               			; (2.5 for 5 row, 3 is min for 6 row) extra space between the base of keys, between columns only (X axis); original= 2
		(def extra-height 1);1                  ; original= 0.5

		(def wall-z-offset -8);-8               ; length of the first downward-sloping part of the wall (negative)
		(def wall-xy-offset 5);5                ; offset in the x and/or y direction for the first downward-sloping part of the wall (negative)
		(def wall-thickness 2);2                ; wall thickness parameter; originally 5
	;;; Offset parameters
		(defn column-offset [column]
			(if inner-column
				(cond (<= column 1) [0 -2 0]
							(= column 3) [0 2.82 -4.5]
							(>= column 5) [0 -12 5.64]    ; original [0 -5.8 5.64]
							:else [0 0 0])
				(cond (= column 2) [0 2.82 -4.5]
							(>= column 4) [0 -12 5.64]    ; original [0 -5.8 5.64]
							:else [0 0 0])))

		(def thumb-offsets
			(case thumb-style
				"default" [6 -3 7]
				"mini" [6 -3 7]
				"cf" [6 -3 7]
				"tightly" [8 -5 1])); 8 -5 1

	;## Placement of magnets on case (hard-coded)
		(def hook1-placement-x 3.5)
		(def hook1-placement-y (+ -59.22 0.88))
		(if extra-row
			(do
				(def hook2-placement-x 25)
				(def hook2-placement-y (+ -71.22 0.88))
			)
			(do
				(def hook2-placement-x 25)
				(def hook2-placement-y (+ -50.38 0.88))
			)
		)
	;## Settings for column-style == :fixed
		;; The defaults roughly match Maltron settings
		;; http://patentimages.storage.googleapis.com/EP0219944A2/imgf0002.png
		;; Fixed-z overrides the z portion of the column ofsets above.
		;; NOTE: THIS DOESN'T WORK QUITE LIKE I'D HOPED.
		(def fixed-angles [(deg2rad 10) (deg2rad 10) 0 0 0 (deg2rad -15) (deg2rad -15)])
		(def fixed-x [-41.5 -22.5 0 20.3 41.4 65.5 89.6])  ; relative to the middle finger
		(def fixed-z [12.1    8.3 0  5   10.7 14.5 17.5])
		(def fixed-tenting (deg2rad 0))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Calculations before building ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

	; determine pinky 1.5u keys
	(if (and extra-row (= end-15u-row 99))                           
		(def last-15u-row (- nrows 1))) 
	(if (and (not extra-row) (= end-15u-row 99))
		(def last-15u-row (- nrows 2)))
	(if (not= end-15u-row 99)
		(def last-15u-row end-15u-row))

	; parameters for building
	(def lastrow (dec nrows))
	(def cornerrow (dec lastrow))
	(def lastcol (dec ncols))
	(def extra-cornerrow (if extra-row lastrow cornerrow))
	(def innercol-offset (if inner-column 1 0))

;;;;;;;;;;;;;;;;;;;
;;; Switch Hole ;;;
;;;;;;;;;;;;;;;;;;;

	(def keyswitch-height 14.1)
	(def keyswitch-width 14.1)

	(def sa-profile-key-height 12.7)

	(def plate-thickness 3.5)
	(def side-nub-thickness 4)
	(def retention-tab-thickness 1.5)
	(def retention-tab-hole-thickness (- (+ plate-thickness 0.5) retention-tab-thickness))
	(def mount-width (+ keyswitch-width 3.2))
	(def mount-height (+ keyswitch-height 2.7))

	(def socket-height-adjust 1.2)

	(def hot-socket
		(difference
			(difference
				(translate [0 0 (- -2.05 (/ socket-height-adjust 2))]
					(cube (+ keyswitch-height 3.6) (+ keyswitch-width 3) (+ 3.1 socket-height-adjust)))
				(translate [0 0 (- (/ socket-height-adjust -2) 0.5)]
					(cube keyswitch-height keyswitch-width socket-height-adjust)))

			; corner hot-fix for tightly
			(rotate [(deg2rad -25) 0 (deg2rad 60)]
				(translate [0 16.2 0]
					(cube 10 10 10)))
			; other corner fix
			(rotate [0 (deg2rad 25) (deg2rad 40)]
						(translate [16.5 3 5]
												(cube 10 10 10)))

			; hot-swap socket hole
			;(scale [1 1 1]
				(translate [0.075 4.815 (- -2.75 socket-height-adjust)]
					(union
						; cube1
						(cube 119.6 114.1 2)
						; circle1
						(translate [-4.8 0.55 0]
							(binding [*fn* 100] (cylinder 1.5 2)))
						(translate [-3.35 -1.75 0]
							(difference
								;cube2
								(cube 5.9 4.6 2)
								;circle2
								(translate [2.95 -2.55 0]
									(binding [*fn* 100] (cylinder 2.25 2)))))
						(translate [6 0.325 0]
							(cube 6 1.8 2))
						(translate [-6 -2.215 0]
							(cube 6 1.8 2))
						(translate [2.475 0.325 0]
							(binding [*fn* 200] (cylinder 1.7 20)))
						(translate [-3.875 -2.215 0]
							(binding [*fn* 200] (cylinder 1.7 20)))))
				(binding [*fn* 100] (cylinder 2.3 20))
				(translate [-5.08 0 0]
					(binding [*fn* 100] (cylinder 1.1 20))
					(translate [2 -0.4 0]
						(cube 4 4 10)))
				(translate [5.08 0 0]
					(binding [*fn* 100] (cylinder 1.1 20)));)
				(translate [0 (/ (+ keyswitch-width 3) -4) (- -2.05 socket-height-adjust)]
					(cube (+ keyswitch-height 3.6) (/ (+ keyswitch-width 3) 2) 3.1))
			;(binding [*fn* 50] (cylinder 2 2))
		)
	)

	(def fill-caps
		(hull
		(union
			(translate [0 0 (- 9.75 0)]
								(cube 18.3 18.3 0.1))
			(translate [0 0 (- 17.25 0)]
								(cube 12.2 12.2 0.1)))))

	(def single-plate
		(let [top-wall (->> (cube (+ keyswitch-width 3) 1.5 (+ plate-thickness 0.5))
												(translate [0
																		(+ (/ 1.5 2) (/ keyswitch-height 2))
																		(- (/ plate-thickness 2) 0.25)]))
					left-wall (->> (cube 1.8 (+ keyswitch-height 3) (+ plate-thickness 0.5))
												(translate [(+ (/ 1.8 2) (/ keyswitch-width 2))
																		0
																		(- (/ plate-thickness 2) 0.25)]))
					side-nub (->> (binding [*fn* 30] (cylinder 1 2.75))
												(rotate (/ π 2) [1 0 0])
												(translate [(+ (/ keyswitch-width 2)) 0 1])
												(hull (->> (cube 1.5 2.75 side-nub-thickness)
																	(translate [(+ (/ 1.5 2) (/ keyswitch-width 2))
																							0
																							(/ side-nub-thickness 2)])))
												(translate [0 0 (- plate-thickness side-nub-thickness)]))
					plate-half (union top-wall left-wall (if create-side-nubs? (with-fn 100 side-nub)))
					top-nub (->> (cube 5 5 retention-tab-hole-thickness)
											(translate [(+ (/ keyswitch-width 2.5)) 0 (- (/ retention-tab-hole-thickness 2) 0.5)]))
					top-nub-pair (union top-nub
															(->> top-nub
																	(mirror [1 0 0])
																	(mirror [0 1 0])))]
			(difference
			(union plate-half
							(->> plate-half
									(mirror [1 0 0])
									(mirror [0 1 0]))
							(if hot-swap (mirror [0 0 0] hot-socket))
							(if show-caps fill-caps))
			(->>
				top-nub-pair
				(rotate (/ π 2) [0 0 1])))))

;;;;;;;;;;;;;;;;;;
;;; SA Keycaps ;;;
;;;;;;;;;;;;;;;;;;

	(def sa-length 18.415)
	(def sa-double-length 37.5)
	(def sa-cap {1 (let [bl2 (/ sa-length 2)
											m (/ 17 2)
											key-cap (hull (->> (polygon [[bl2 bl2] [bl2 (- bl2)] [(- bl2) (- bl2)] [(- bl2) bl2]])
																					(extrude-linear {:height 0.1 :twist 0 :convexity 0})
																					(translate [0 0 0.05]))
																		(->> (polygon [[m m] [m (- m)] [(- m) (- m)] [(- m) m]])
																					(extrude-linear {:height 0.1 :twist 0 :convexity 0})
																					(translate [0 0 3.7]))
																		(->> (polygon [[6 6] [6 -6] [-6 -6] [-6 6]])
																					(extrude-linear {:height 0.1 :twist 0 :convexity 0})
																					(translate [0 0 7.4])))]
									(->> key-cap
												(translate [0 0 (+ 6 plate-thickness)])
												(color [220/255 163/255 163/255 1])))
							2 (let [bl2 sa-length
											bw2 (/ sa-length 2)
											key-cap (hull (->> (polygon [[bw2 bl2] [bw2 (- bl2)] [(- bw2) (- bl2)] [(- bw2) bl2]])
																					(extrude-linear {:height 0.1 :twist 0 :convexity 0})
																					(translate [0 0 0.05]))
																		(->> (polygon [[6 16] [6 -16] [-6 -16] [-6 16]])
																					(extrude-linear {:height 0.1 :twist 0 :convexity 0})
																					(translate [0 0 12])))]
									(->> key-cap
												(translate [0 0 (+ 5 plate-thickness)])
												(color [127/255 159/255 127/255 1])))
							1.5 (let [bl2 (/ sa-length 2)
												bw2 (/ (* sa-length 1.5) 2)
												key-cap (hull (->> (polygon [[bw2 bl2] [bw2 (- bl2)] [(- bw2) (- bl2)] [(- bw2) bl2]])
																						(extrude-linear {:height 0.1 :twist 0 :convexity 0})
																						(translate [0 0 0.05]))
																			(->> (polygon [[11 6] [-11 6] [-11 -6] [11 -6]])
																						(extrude-linear {:height 0.1 :twist 0 :convexity 0})
																						(translate [0 0 7.4])))]
										(->> key-cap
													(translate [0 0 (+ 5 plate-thickness)])
													(color [240/255 223/255 175/255 1])))})

	;; Fill the keyholes instead of placing a a keycap over them
	(def keyhole-fill (->> (cube keyswitch-height keyswitch-width plate-thickness)
												(translate [0 0 (/ plate-thickness 2)])))

;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Placement Functions ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;

	(def columns (range (+ innercol-offset 0) ncols))
	(def rows (range 0 nrows))

	(def innercolumn 0)
	(def innerrows (range 0 (- nrows 2)))

	(def cap-top-height (+ plate-thickness sa-profile-key-height))
	(def row-radius (+ (/ (/ (+ mount-height extra-height) 2)
												(Math/sin (/ α 2)))
										cap-top-height))
	(def column-radius (+ (/ (/ (+ mount-width extra-width) 2)
													(Math/sin (/ β 2)))
												cap-top-height))
	(def column-x-delta (+ -1 (- (* column-radius (Math/sin β)))))

	(defn offset-for-column [col, row]
		(if (and pinky-15u
						(= col lastcol)
						(<= row last-15u-row)
						(>= row first-15u-row))
			4.7625
			0))

	(defn apply-key-geometry [translate-fn rotate-x-fn rotate-y-fn column row shape]
		(let [column-angle (* β (- centercol column))
					placed-shape (->> shape
														(translate-fn [(offset-for-column column, row) 0 (- row-radius)])
														(rotate-x-fn  (* α (- centerrow row)))
														(translate-fn [0 0 row-radius])
														(translate-fn [0 0 (- column-radius)])
														(rotate-y-fn  column-angle)
														(translate-fn [0 0 column-radius])
														(translate-fn (column-offset column)))
					column-z-delta (* column-radius (- 1 (Math/cos column-angle)))
					placed-shape-ortho (->> shape
																	(translate-fn [0 0 (- row-radius)])
																	(rotate-x-fn  (* α (- centerrow row)))
																	(translate-fn [0 0 row-radius])
																	(rotate-y-fn  column-angle)
																	(translate-fn [(- (* (- column centercol) column-x-delta)) 0 column-z-delta])
																	(translate-fn (column-offset column)))
					placed-shape-fixed (->> shape
																	(rotate-y-fn  (nth fixed-angles column))
																	(translate-fn [(nth fixed-x column) 0 (nth fixed-z column)])
																	(translate-fn [0 0 (- (+ row-radius (nth fixed-z column)))])
																	(rotate-x-fn  (* α (- centerrow row)))
																	(translate-fn [0 0 (+ row-radius (nth fixed-z column))])
																	(rotate-y-fn  fixed-tenting)
																	(translate-fn [0 (second (column-offset column)) 0]))]
			(->> (case column-style
						:orthographic placed-shape-ortho
						:fixed        placed-shape-fixed
						placed-shape)
					(rotate-y-fn  tenting-angle)
					(translate-fn [0 0 keyboard-z-offset]))))

	(defn key-place [column row shape]
		(apply-key-geometry translate
												(fn [angle obj] (rotate angle [1 0 0] obj))
												(fn [angle obj] (rotate angle [0 1 0] obj))
												column row shape))

	(defn rotate-around-x [angle position]
		(mmul
		[[1 0 0]
			[0 (Math/cos angle) (- (Math/sin angle))]
			[0 (Math/sin angle)    (Math/cos angle)]]
		position))

	(defn rotate-around-y [angle position]
		(mmul
		[[(Math/cos angle)     0 (Math/sin angle)]
			[0                    1 0]
			[(- (Math/sin angle)) 0 (Math/cos angle)]]
		position))

	(defn key-position [column row position]
		(apply-key-geometry (partial map +) rotate-around-x rotate-around-y column row position))

	(def key-holes
		(apply union
					(for [column columns
								row rows
								:when (or (.contains [(+ innercol-offset 2) (+ innercol-offset 3)] column)
													(and (.contains [(+ innercol-offset 4) (+ innercol-offset 5)] column) extra-row (= ncols (+ innercol-offset 6)))
													(and (.contains [(+ innercol-offset 4)] column) extra-row (= ncols (+ innercol-offset 5)))
													(and inner-column (not= row cornerrow) (= column 0))
													(not= row lastrow))]
						(->> single-plate
									;                (rotate (/ π 2) [0 0 1])
									(key-place column row)))
					;(key-place 5 5 (single-plate))
					))

	(def key-holes-left
		(apply union
					(for [column columns
								row rows
								:when (or (.contains [(+ innercol-offset 2) (+ innercol-offset 3)] column)
													(and (.contains [(+ innercol-offset 4) (+ innercol-offset 5)] column) extra-row (= ncols (+ innercol-offset 6)))
													(and (.contains [(+ innercol-offset 4)] column) extra-row (= ncols (+ innercol-offset 5)))
													(and inner-column (not= row cornerrow) (= column 0))
													(not= row lastrow))]
						(->> (mirror [1 0 0] single-plate)
									;                (rotate (/ π 2) [0 0 1])
									(key-place column row)))))
	(def caps
		(apply union
					(conj (for [column columns
											row rows
											:when (or (and (= column 0) (< row 3))
																(and (.contains [1 2] column) (< row 4))
																(.contains [3 4 5 6] column))]
									(->> (sa-cap (if (and pinky-15u (= column lastcol) (not= row lastrow)) 1.5 1))
												(key-place column row)))
								(list (key-place 0 0 (sa-cap 1))
											(key-place 0 1 (sa-cap 1))
											(key-place 0 2 (sa-cap 1))))))

	(def caps-fill
		(apply union
					(conj (for [column columns
											row rows
											:when (or (.contains [(+ innercol-offset 2) (+ innercol-offset 3)] column)
																(and (.contains [(+ innercol-offset 4) (+ innercol-offset 5)] column) extra-row (= ncols (+ innercol-offset 6)))
																(and (.contains [(+ innercol-offset 4)] column) extra-row (= ncols (+ innercol-offset 5)))
																(and inner-column (not= row cornerrow) (= column 0))
																(not= row lastrow))]
									(key-place column row keyhole-fill))
								(list (key-place 0 0 keyhole-fill)
											(key-place 0 1 keyhole-fill)
											(key-place 0 2 keyhole-fill)))))

	;placement for the innermost column
	(def key-holes-inner
		(if inner-column
			(apply union
						(for [row innerrows]
							(->> single-plate
										;               (rotate (/ π 2) [0 0 1])
										(key-place 0 row))))))

;;;;;;;;;;;;;;;;;;;;;;
;;; Web Connectors ;;;
;;;;;;;;;;;;;;;;;;;;;;

	(def web-thickness 4)
	(def post-size 0.1)
	(def web-post (->> (cube post-size post-size web-thickness)
										(translate [0 0 (+ (/ web-thickness -2)
																				plate-thickness)])))

	(def post-adj (/ post-size 2))
	(def web-post-tr (translate [(- (/ mount-width 1.95) post-adj) (- (/ mount-height 1.95) post-adj) 0] web-post))
	(def web-post-tl (translate [(+ (/ mount-width -1.95) post-adj) (- (/ mount-height 1.95) post-adj) 0] web-post))
	(def web-post-bl (translate [(+ (/ mount-width -1.95) post-adj) (+ (/ mount-height -1.95) post-adj) 0] web-post))
	(def web-post-br (translate [(- (/ mount-width 1.95) post-adj) (+ (/ mount-height -1.95) post-adj) 0] web-post))

	; wide posts for 1.5u keys in the main cluster
	(if pinky-15u
		(do (def wide-post-tr (translate [(- (/ mount-width 1.2) post-adj)  (- (/ mount-height  2) post-adj) 0] web-post))
				(def wide-post-tl (translate [(+ (/ mount-width -1.2) post-adj) (- (/ mount-height  2) post-adj) 0] web-post))
				(def wide-post-bl (translate [(+ (/ mount-width -1.2) post-adj) (+ (/ mount-height -2) post-adj) 0] web-post))
				(def wide-post-br (translate [(- (/ mount-width 1.2) post-adj)  (+ (/ mount-height -2) post-adj) 0] web-post)))
		(do (def wide-post-tr web-post-tr)
				(def wide-post-tl web-post-tl)
				(def wide-post-bl web-post-bl)
				(def wide-post-br web-post-br)))

	(defn triangle-hulls [& shapes]
		(apply union
					(map (partial apply hull)
								(partition 3 1 shapes))))

	(def connectors
		(apply union
					(concat
						;; Row connections
						(for [column (range (+ innercol-offset 0) (dec ncols))
									row (range 0 lastrow)]
							(triangle-hulls
							(key-place (inc column) row web-post-tl)
							(key-place column row web-post-tr)
							(key-place (inc column) row web-post-bl)
							(key-place column row web-post-br)))

						;; Column connections
						(for [column columns
									row (range 0 cornerrow)]
							(triangle-hulls
							(key-place column row web-post-bl)
							(key-place column row web-post-br)
							(key-place column (inc row) web-post-tl)
							(key-place column (inc row) web-post-tr)))

						;; Diagonal connections
						(for [column (range 0 (dec ncols))
									row (range 0 cornerrow)]
							(triangle-hulls
							(key-place column row web-post-br)
							(key-place column (inc row) web-post-tr)
							(key-place (inc column) row web-post-bl)
							(key-place (inc column) (inc row) web-post-tl))))))

	(def inner-connectors
		(if inner-column
			(apply union
						(concat
							;; Row connections
							(for [column (range 0 1)
										row (range 0 (- nrows 2))]
								(triangle-hulls
								(key-place (inc column) row web-post-tl)
								(key-place column row web-post-tr)
								(key-place (inc column) row web-post-bl)
								(key-place column row web-post-br)))

							;; Column connections
							(for [row (range 0 (dec cornerrow))]
								(triangle-hulls
								(key-place innercolumn row web-post-bl)
								(key-place innercolumn row web-post-br)
								(key-place innercolumn (inc row) web-post-tl)
								(key-place innercolumn (inc row) web-post-tr)))

							;; Diagonal connections
							(for [column (range 0 (dec ncols))
										row (range 0 2)]
								(triangle-hulls
								(key-place column row web-post-br)
								(key-place column (inc row) web-post-tr)
								(key-place (inc column) row web-post-bl)
								(key-place (inc column) (inc row) web-post-tl)))))))

	(def extra-connectors
		(if extra-row
			(apply union
						(concat
							(for [column (range 3 ncols)
										row (range cornerrow lastrow)]
								(triangle-hulls
								(key-place column row web-post-bl)
								(key-place column row web-post-br)
								(key-place column (inc row) web-post-tl)
								(key-place column (inc row) web-post-tr)))

							(for [column (range 3 (dec ncols))
										row (range cornerrow lastrow)]
								(triangle-hulls
								(key-place column row web-post-br)
								(key-place column (inc row) web-post-tr)
								(key-place (inc column) row web-post-bl)
								(key-place (inc column) (inc row) web-post-tl)))

							(for [column (range 4 (dec ncols))
										row (range lastrow nrows)]
								(triangle-hulls
								(key-place (inc column) row web-post-tl)
								(key-place column row web-post-tr)
								(key-place (inc column) row web-post-bl)
								(key-place column row web-post-br)))))))

;;;;;;;;;;;;;;;;;;;;;
;;; Default Thumb ;;;
;;;;;;;;;;;;;;;;;;;;;

	(def thumborigin
		(map + (key-position (+ innercol-offset 1) cornerrow [(/ mount-width 2) (- (/ mount-height 2)) 0])
				thumb-offsets))

	(defn thumb-tr-place [shape]
		(->> shape
				(rotate (deg2rad  10) [1 0 0])
				(rotate (deg2rad -23) [0 1 0])
				(rotate (deg2rad  10) [0 0 1])
				(translate thumborigin)
				(translate [-12 -16 3])))
	(defn thumb-tl-place [shape]
		(->> shape
				(rotate (deg2rad  10) [1 0 0])
				(rotate (deg2rad -23) [0 1 0])
				(rotate (deg2rad  10) [0 0 1])
				(translate thumborigin)
				(translate [-32 -15 -2])))
	(defn thumb-mr-place [shape]
		(->> shape
				(rotate (deg2rad  -6) [1 0 0])
				(rotate (deg2rad -34) [0 1 0])
				(rotate (deg2rad  48) [0 0 1])
				(translate thumborigin)
				(translate [-29 -40 -13])))
	(defn thumb-ml-place [shape]
		(->> shape
				(rotate (deg2rad   6) [1 0 0])
				(rotate (deg2rad -34) [0 1 0])
				(rotate (deg2rad  40) [0 0 1])
				(translate thumborigin)
				(translate [-51 -25 -12])))
	(defn thumb-br-place [shape]
		(->> shape
				(rotate (deg2rad -16) [1 0 0])
				(rotate (deg2rad -33) [0 1 0])
				(rotate (deg2rad  54) [0 0 1])
				(translate thumborigin)
				(translate [-37.8 -55.3 -25.3])))
	(defn thumb-bl-place [shape]
		(->> shape
				(rotate (deg2rad  -4) [1 0 0])
				(rotate (deg2rad -35) [0 1 0])
				(rotate (deg2rad  52) [0 0 1])
				(translate thumborigin)
				(translate [-56.3 -43.3 -23.5])))

	(defn thumb-1x-layout [shape]
		(union
		(thumb-mr-place shape)
		(thumb-ml-place shape)
		(thumb-br-place shape)
		(thumb-bl-place shape)))

	(defn thumb-15x-layout [shape]
		(union
		(thumb-tr-place shape)
		(thumb-tl-place shape)))

	(def larger-plate
		(let [plate-height (/ (- sa-double-length mount-height) 3)
					top-plate (->> (cube mount-width plate-height web-thickness)
												(translate [0 (/ (+ plate-height mount-height) 2)
																		(- plate-thickness (/ web-thickness 2))]))]
			(union top-plate (mirror [0 1 0] top-plate))))

	(def larger-plate-half
		(let [plate-height (/ (- sa-double-length mount-height) 3)
					top-plate (->> (cube mount-width plate-height web-thickness)
												(translate [0 (/ (+ plate-height mount-height) 2)
																		(- plate-thickness (/ web-thickness 2))]))]
			(union top-plate (mirror [0 0 0] top-plate))))

	(def thumbcaps
		(union
		(thumb-1x-layout (sa-cap 1))
		(thumb-15x-layout (rotate (/ π 2) [0 0 1] (sa-cap 1.5)))))

	(def thumbcaps-fill
		(union
		(thumb-1x-layout keyhole-fill)
		(thumb-15x-layout (rotate (/ π 2) [0 0 1] keyhole-fill))))

	(def thumb
		(union
		(thumb-1x-layout (rotate (/ π 2) [0 0 0] single-plate))
		(thumb-tr-place (rotate (/ π 2) [0 0 1] single-plate))
		(thumb-tr-place larger-plate)
		(thumb-tl-place (rotate (/ π 2) [0 0 1] single-plate))
		(thumb-tl-place larger-plate-half)))

	(def thumb-left
		(union
		(thumb-1x-layout (rotate (/ π 2) [0 0 0] (mirror [1 0 0] single-plate)))
		(thumb-tr-place (rotate (/ π 2) [0 0 1] (mirror [1 0 0] single-plate)))
		(thumb-tr-place larger-plate)
		(thumb-tl-place (rotate (/ π 2) [0 0 1] (mirror [1 0 0] single-plate)))
		(thumb-tl-place larger-plate-half)))

	(def thumb-post-tr (translate [(- (/ mount-width 2) post-adj)  (- (/ mount-height  1.1) post-adj) 0] web-post))
	(def thumb-post-tl (translate [(+ (/ mount-width -2) post-adj) (- (/ mount-height  1.1) post-adj) 0] web-post))
	(def thumb-post-bl (translate [(+ (/ mount-width -2) post-adj) (+ (/ mount-height -1.1) post-adj) 0] web-post))
	(def thumb-post-br (translate [(- (/ mount-width 2) post-adj)  (+ (/ mount-height -1.1) post-adj) 0] web-post))

	(def thumb-connectors
  (union
   (triangle-hulls    ; top two
    (thumb-tl-place thumb-post-tr)
    (thumb-tl-place (translate [-0.33 -0.25 0] web-post-br))
    (thumb-tr-place thumb-post-tl)
    (thumb-tr-place thumb-post-bl))
   (triangle-hulls    ; bottom two on the right
    (thumb-br-place web-post-tr)
    (thumb-br-place web-post-br)
    (thumb-mr-place web-post-tl)
    (thumb-mr-place web-post-bl))
   (triangle-hulls    ; bottom two on the left
    (thumb-bl-place web-post-tr)
    (thumb-bl-place web-post-br)
    (thumb-ml-place web-post-tl)
    (thumb-ml-place web-post-bl))
   (triangle-hulls    ; centers of the bottom four
    (thumb-br-place web-post-tl)
    (thumb-bl-place web-post-bl)
    (thumb-br-place web-post-tr)
    (thumb-bl-place web-post-br)
    (thumb-mr-place web-post-tl)
    (thumb-ml-place web-post-bl)
    (thumb-mr-place web-post-tr)
    (thumb-ml-place web-post-br))
   (triangle-hulls    ; top two to the middle two, starting on the left
    (thumb-tl-place thumb-post-tl)
    (thumb-ml-place web-post-tr)
    (thumb-tl-place (translate [0.25 0.1 0] web-post-bl))
    (thumb-ml-place web-post-br)
    (thumb-tl-place (translate [-0.33 -0.25 0] web-post-br))
    (thumb-mr-place web-post-tr)
    (thumb-tr-place thumb-post-bl)
    (thumb-mr-place web-post-br)
    (thumb-tr-place thumb-post-br))
   (triangle-hulls    ; top two to the main keyboard, starting on the left
    (thumb-tl-place thumb-post-tl)
    (key-place (+ innercol-offset 0) cornerrow web-post-bl)
    (thumb-tl-place thumb-post-tr)
    (key-place (+ innercol-offset 0) cornerrow web-post-br)
    (thumb-tr-place thumb-post-tl)
    (key-place (+ innercol-offset 1) cornerrow web-post-bl)
    (thumb-tr-place thumb-post-tr)
    (key-place (+ innercol-offset 1) cornerrow web-post-br)
    (key-place (+ innercol-offset 2) lastrow web-post-tl)
    (key-place (+ innercol-offset 2) lastrow web-post-bl)
    (thumb-tr-place thumb-post-tr)
    (key-place (+ innercol-offset 2) lastrow web-post-bl)
    (thumb-tr-place thumb-post-br)
    (key-place (+ innercol-offset 2) lastrow web-post-br)
    (key-place (+ innercol-offset 3) lastrow web-post-bl)
    (key-place (+ innercol-offset 2) lastrow web-post-tr)
    (key-place (+ innercol-offset 3) lastrow web-post-tl)
    (key-place (+ innercol-offset 3) cornerrow web-post-bl)
    (key-place (+ innercol-offset 3) lastrow web-post-tr)
    (key-place (+ innercol-offset 3) cornerrow web-post-br))
   (triangle-hulls
    (key-place (+ innercol-offset 1) cornerrow web-post-br)
    (key-place (+ innercol-offset 2) lastrow web-post-tl)
    (key-place (+ innercol-offset 2) cornerrow web-post-bl)
    (key-place (+ innercol-offset 2) lastrow web-post-tr)
    (key-place (+ innercol-offset 2) cornerrow web-post-br)
    (key-place (+ innercol-offset 3) cornerrow web-post-bl))
   (if extra-row
     (union
      (triangle-hulls
       (key-place (+ innercol-offset 3) lastrow web-post-tr)
       (key-place (+ innercol-offset 3) lastrow web-post-br)
       (key-place (+ innercol-offset 4) lastrow web-post-tl)
       (key-place (+ innercol-offset 4) lastrow web-post-bl))
      (triangle-hulls
       (key-place (+ innercol-offset 3) lastrow web-post-tr)
       (key-place (+ innercol-offset 3) cornerrow web-post-br)
       (key-place (+ innercol-offset 4) lastrow web-post-tl)
       (key-place (+ innercol-offset 4) cornerrow web-post-bl)))
     (union
      (triangle-hulls
       (key-place (+ innercol-offset 3) lastrow web-post-tr)
       (key-place (+ innercol-offset 3) lastrow web-post-br)
       (key-place (+ innercol-offset 4) cornerrow web-post-bl))
      (triangle-hulls
       (key-place (+ innercol-offset 3) lastrow web-post-tr)
       (key-place (+ innercol-offset 3) cornerrow web-post-br)
       (key-place (+ innercol-offset 4) cornerrow web-post-bl))))))

;;;;;;;;;;;;;;;;;
;;; Mini Thumb ;;
;;;;;;;;;;;;;;;;;

	(defn minithumb-tr-place [shape]
		(->> shape
				(rotate (deg2rad  14) [1 0 0])
				(rotate (deg2rad -15) [0 1 0])
				(rotate (deg2rad  10) [0 0 1]) ; original 10
				(translate thumborigin)
				(translate [-15 -10 5]))) ; original 1.5u  (translate [-12 -16 3])
	(defn minithumb-tl-place [shape]
		(->> shape
				(rotate (deg2rad  10) [1 0 0])
				(rotate (deg2rad -23) [0 1 0])
				(rotate (deg2rad  25) [0 0 1]) ; original 10
				(translate thumborigin)
				(translate [-35 -16 -2]))) ; original 1.5u (translate [-32 -15 -2])))
	(defn minithumb-mr-place [shape]
		(->> shape
				(rotate (deg2rad  10) [1 0 0])
				(rotate (deg2rad -23) [0 1 0])
				(rotate (deg2rad  25) [0 0 1])
				(translate thumborigin)
				(translate [-23 -34 -6])))
	(defn minithumb-br-place [shape]
		(->> shape
				(rotate (deg2rad   6) [1 0 0])
				(rotate (deg2rad -34) [0 1 0])
				(rotate (deg2rad  35) [0 0 1])
				(translate thumborigin)
				(translate [-39 -43 -16])))
	(defn minithumb-bl-place [shape]
		(->> shape
				(rotate (deg2rad   6) [1 0 0])
				(rotate (deg2rad -32) [0 1 0])
				(rotate (deg2rad  35) [0 0 1])
				(translate thumborigin)
				(translate [-51 -25 -11.5]))) ;        (translate [-51 -25 -12])))

	(defn minithumb-1x-layout [shape]
		(union
		(minithumb-mr-place shape)
		(minithumb-br-place shape)
		(minithumb-tl-place shape)
		(minithumb-bl-place shape)))

	(defn minithumb-15x-layout [shape]
		(union
		(minithumb-tr-place shape)))

	(def minithumbcaps
		(union
		(minithumb-1x-layout (sa-cap 1))
		(minithumb-15x-layout (rotate (/ π 2) [0 0 1] (sa-cap 1)))))

	(def minithumbcaps-fill
		(union
		(minithumb-1x-layout keyhole-fill)
		(minithumb-15x-layout (rotate (/ π 2) [0 0 1] keyhole-fill))))

	(def minithumb
		(union
		(minithumb-1x-layout single-plate)
		(minithumb-15x-layout single-plate)))

	(def minithumb-left
		(union
		(minithumb-1x-layout (mirror [1 0 0] single-plate))
		(minithumb-15x-layout (mirror [1 0 0] single-plate))))


	(def minithumb-post-tr (translate [(- (/ mount-width 2) post-adj)  (- (/ mount-height  2) post-adj) 0] web-post))
	(def minithumb-post-tl (translate [(+ (/ mount-width -2) post-adj) (- (/ mount-height  2) post-adj) 0] web-post))
	(def minithumb-post-bl (translate [(+ (/ mount-width -2) post-adj) (+ (/ mount-height -2) post-adj) 0] web-post))
	(def minithumb-post-br (translate [(- (/ mount-width 2) post-adj)  (+ (/ mount-height -2) post-adj) 0] web-post))

	; thumb cluster naming:
	; bl tl tr
	; br mr

	(def minithumb-connectors
		(union
		(triangle-hulls		; between tr and tl
			(minithumb-tl-place web-post-tr)
			(minithumb-tl-place web-post-br)
			(minithumb-tr-place web-post-tl)
			(minithumb-tr-place web-post-bl))
		(triangle-hulls		; between mr and br
			(minithumb-br-place web-post-tr)
			(minithumb-br-place web-post-br)
			(minithumb-mr-place web-post-tl)
			(minithumb-mr-place web-post-bl))
		(triangle-hulls		; between mr and tr
			(minithumb-mr-place web-post-tr)
			(minithumb-mr-place web-post-br)
			(minithumb-tr-place web-post-br))
		(triangle-hulls		; between tl and bl
			(minithumb-tl-place web-post-tl)
			(minithumb-bl-place web-post-tr)
			(minithumb-tl-place web-post-bl)
			(minithumb-bl-place web-post-br))
		(triangle-hulls		; between all in the middle
			(minithumb-br-place web-post-tl)
			(minithumb-bl-place web-post-bl)
			(minithumb-br-place web-post-tr)
			(minithumb-bl-place web-post-br)
			(minithumb-mr-place web-post-tl)
			(minithumb-tl-place web-post-bl)
			(minithumb-mr-place web-post-tr)
			(minithumb-tl-place web-post-br)
			(minithumb-tr-place web-post-bl)
			(minithumb-mr-place web-post-tr)
			(minithumb-tr-place web-post-br))
		; above tr to the other keys, from left
		(triangle-hulls
			(minithumb-tl-place web-post-tr)
			(key-place (+ innercol-offset 0) cornerrow web-post-bl)
			(minithumb-tr-place web-post-tl)
			(key-place (+ innercol-offset 0) cornerrow web-post-br)
		)
		(triangle-hulls	
			(minithumb-tr-place web-post-tl)
			(key-place (+ innercol-offset 0) cornerrow web-post-br)
			(key-place (+ innercol-offset 1) cornerrow web-post-bl)
		)
		(triangle-hulls
			(minithumb-tr-place web-post-tl)
			(key-place (+ innercol-offset 1) cornerrow web-post-bl)
			(minithumb-tr-place web-post-tr)
		)
		(triangle-hulls
			(minithumb-tr-place web-post-tr)
			(key-place (+ innercol-offset 1) cornerrow web-post-bl)
			(key-place (+ innercol-offset 1) cornerrow web-post-br)
		)

		(triangle-hulls		; to the right of tr
			(minithumb-tr-place web-post-br)
			(minithumb-tr-place web-post-tr)
			(key-place (+ innercol-offset 2) lastrow web-post-bl)
			(key-place (+ innercol-offset 2) lastrow web-post-tl)
		)

		(triangle-hulls		; fill
			(key-place (+ innercol-offset 1) cornerrow web-post-br)
			(key-place (+ innercol-offset 2) cornerrow web-post-bl)
			(key-place (+ innercol-offset 2) lastrow web-post-tl)
		)
		(triangle-hulls		; fill
			(key-place (+ innercol-offset 2) lastrow web-post-tl)
			(minithumb-tr-place web-post-tr)
			(key-place (+ innercol-offset 1) cornerrow web-post-br)
		)

		(triangle-hulls		; above 2 x lastrow
			(key-place (+ innercol-offset 2) cornerrow web-post-bl)
			(key-place (+ innercol-offset 2) lastrow web-post-tl)
			(key-place (+ innercol-offset 2) cornerrow web-post-br)
			(key-place (+ innercol-offset 2) lastrow web-post-tr)
			
		)
		(triangle-hulls		; below 2 x lastrow
			(minithumb-tr-place web-post-br)
			(key-place (+ innercol-offset 2) lastrow web-post-bl)
			(key-place (+ innercol-offset 3) lastrow web-post-bl)
			(key-place (+ innercol-offset 2) lastrow web-post-br)
		)

		(triangle-hulls		; to the right of 2 x lastrow
			(key-place (+ innercol-offset 2) lastrow web-post-tr)
			(key-place (+ innercol-offset 2) cornerrow web-post-br)
			(key-place (+ innercol-offset 3) lastrow web-post-tl)
			(key-place (+ innercol-offset 3) cornerrow web-post-bl)
		)

		(triangle-hulls		; to the right of 2 x lastrow
			(key-place (+ innercol-offset 2) lastrow web-post-tr)
			(key-place (+ innercol-offset 3) lastrow web-post-tl)
			(key-place (+ innercol-offset 2) lastrow web-post-br)
			(key-place (+ innercol-offset 3) lastrow web-post-bl)
		)

		(triangle-hulls		; above 3 x lastrow
			(key-place (+ innercol-offset 3) cornerrow web-post-bl)
			(key-place (+ innercol-offset 3) lastrow web-post-tl)
			(key-place (+ innercol-offset 3) cornerrow web-post-br)
			(key-place (+ innercol-offset 3) lastrow web-post-tr)
		)

		(triangle-hulls		; to the right of 3 x lastrow
			(key-place (+ innercol-offset 3) cornerrow web-post-br)
			(key-place (+ innercol-offset 4) cornerrow web-post-bl)
			(key-place (+ innercol-offset 3) lastrow web-post-tr)
			(key-place (+ innercol-offset 3) lastrow web-post-br)
		)

		(if extra-row
			(union
				(triangle-hulls
				(key-place (+ innercol-offset 4) cornerrow web-post-bl)
				(key-place (+ innercol-offset 4) lastrow web-post-tl)
				(key-place (+ innercol-offset 3) lastrow web-post-br)
				(key-place (+ innercol-offset 4) lastrow web-post-bl)
				)
			)
		)

		)
	)

;;;;;;;;;;;;;;;;
;;; cf Thumb ;;;
;;;;;;;;;;;;;;;;

	(defn cfthumb-tl-place [shape]
		(->> shape
				(rotate (deg2rad  10) [1 0 0])
				(rotate (deg2rad -24) [0 1 0])
				(rotate (deg2rad  10) [0 0 1])
				(translate thumborigin)
				(translate [-13 -9.8 4])))
	(defn cfthumb-tr-place [shape]
		(->> shape
				(rotate (deg2rad  6) [1 0 0])
				(rotate (deg2rad -24) [0 1 0])
				(rotate (deg2rad  10) [0 0 1])
				(translate thumborigin)
				(translate [-7.5 -29.5 0])))
	(defn cfthumb-ml-place [shape]
		(->> shape
				(rotate (deg2rad  8) [1 0 0])
				(rotate (deg2rad -31) [0 1 0])
				(rotate (deg2rad  14) [0 0 1])
				(translate thumborigin)
				(translate [-30.5 -17 -6])))
	(defn cfthumb-mr-place [shape]
		(->> shape
				(rotate (deg2rad  4) [1 0 0])
				(rotate (deg2rad -31) [0 1 0])
				(rotate (deg2rad  14) [0 0 1])
				(translate thumborigin)
				(translate [-22.2 -41 -10.3])))
	(defn cfthumb-br-place [shape]
		(->> shape
				(rotate (deg2rad   2) [1 0 0])
				(rotate (deg2rad -37) [0 1 0])
				(rotate (deg2rad  18) [0 0 1])
				(translate thumborigin)
				(translate [-37 -46.4 -22])))
	(defn cfthumb-bl-place [shape]
		(->> shape
				(rotate (deg2rad   6) [1 0 0])
				(rotate (deg2rad -37) [0 1 0])
				(rotate (deg2rad  18) [0 0 1])
				(translate thumborigin)
				(translate [-47 -23 -19])))

	(defn cfthumb-1x-layout [shape]
		(union
		(cfthumb-tr-place (rotate (/ π 2) [0 0 0] shape))
		(cfthumb-mr-place shape)
		(cfthumb-br-place shape)
		(cfthumb-tl-place (rotate (/ π 2) [0 0 0] shape))))

	(defn cfthumb-15x-layout [shape]
		(union
		(cfthumb-bl-place shape)
		(cfthumb-ml-place shape)))

	(def cfthumbcaps
		(union
		(cfthumb-1x-layout (sa-cap 1))
		(cfthumb-15x-layout (rotate (/ π 2) [0 0 1] (sa-cap 1.5)))))

	(def cfthumbcaps-fill
		(union
		(cfthumb-1x-layout keyhole-fill)
		(cfthumb-15x-layout (rotate (/ π 2) [0 0 1] keyhole-fill))))

	(def cfthumb
		(union
		(cfthumb-1x-layout single-plate)
		(cfthumb-15x-layout larger-plate-half)
		(cfthumb-15x-layout single-plate)))

	(def cfthumb-left
		(union
		(cfthumb-1x-layout (mirror [1 0 0] single-plate))
		(cfthumb-15x-layout larger-plate-half)
		(cfthumb-15x-layout (mirror [1 0 0] single-plate))))


	(def cfthumb-connectors
		(union
		(triangle-hulls    ; top two
			(cfthumb-tl-place web-post-tl)
			(cfthumb-tl-place web-post-bl)
			(cfthumb-ml-place thumb-post-tr)
			(cfthumb-ml-place web-post-br))
		(triangle-hulls
			(cfthumb-ml-place thumb-post-tl)
			(cfthumb-ml-place web-post-bl)
			(cfthumb-bl-place thumb-post-tr)
			(cfthumb-bl-place web-post-br))
		(triangle-hulls    ; bottom two
			(cfthumb-br-place web-post-tr)
			(cfthumb-br-place web-post-br)
			(cfthumb-mr-place web-post-tl)
			(cfthumb-mr-place web-post-bl))
		(triangle-hulls
			(cfthumb-mr-place web-post-tr)
			(cfthumb-mr-place web-post-br)
			(cfthumb-tr-place web-post-tl)
			(cfthumb-tr-place web-post-bl))
		(triangle-hulls
			(cfthumb-tr-place web-post-br)
			(cfthumb-tr-place web-post-bl)
			(cfthumb-mr-place web-post-br))
		(triangle-hulls    ; between top row and bottom row
			(cfthumb-br-place web-post-tl)
			(cfthumb-bl-place web-post-bl)
			(cfthumb-br-place web-post-tr)
			(cfthumb-bl-place web-post-br)
			(cfthumb-mr-place web-post-tl)
			(cfthumb-ml-place web-post-bl)
			(cfthumb-mr-place web-post-tr)
			(cfthumb-ml-place web-post-br)
			(cfthumb-tr-place web-post-tl)
			(cfthumb-tl-place web-post-bl)
			(cfthumb-tr-place web-post-tr)
			(cfthumb-tl-place web-post-br))
		(triangle-hulls    ; top two to the main keyboard, starting on the left
			(cfthumb-ml-place thumb-post-tl)
			(key-place (+ innercol-offset 0) cornerrow web-post-bl)
			(cfthumb-ml-place thumb-post-tr)
			(key-place (+ innercol-offset 0) cornerrow web-post-br)
			(cfthumb-tl-place web-post-tl)
			(key-place (+ innercol-offset 1) cornerrow web-post-bl)
			(cfthumb-tl-place web-post-tr)
			(key-place (+ innercol-offset 1) cornerrow web-post-br)
			(key-place (+ innercol-offset 2) lastrow web-post-tl)
			(key-place (+ innercol-offset 2) lastrow web-post-bl)
			(cfthumb-tl-place web-post-tr)
			(key-place (+ innercol-offset 2) lastrow web-post-bl)
			(cfthumb-tl-place web-post-br)
			(key-place (+ innercol-offset 2) lastrow web-post-br)
			(key-place (+ innercol-offset 3) lastrow web-post-bl)
			(cfthumb-tl-place web-post-br)
			(cfthumb-tr-place web-post-tr))
		(triangle-hulls
			(key-place (+ innercol-offset 3) lastrow web-post-tr)
			(key-place (+ innercol-offset 3) cornerrow web-post-br)
			(key-place (+ innercol-offset 3) lastrow web-post-tl)
			(key-place (+ innercol-offset 3) cornerrow web-post-bl))
		(triangle-hulls
			(key-place (+ innercol-offset 2) lastrow web-post-tr)
			(key-place (+ innercol-offset 2) lastrow web-post-br)
			(key-place (+ innercol-offset 3) lastrow web-post-tl)
			(key-place (+ innercol-offset 3) lastrow web-post-bl))
		(triangle-hulls
			(cfthumb-tr-place web-post-br)
			(cfthumb-tr-place web-post-tr)
			(key-place (+ innercol-offset 3) lastrow web-post-bl))
		(triangle-hulls
			(key-place (+ innercol-offset 1) cornerrow web-post-br)
			(key-place (+ innercol-offset 2) lastrow web-post-tl)
			(key-place (+ innercol-offset 2) cornerrow web-post-bl)
			(key-place (+ innercol-offset 2) lastrow web-post-tr)
			(key-place (+ innercol-offset 2) cornerrow web-post-br)
			(key-place (+ innercol-offset 3) lastrow web-post-tl)
			(key-place (+ innercol-offset 3) cornerrow web-post-bl))
		(if extra-row
			(union
				(triangle-hulls
				(key-place (+ innercol-offset 3) lastrow web-post-tr)
				(key-place (+ innercol-offset 3) lastrow web-post-br)
				(key-place (+ innercol-offset 4) lastrow web-post-tl)
				(key-place (+ innercol-offset 4) lastrow web-post-bl))
				(triangle-hulls
				(key-place (+ innercol-offset 3) lastrow web-post-tr)
				(key-place (+ innercol-offset 3) cornerrow web-post-br)
				(key-place (+ innercol-offset 4) lastrow web-post-tl)
				(key-place (+ innercol-offset 4) cornerrow web-post-bl)))
			(union
				(triangle-hulls
				(key-place (+ innercol-offset 3) lastrow web-post-tr)
				(key-place (+ innercol-offset 3) lastrow web-post-br)
				(key-place (+ innercol-offset 4) cornerrow web-post-bl))
				(triangle-hulls
				(key-place (+ innercol-offset 3) lastrow web-post-tr)
				(key-place (+ innercol-offset 3) cornerrow web-post-br)
				(key-place (+ innercol-offset 4) cornerrow web-post-bl))))))

;;;;;;;;;;;;;;;;;;;;;;
;;; Tightly thumb ;;;;
;;;;;;;;;;;;;;;;;;;;;;

	(def tightly-thumborigin
		(map + (key-position 1 cornerrow [(/ mount-width 2) (- (/ mount-height 2)) 0])
				thumb-offsets))

	(defn tightly-thumb-place [rot move shape]
		(->> shape
				(rotate (deg2rad (nth rot 0)) [1 0 0])
				(rotate (deg2rad (nth rot 1)) [0 1 0])
				(rotate (deg2rad (nth rot 2)) [0 0 1])               ; original 10
				(translate tightly-thumborigin)
				(translate move)))

	; convexer
	(defn thumb-r-place [shape] (tightly-thumb-place [14 -40 10] [-15 -10 5] shape)) ; right
	(defn thumb-m-place [shape] (tightly-thumb-place [10 -23 20] [-35.5 -16 -7] shape)) ; middle
	(defn thumb-l-place [shape] (tightly-thumb-place [6 -5 35] [-57.5 -27.5 -13] shape)) ; left

	(defn tightly-thumb-layout [shape]
		(union
		(thumb-r-place shape)
		(thumb-m-place shape)
		(thumb-l-place shape)))

	(defn debug [shape]
		(color [0.5 0.5 0.5 0.5] shape))

	(def tightly-thumbcaps (tightly-thumb-layout (sa-cap 1)))
	(def tightly (tightly-thumb-layout single-plate))
	(def tightly-left (tightly-thumb-layout (mirror [1 0 0] single-plate)))
	;(def tightly-thumb-fill (tightly-thumb-layout filled-plate))
	;(def thumb-space-below (tightly-thumb-layout switch-bottom))

	(def tightly-thumb-connectors
  (union
   (triangle-hulls   ; top two
    (thumb-m-place web-post-tr)
    (thumb-m-place web-post-br)
    (thumb-r-place web-post-tl)
    (thumb-r-place web-post-bl))
   (triangle-hulls   ; top two
    (thumb-m-place web-post-tl)
    (thumb-l-place web-post-tr)
    (thumb-m-place web-post-bl)
    (thumb-l-place web-post-br))
   (triangle-hulls   ; top two to the main keyboard, starting on the left
    (key-place 2 lastrow web-post-br)
    (key-place 3 lastrow web-post-bl)
    (key-place 2 lastrow web-post-tr)
    (key-place 3 lastrow web-post-tl)
    (key-place 3 cornerrow web-post-bl)
    (key-place 3 lastrow web-post-tr)
    (key-place 3 cornerrow web-post-br)
    (key-place 4 cornerrow web-post-bl))
   (triangle-hulls   ; good
    (key-place 1 cornerrow web-post-br)
    (key-place 2 lastrow web-post-tl)
    (key-place 2 cornerrow web-post-bl)
    (key-place 2 lastrow web-post-tr)
    (key-place 2 cornerrow web-post-br)
    (key-place 3 cornerrow web-post-bl))
   (triangle-hulls
    (key-place 3 lastrow web-post-tr)
    (key-place 3 lastrow web-post-br)
    (key-place 3 lastrow web-post-tr)
    (key-place 4 cornerrow web-post-bl))
    ;above thumb-r the biggest left triangle
   (triangle-hulls
    (key-place 0 cornerrow web-post-br)
    (key-place 0 cornerrow web-post-bl)
    (thumb-r-place web-post-tl))
    ;above thumb-r, the middle small triangle
   (triangle-hulls
    (key-place 0 cornerrow web-post-br)
    (key-place 1 cornerrow web-post-bl)
    (thumb-r-place web-post-tl))
    ;above thumb-r, the right triangle
   (triangle-hulls
    (thumb-r-place web-post-tr)
    (thumb-r-place web-post-tl)
    (key-place 1 cornerrow web-post-bl))
    ;thumb-r connected to cornerrow 1
   (triangle-hulls
    (key-place 1 cornerrow web-post-br)
    (key-place 1 cornerrow web-post-bl)
    (thumb-r-place web-post-tr))
    ;to the right of thumb-r connected with 1 point to thumb-r
   (triangle-hulls
    (key-place 1 cornerrow web-post-br)
    (key-place 2 lastrow web-post-bl)
    (thumb-r-place web-post-tr))
    ;to the right of thumb-r connected with 2 points to thumb-r
   (triangle-hulls
    (key-place 2 lastrow web-post-bl)
    (thumb-r-place web-post-br)
    (thumb-r-place web-post-tr))
    ; between lastrow and lastrow-1
   (triangle-hulls
    (key-place 2 lastrow web-post-bl)
    (key-place 2 lastrow web-post-tl)
    (key-place 1 cornerrow web-post-br))
    ; between top-r and below lastrow
   (triangle-hulls
    (thumb-r-place web-post-br)
    (key-place 2 lastrow web-post-bl)
    (key-place 3 lastrow web-post-bl)
    (key-place 2 lastrow web-post-br))
		
		(if extra-row
			(union
				(triangle-hulls
				(key-place (+ innercol-offset 4) cornerrow web-post-bl)
				(key-place (+ innercol-offset 4) lastrow web-post-tl)
				(key-place (+ innercol-offset 3) lastrow web-post-br)
				(key-place (+ innercol-offset 4) lastrow web-post-bl)
				)
			)
		)
		))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Defining picked thumb ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

	(when (= thumb-style "default")
		(def thumb-type thumb)
		(def thumb-type-left thumb-left)
		(def thumb-connector-type thumb-connectors)
		(def thumbcaps-type thumbcaps)
		(def thumbcaps-fill-type thumbcaps-fill))

	(when (= thumb-style "cf")
		(def thumb-type cfthumb)
		(def thumb-type-left cfthumb-left)
		(def thumb-connector-type cfthumb-connectors)
		(def thumbcaps-type cfthumbcaps)
		(def thumbcaps-fill-type cfthumbcaps-fill))

	(when (= thumb-style "mini")
		(def thumb-type minithumb)
		(def thumb-type-left minithumb-left)
		(def thumb-connector-type minithumb-connectors)
		(def thumbcaps-type minithumbcaps)
		(def thumbcaps-fill-type minithumbcaps-fill))

	(when (= thumb-style "tightly")
		(def thumb-type tightly)
		(def thumb-type-left tightly-left)
		(def thumb-connector-type tightly-thumb-connectors)
		(def thumbcaps-type tightly-thumbcaps)
		;(def thumbcaps-fill-type tightly-thumb-fill))
		)

;;;;;;;;;;;;
;;; Case ;;;
;;;;;;;;;;;;

	(defn bottom [height p]
		(->> (project p)
				(extrude-linear {:height height :twist 0 :convexity 0})
				(translate [0 0 (- (/ height 2) 10)])))

	(defn bottom-hull [& p]
		(hull p (bottom 0.001 p)))

	(def left-wall-x-offset 4)
	(def left-wall-z-offset 1)

	(defn left-key-position [row direction]
		(map - (key-position 0 row [(* mount-width -0.5) (* direction mount-height 0.5) 0]) [left-wall-x-offset 0 left-wall-z-offset]))

	(defn left-key-place [row direction shape]
		(translate (left-key-position row direction) shape))

	(defn wall-locate1 [dx dy] [(* dx wall-thickness) (* dy wall-thickness) -1])
	(defn wall-locate2 [dx dy] [(* dx wall-xy-offset) (* dy wall-xy-offset) wall-z-offset])
	(defn wall-locate3 [dx dy] [(* dx (+ wall-xy-offset wall-thickness)) (* dy (+ wall-xy-offset wall-thickness)) wall-z-offset])

	(defn wall-brace [place1 dx1 dy1 post1 place2 dx2 dy2 post2]
		(union
		(hull
			(place1 post1)
			(place1 (translate (wall-locate1 dx1 dy1) post1))
			(place1 (translate (wall-locate2 dx1 dy1) post1))
			(place1 (translate (wall-locate3 dx1 dy1) post1))
			(place2 post2)
			(place2 (translate (wall-locate1 dx2 dy2) post2))
			(place2 (translate (wall-locate2 dx2 dy2) post2))
			(place2 (translate (wall-locate3 dx2 dy2) post2)))
		(bottom-hull
			(place1 (translate (wall-locate2 dx1 dy1) post1))
			(place1 (translate (wall-locate3 dx1 dy1) post1))
			(place2 (translate (wall-locate2 dx2 dy2) post2))
			(place2 (translate (wall-locate3 dx2 dy2) post2)))))

	(defn key-wall-brace [x1 y1 dx1 dy1 post1 x2 y2 dx2 dy2 post2]
		(wall-brace (partial key-place x1 y1) dx1 dy1 post1
								(partial key-place x2 y2) dx2 dy2 post2))

	; right wall extended; to be fixed
	(def right-wall
		(if pinky-15u
			(union
			; corner between the right wall and back wall
			(if (> first-15u-row 0)
				(key-wall-brace lastcol 0 0 1 web-post-tr lastcol 0 1 0 web-post-tr)
				(union (key-wall-brace lastcol 0 0 1 web-post-tr lastcol 0 0 1 wide-post-tr)
								(key-wall-brace lastcol 0 0 1 wide-post-tr lastcol 0 1 0 wide-post-tr)))
			; corner between the right wall and front wall
			(if (= last-15u-row extra-cornerrow)
				(union (key-wall-brace lastcol extra-cornerrow 0 -1 web-post-br lastcol extra-cornerrow 0 -1 wide-post-br)
								(key-wall-brace lastcol extra-cornerrow 0 -1 wide-post-br lastcol extra-cornerrow 1 0 wide-post-br))
				(key-wall-brace lastcol extra-cornerrow 0 -1 web-post-br lastcol extra-cornerrow 1 0 web-post-br))

			(if (>= first-15u-row 2)
				(for [y (range 0 (dec first-15u-row))]
					(union (key-wall-brace lastcol y 1 0 web-post-tr lastcol y 1 0 web-post-br)
									(key-wall-brace lastcol y 1 0 web-post-br lastcol (inc y) 1 0 web-post-tr))))

			(if (>= first-15u-row 1)
				(for [y (range (dec first-15u-row) first-15u-row)] (key-wall-brace lastcol y 1 0 web-post-tr lastcol (inc y) 1 0 wide-post-tr)))

			(for [y (range first-15u-row (inc last-15u-row))] (key-wall-brace lastcol y 1 0 wide-post-tr lastcol y 1 0 wide-post-br))
			(for [y (range first-15u-row last-15u-row)] (key-wall-brace lastcol (inc y) 1 0 wide-post-tr lastcol y 1 0 wide-post-br))

			(if (<= last-15u-row (- extra-cornerrow 1))
				(for [y (range last-15u-row (inc last-15u-row))] (key-wall-brace lastcol y 1 0 wide-post-br lastcol (inc y) 1 0 web-post-br)))

			(if (<= last-15u-row (- extra-cornerrow 2))
				(for [y (range (inc last-15u-row) extra-cornerrow)]
					(union (key-wall-brace lastcol y 1 0 web-post-br lastcol (inc y) 1 0 web-post-tr)
									(key-wall-brace lastcol (inc y) 1 0 web-post-tr lastcol (inc y) 1 0 web-post-br)))))
			(union (key-wall-brace lastcol 0 0 1 web-post-tr lastcol 0 1 0 web-post-tr)
						(if extra-row
							(union (for [y (range 0 (inc lastrow))] (key-wall-brace lastcol y 1 0 web-post-tr lastcol y 1 0 web-post-br))
											(for [y (range 1 (inc lastrow))] (key-wall-brace lastcol (dec y) 1 0 web-post-br lastcol y 1 0 web-post-tr)))
							(union (for [y (range 0 lastrow)] (key-wall-brace lastcol y 1 0 web-post-tr lastcol y 1 0 web-post-br))
											(for [y (range 1 lastrow)] (key-wall-brace lastcol (dec y) 1 0 web-post-br lastcol y 1 0 web-post-tr))))
						(key-wall-brace lastcol extra-cornerrow 0 -1 web-post-br lastcol extra-cornerrow 1 0 web-post-br))))

	(def cf-thumb-wall
		(union
		; thumb walls
		(wall-brace cfthumb-mr-place  0 -1 web-post-br cfthumb-tr-place  0 -1 web-post-br)
		(wall-brace cfthumb-mr-place  0 -1 web-post-br cfthumb-mr-place  0 -1.15 web-post-bl)
		(wall-brace cfthumb-br-place  0 -1 web-post-br cfthumb-br-place  0 -1 web-post-bl)
		(wall-brace cfthumb-bl-place -0.3  1 thumb-post-tr cfthumb-bl-place  0  1 thumb-post-tl)
		(wall-brace cfthumb-br-place -1  0 web-post-tl cfthumb-br-place -1  0 web-post-bl)
		(wall-brace cfthumb-bl-place -1  0 thumb-post-tl cfthumb-bl-place -1  0 web-post-bl)
		; cfthumb corners
		(wall-brace cfthumb-br-place -1  0 web-post-bl cfthumb-br-place  0 -1 web-post-bl)
		(wall-brace cfthumb-bl-place -1  0 thumb-post-tl cfthumb-bl-place  0  1 thumb-post-tl)
		; cfthumb tweeners
		(wall-brace cfthumb-mr-place  0 -1.15 web-post-bl cfthumb-br-place  0 -1 web-post-br)
		(wall-brace cfthumb-bl-place -1  0 web-post-bl cfthumb-br-place -1  0 web-post-tl)
		(wall-brace cfthumb-tr-place  0 -1 web-post-br (partial key-place (+ innercol-offset 3) lastrow)  0 -1 web-post-bl)
		; clunky bit on the top left cfthumb connection  (normal connectors don't work well)
		(bottom-hull
			(left-key-place (- cornerrow innercol-offset) -1 (translate (wall-locate2 -1 0) web-post))
			(left-key-place (- cornerrow innercol-offset) -1 (translate (wall-locate3 -1 0) web-post))
			(cfthumb-bl-place (translate (wall-locate2 -0.3 1) thumb-post-tr))
			(cfthumb-bl-place (translate (wall-locate3 -0.3 1) thumb-post-tr)))
		(hull
			(left-key-place (- cornerrow innercol-offset) -1 (translate (wall-locate2 -1 0) web-post))
			(left-key-place (- cornerrow innercol-offset) -1 (translate (wall-locate3 -1 0) web-post))
			(cfthumb-bl-place (translate (wall-locate2 -0.3 1) thumb-post-tr))
			(cfthumb-bl-place (translate (wall-locate3 -0.3 1) thumb-post-tr))
			(cfthumb-ml-place thumb-post-tl))
		(hull
			(left-key-place (- cornerrow innercol-offset) -1 web-post)
			(left-key-place (- cornerrow innercol-offset) -1 (translate (wall-locate1 -1 0) web-post))
			(left-key-place (- cornerrow innercol-offset) -1 (translate (wall-locate2 -1 0) web-post))
			(left-key-place (- cornerrow innercol-offset) -1 (translate (wall-locate3 -1 0) web-post))
			(cfthumb-ml-place thumb-post-tl))
		(hull
			(left-key-place (- cornerrow innercol-offset) -1 web-post)
			(left-key-place (- cornerrow innercol-offset) -1 (translate (wall-locate1 -1 0) web-post))
			(key-place 0 (- cornerrow innercol-offset) web-post-bl)
			(cfthumb-ml-place thumb-post-tl))
		(hull
			(cfthumb-bl-place thumb-post-tr)
			(cfthumb-bl-place (translate (wall-locate1 -0.3 1) thumb-post-tr))
			(cfthumb-bl-place (translate (wall-locate2 -0.3 1) thumb-post-tr))
			(cfthumb-bl-place (translate (wall-locate3 -0.3 1) thumb-post-tr))
			(cfthumb-ml-place thumb-post-tl))
		; connectors below the inner column to the thumb & second column
		(if inner-column
			(union
				(hull
				(key-place 0 (dec cornerrow) web-post-bl)
				(key-place 0 (dec cornerrow) web-post-br)
				(key-place 0 cornerrow web-post-tr))
				(hull
				(key-place 0 cornerrow web-post-tr)
				(key-place 1 cornerrow web-post-tl)
				(key-place 1 cornerrow web-post-bl))
				(hull
				(key-place 0 (dec cornerrow) web-post-bl)
				(key-place 0 cornerrow web-post-tr)
				(key-place 1 cornerrow web-post-bl))
				(hull
				(key-place 0 (dec cornerrow) web-post-bl)
				(key-place 1 cornerrow web-post-bl)
				(cfthumb-ml-place thumb-post-tl))))))

	(def mini-thumb-wall
		(union
		; this wall is from tr and mr to the left
		(wall-brace minithumb-mr-place 0 -1 web-post-br minithumb-tr-place  0 -2 web-post-br)
		; this wall is from tr and to the right
		(wall-brace minithumb-tr-place  0 -2 web-post-br (partial key-place (+ innercol-offset 3) lastrow)  0 -1 web-post-bl)
		(wall-brace minithumb-mr-place 0 -1 web-post-br minithumb-mr-place  0 -1 web-post-bl)
		(wall-brace minithumb-br-place -1 -1 web-post-br minithumb-br-place  0 -1 web-post-bl)
		(wall-brace minithumb-br-place -1 -1 web-post-br minithumb-mr-place 0 -1 web-post-bl)
		(wall-brace minithumb-br-place -1  0 web-post-tl minithumb-br-place -1  0 web-post-bl)
		(wall-brace minithumb-bl-place -1  0 web-post-tl minithumb-bl-place -1  0 web-post-bl)
		; minithumb corners
		(wall-brace minithumb-br-place -1  0 web-post-bl minithumb-br-place  0 -1 web-post-bl)
		(wall-brace minithumb-bl-place -1  0 web-post-tl minithumb-bl-place  0  1 web-post-tl)
		; between br and bl
		(wall-brace minithumb-bl-place -1  0 web-post-bl minithumb-br-place -1  0 web-post-tl)
		; wall above bl
		(wall-brace minithumb-bl-place  -0.3  1 web-post-tr minithumb-bl-place  0  1 web-post-tl)
		(bottom-hull		; between the wall above bl and the rest of the case 
			(left-key-place (- cornerrow innercol-offset) -1 (translate (wall-locate2 -1 0) web-post))
			(left-key-place (- cornerrow innercol-offset) -1 (translate (wall-locate3 -1 0) web-post))
			(minithumb-bl-place (translate (wall-locate2 -0.3 1) web-post-tr))
			(minithumb-bl-place (translate (wall-locate3 -0.3 1) web-post-tr)))
		(hull		; above inbetween bl-tl to the wall
			(left-key-place (- cornerrow innercol-offset) -1 (translate (wall-locate2 -1 0) web-post))
			(left-key-place (- cornerrow innercol-offset) -1 (translate (wall-locate3 -1 0) web-post))
			(minithumb-bl-place (translate (wall-locate2 -0.3 1) web-post-tr))
			(minithumb-bl-place (translate (wall-locate3 -0.3 1) web-post-tr))
			(minithumb-bl-place (translate (wall-locate1 -0.3 1) web-post-tr))
			(minithumb-bl-place web-post-tr)
			(minithumb-tl-place web-post-tl)
			(left-key-place (- cornerrow innercol-offset) -1 web-post)
			(left-key-place (- cornerrow innercol-offset) -1 (translate (wall-locate1 -1 0) web-post))
			)
		; above tl to the wall
		(hull
			(left-key-place (- cornerrow innercol-offset) -1 web-post)
			(left-key-place (- cornerrow innercol-offset) -1 (translate (wall-locate1 -1 0) web-post))
			(key-place 0 (- cornerrow innercol-offset) web-post-bl)
			(minithumb-tl-place web-post-tr)
			(minithumb-tl-place web-post-tl))
		; connectors below the inner column to the thumb & second column
		(if inner-column
			(union
				(hull
				(key-place 0 (dec cornerrow) web-post-bl)
				(key-place 0 (dec cornerrow) web-post-br)
				(key-place 0 cornerrow web-post-tr))
				(hull
				(key-place 0 cornerrow web-post-tr)
				(key-place 1 cornerrow web-post-tl)
				(key-place 1 cornerrow web-post-bl))
				(hull
				(key-place 0 (dec cornerrow) web-post-bl)
				(key-place 0 cornerrow web-post-tr)
				(key-place 1 cornerrow web-post-bl))
				(hull
				(key-place 0 (dec cornerrow) web-post-bl)
				(key-place 1 cornerrow web-post-bl)
				(minithumb-tl-place minithumb-post-tl))))
		)
	)

	(def default-thumb-wall
		(union
		; thumb walls
		(wall-brace thumb-mr-place  0 -1 web-post-br thumb-tr-place  0 -1 thumb-post-br)
		(wall-brace thumb-mr-place  0 -1 web-post-br thumb-mr-place  0 -1 web-post-bl)
		(wall-brace thumb-br-place  0 -1 web-post-br thumb-br-place  0 -1 web-post-bl)
		(wall-brace thumb-ml-place -0.3  1 web-post-tr thumb-ml-place  0  1 web-post-tl)
		(wall-brace thumb-bl-place  0  1 web-post-tr thumb-bl-place  0  1 web-post-tl)
		(wall-brace thumb-br-place -1  0 web-post-tl thumb-br-place -1  0 web-post-bl)
		(wall-brace thumb-bl-place -1  0 web-post-tl thumb-bl-place -1  0 web-post-bl)
		; thumb corners
		(wall-brace thumb-br-place -1  0 web-post-bl thumb-br-place  0 -1 web-post-bl)
		(wall-brace thumb-bl-place -1  0 web-post-tl thumb-bl-place  0  1 web-post-tl)
		; thumb tweeners
		(wall-brace thumb-mr-place  0 -1 web-post-bl thumb-br-place  0 -1 web-post-br)
		(wall-brace thumb-ml-place  0  1 web-post-tl thumb-bl-place  0  1 web-post-tr)
		(wall-brace thumb-bl-place -1  0 web-post-bl thumb-br-place -1  0 web-post-tl)
		(wall-brace thumb-tr-place  0 -1 thumb-post-br (partial key-place (+ innercol-offset 3) lastrow)  0 -1 web-post-bl)
		; clunky bit on the top left thumb connection  (normal connectors don't work well)
		(bottom-hull
			(left-key-place (- cornerrow innercol-offset) -1 (translate (wall-locate2 -1 0) web-post))
			(left-key-place (- cornerrow innercol-offset) -1 (translate (wall-locate3 -1 0) web-post))
			(thumb-ml-place (translate (wall-locate2 -0.3 1) web-post-tr))
			(thumb-ml-place (translate (wall-locate3 -0.3 1) web-post-tr)))
		(hull
			(left-key-place (- cornerrow innercol-offset) -1 (translate (wall-locate2 -1 0) web-post))
			(left-key-place (- cornerrow innercol-offset) -1 (translate (wall-locate3 -1 0) web-post))
			(thumb-ml-place (translate (wall-locate2 -0.3 1) web-post-tr))
			(thumb-ml-place (translate (wall-locate3 -0.3 1) web-post-tr))
			(thumb-tl-place thumb-post-tl))
		(hull
			(left-key-place (- cornerrow innercol-offset) -1 web-post)
			(left-key-place (- cornerrow innercol-offset) -1 (translate (wall-locate1 -1 0) web-post))
			(left-key-place (- cornerrow innercol-offset) -1 (translate (wall-locate2 -1 0) web-post))
			(left-key-place (- cornerrow innercol-offset) -1 (translate (wall-locate3 -1 0) web-post))
			(thumb-tl-place thumb-post-tl))
		(hull
			(left-key-place (- cornerrow innercol-offset) -1 web-post)
			(left-key-place (- cornerrow innercol-offset) -1 (translate (wall-locate1 -1 0) web-post))
			(key-place 0 (- cornerrow innercol-offset) web-post-bl)
			(key-place 0 (- cornerrow innercol-offset) (translate (wall-locate1 0 0) web-post-bl))
			(thumb-tl-place thumb-post-tl))
		; connectors below the inner column to the thumb & second column
		(if inner-column
			(union
				(hull
				(key-place 0 (dec cornerrow) web-post-bl)
				(key-place 0 (dec cornerrow) web-post-br)
				(key-place 0 cornerrow web-post-tr))
				(hull
				(key-place 0 cornerrow web-post-tr)
				(key-place 1 cornerrow web-post-tl)
				(key-place 1 cornerrow web-post-bl))
				(hull
				(key-place 0 (dec cornerrow) web-post-bl)
				(key-place 0 cornerrow web-post-tr)
				(key-place 1 cornerrow web-post-bl))
				(hull
				(key-place 0 (dec cornerrow) web-post-bl)
				(key-place 1 cornerrow web-post-bl)
				(thumb-tl-place thumb-post-tl))))
		(hull
			(thumb-ml-place web-post-tr)
			(thumb-ml-place (translate (wall-locate1 -0.3 1) web-post-tr))
			(thumb-ml-place (translate (wall-locate2 -0.3 1) web-post-tr))
			(thumb-ml-place (translate (wall-locate3 -0.3 1) web-post-tr))
			(thumb-tl-place thumb-post-tl))))

	(def tightly-thumb-wall
		(union
		(wall-brace thumb-r-place 0 -1 web-post-br (partial key-place 3 lastrow) 0 -1 web-post-bl)
		(wall-brace thumb-r-place 0 -1 web-post-br thumb-r-place 0 -1 web-post-bl)
		(wall-brace thumb-m-place 0 -1 web-post-br thumb-m-place 0 -1 web-post-bl)
		(wall-brace thumb-l-place 0 -1 web-post-br thumb-l-place 0 -1 web-post-bl)
		(wall-brace thumb-l-place 0 1 web-post-tr thumb-l-place 0 1 web-post-tl)
		(wall-brace thumb-l-place -1 0 web-post-tl thumb-l-place -1 0 web-post-bl)
			; thumb corners
		(wall-brace thumb-l-place -1 0 web-post-bl thumb-l-place 0 -1 web-post-bl)
		(wall-brace thumb-l-place -1 0 web-post-tl thumb-l-place 0 1 web-post-tl)
			; thumb tweeners
		(wall-brace thumb-r-place 0 -1 web-post-bl thumb-m-place 0 -1 web-post-br)
		(wall-brace thumb-m-place 0 -1 web-post-bl thumb-l-place 0 -1 web-post-br)
			;(wall-brace thumb-m-place 0 1 web-post-tl thumb-l-place 0 1 web-post-tr)
		(wall-brace thumb-l-place -1 0 web-post-bl thumb-l-place -1 0 web-post-tl)

		(wall-brace (partial left-key-place cornerrow -1) -1 0 web-post thumb-l-place 0 1 web-post-tr)

		(hull
			(left-key-place cornerrow -1 web-post)
			(thumb-m-place web-post-tr)
			(thumb-m-place web-post-tl))
		(hull
			(left-key-place cornerrow -1 web-post)
			(key-place 0 cornerrow web-post-bl)
			(thumb-m-place web-post-tr)
			(thumb-r-place web-post-tl))
		(hull
			(thumb-m-place web-post-tl)
			(thumb-l-place web-post-tr)
			(left-key-place cornerrow -1 web-post))))

	;switching walls depending on thumb-style used
	(def thumb-wall-type
		(case thumb-style
			"default" default-thumb-wall
			"cf" cf-thumb-wall
			"mini" mini-thumb-wall
			"tightly" tightly-thumb-wall))

	(def case-walls
		(union
		thumb-wall-type
		right-wall
		; back wall
		(for [x (range 0 ncols)] (key-wall-brace x 0 0 1 web-post-tl x       0 0 1 web-post-tr))
		(for [x (range 1 ncols)] (key-wall-brace x 0 0 1 web-post-tl (dec x) 0 0 1 web-post-tr))
		; left wall
		(for [y (range 0 (- lastrow innercol-offset))] (union
																										(wall-brace (partial left-key-place y 1) -1 0 web-post (partial left-key-place y -1) -1 0 web-post)
																										(hull (key-place 0 y web-post-tl)
																													(key-place 0 y web-post-bl)
																													(left-key-place y  1 web-post)
																													(left-key-place y -1 web-post))))
		(for [y (range 1 (- lastrow innercol-offset))] (union
																										(wall-brace (partial left-key-place (dec y) -1) -1 0 web-post (partial left-key-place y  1) -1 0 web-post)
																										(hull (key-place 0 y       web-post-tl)
																													(key-place 0 (dec y) web-post-bl)
																													(left-key-place y        1 web-post)
																													(left-key-place (dec y) -1 web-post))))
		(wall-brace (partial key-place 0 0) 0 1 web-post-tl (partial left-key-place 0 1) -0.6 1 web-post)
		(wall-brace (partial left-key-place 0 1) -0.6 1 web-post (partial left-key-place 0 1) -1 0 web-post)
		; front wall
		(key-wall-brace (+ innercol-offset 3) lastrow  0 -1 web-post-bl (+ innercol-offset 3) lastrow   0 -1 web-post-br)
		(key-wall-brace (+ innercol-offset 3) lastrow  0 -1 web-post-br (+ innercol-offset 4) extra-cornerrow 0 -1 web-post-bl)
		(for [x (range (+ innercol-offset 4) ncols)] (key-wall-brace x extra-cornerrow 0 -1 web-post-bl x       extra-cornerrow 0 -1 web-post-br))
		(for [x (range (+ innercol-offset 5) ncols)] (key-wall-brace x extra-cornerrow 0 -1 web-post-bl (dec x) extra-cornerrow 0 -1 web-post-br))))

	; Offsets for the controller/trrs holder cutout
	(def holder-offset
		(case nrows
			4 -3.5
			5 0
			6 (if inner-column
					3.2
					1)))

	; offsets so it is 1mm from the outside wall
	(def notch-offset
		(case nrows
			4 3.30
			5 0.53
			6 -3.98))

	; Cutout for controller/trrs jack holder
	(def usb-holder-ref (key-position 0 0 (map - (wall-locate2  0  -1) [0 (/ mount-height 2) 0])))
	(def usb-holder-position (map + [(+ 18.8 holder-offset) 18.7 1.3] [(first usb-holder-ref) (second usb-holder-ref) 2]))
	(def usb-holder-space  (translate (map + usb-holder-position [-1.5 (* -1 wall-thickness) 4.4]) (cube 28.666 30 15.4))) ;28.666
	(def usb-holder-notch  (translate (map + usb-holder-position [-1.5 (+ 4.5 notch-offset) 4.4]) (cube 31.366 1.3 15.4))) ;31.366
	(def trrs-notch        (translate (map + usb-holder-position [-10.33 (+ 3.6 notch-offset) 6.6]) (cube 8.4 2.4 19.8)))

	; Screw insert definition & position
		(defn screw-insert-shape [bottom-radius top-radius height]
			(union
			(->> (binding [*fn* 30]
							(cylinder [bottom-radius top-radius] height)))))

		(defn screw-insert [column row bottom-radius top-radius height offset]
			(let [shift-right   (= column lastcol)
						shift-left    (= column 0)
						shift-up      (and (not (or shift-right shift-left)) (= row 0))
						shift-down    (and (not (or shift-right shift-left)) (>= row lastrow))
						position      (if shift-up     (key-position column row (map + (wall-locate2  0  1) [0 (/ mount-height 2) 0]))
															(if shift-down  (key-position column row (map - (wall-locate2  0 -2.5) [0 (/ mount-height 2) 0]))
																	(if shift-left (map + (left-key-position row 0) (wall-locate3 -1 0))
																			(key-position column row (map + (wall-locate2  1  0) [(/ mount-width 2) 0 0])))))]
				(->> (screw-insert-shape bottom-radius top-radius height)
						(translate (map + offset [(first position) (second position) (/ height 2)])))))

		; positioning for thumb cluster
		(when (= thumb-style "default")
			(def screw-offset-br-x -10.1)
			(def screw-offset-br-y -8.5)
			(def screw-offset-bm-x 25)
			(def screw-offset-bm-y 9.8))
		(when (= thumb-style "mini")
			(def screw-offset-br-x -1.7)
			(def screw-offset-br-y -23)
			(def screw-offset-bm-x 25)
			(def screw-offset-bm-y 11.8))
		(when (= thumb-style "tightly")
			(def screw-offset-br-x 5)
			(def screw-offset-br-y -22.5)
			(def screw-offset-bm-x 25)
			(def screw-offset-bm-y 12.5))

		; positioning for nrows
		(when (= nrows 4)
			(def screw-offset-tm-x -0)
			(def screw-offset-tm-y -4.4)
			(def screw-offset-tl-y 8.6)
			(def screw-offset-tr-x 4.2)
			(def screw-offset-tr-y -25))
		(when (= nrows 5)
			(def screw-offset-tm-x -0)
			(def screw-offset-tm-y -5.2)
			(def screw-offset-tl-y 6.3)
			(def screw-offset-tr-x 1)
			(def screw-offset-tr-y -25))
		(when (= nrows 6)
			(def screw-offset-tm-x -0)
			(def screw-offset-tm-y -6)
			(def screw-offset-tl-y 3.4)
			(def screw-offset-tr-x -3)
			(def screw-offset-tr-y -25))

		; positioning for ncols
		(when (= ncols 5)
			(def screw-offset-bl-x -7.2)
			(def screw-offset-bl-y 37)
			(def screw-offset-tl-x -16))
		(when (= ncols 6)
			(def screw-offset-bl-x -6.1)
			(def screw-offset-bl-y 37)
			(def screw-offset-tl-x -21))
		(when (= ncols 7)
			(def screw-offset-bl-x -5)
			(def screw-offset-bl-y 37)
			(def screw-offset-tl-x -23))

		; positioning for pinky 1.5u
		(if pinky-15u
			(def screw-offset-pinky-off 10.1)
			(def screw-offset-pinky-off 0)
			)

		; positining for extra row
		(if (and extra-row pinky-15u)
			(def screw-offset-row-off -5)
			(def screw-offset-row-off 0)
			)

		; the screw offsets are mirrored on the X axis for easier positioning
		(def screw-offset-tr [screw-offset-tl-x screw-offset-tl-y 0])
		(def screw-offset-tm [screw-offset-tm-x screw-offset-tm-y 0])
		(def screw-offset-tl [screw-offset-tr-x screw-offset-tr-y 0])
		(def screw-offset-br [(+ (+ screw-offset-bl-x screw-offset-pinky-off) screw-offset-row-off) screw-offset-bl-y 0])
		(def screw-offset-bm [screw-offset-bm-x screw-offset-bm-y 0])
		(def screw-offset-bl [screw-offset-br-x screw-offset-br-y 0])
		
		(defn screw-insert-all-shapes [bottom-radius top-radius height]
			(union
				(screw-insert 0 										0				bottom-radius top-radius height screw-offset-tl)
				(screw-insert 0 										lastrow	bottom-radius top-radius height screw-offset-bl)
				(screw-insert lastcol 							lastrow	bottom-radius top-radius height screw-offset-br)
				(screw-insert lastcol 							0				bottom-radius top-radius height screw-offset-tr)
				(screw-insert (+ 2 innercol-offset) 0				bottom-radius top-radius height screw-offset-tm)
				(screw-insert (+ 1 innercol-offset) lastrow	bottom-radius top-radius height screw-offset-bm)))

		; Hole Depth Y: 7
		(def screw-insert-height 7)

		(if (= print-type "fdm")
			(def insert-diameter 4.2)
			(def insert-diameter 4.8)
		)

		; Hole Diameter C: 4.1-4.4
		(def screw-insert-bottom-radius (/ insert-diameter 2)) ;fdm:4.2 ; msla:4.8 ; 7.2 for position seeking
		(def screw-insert-top-radius (/	insert-diameter 2)) ;fdm:4.2 ; msla:4.8
		(def screw-insert-holes (screw-insert-all-shapes screw-insert-bottom-radius screw-insert-top-radius screw-insert-height))

		; Wall Thickness W:\t1.65;
		(def screw-insert-outers (screw-insert-all-shapes (+ screw-insert-bottom-radius 2.2) (+ screw-insert-top-radius 2.2) (+ screw-insert-height 1)))
		(def screw-insert-screw-holes  (screw-insert-all-shapes 2 2 350))

	; Connectors between outer column and right wall when 1.5u keys are used
	; this is the top one extended
		(def pinky-connectors
			(if pinky-15u
				(apply union
							(concat
								;; Row connections
								(for [row (range first-15u-row (inc last-15u-row))]
									(triangle-hulls
									(key-place lastcol row web-post-tr)
									(key-place lastcol row wide-post-tr)
									(key-place lastcol row web-post-br)
									(key-place lastcol row wide-post-br)))

								(if-not (= last-15u-row extra-cornerrow) (for [row (range last-15u-row (inc last-15u-row))]
																													(triangle-hulls
																														(key-place lastcol (inc row) web-post-tr)
																														(key-place lastcol row wide-post-br)
																														(key-place lastcol (inc row) web-post-br))))
								(if-not (= first-15u-row 0) (for [row (range (dec first-15u-row) first-15u-row)]
																							(triangle-hulls
																							(key-place lastcol row web-post-tr)
																							(key-place lastcol (inc row) wide-post-tr)
																							(key-place lastcol row web-post-br))))

								;; Column connections
								(for [row (range first-15u-row last-15u-row)]
									(triangle-hulls
									(key-place lastcol row web-post-br)
									(key-place lastcol row wide-post-br)
									(key-place lastcol (inc row) web-post-tr)
									(key-place lastcol (inc row) wide-post-tr)))
								(if-not (= last-15u-row extra-cornerrow) (for [row (range last-15u-row (inc last-15u-row))]
																													(triangle-hulls
																														(key-place lastcol row web-post-br)
																														(key-place lastcol row wide-post-br)
																														(key-place lastcol (inc row) web-post-tr))))
								(if-not (= first-15u-row 0) (for [row (range (dec first-15u-row) first-15u-row)]
																							(triangle-hulls
																							(key-place lastcol row web-post-br)
																							(key-place lastcol (inc row) wide-post-tr)
																							(key-place lastcol (inc row) web-post-tr))))))))

;;;;;;;;;;;;;;;;;
;## IC holder ;;;
;;;;;;;;;;;;;;;;;

	(def usb-hole
		(union
			(translate [0 0 10]
				(minkowski
					(binding [*fn* 100] (cylinder 0.95 10))
					(cube 7.34 1.56 10)))
			(translate [0 1.91 10.7]
				(cube 12.3 4.18 18.6))
			(translate [-5.62 -0.58 10.7]
				(cube 0.6 0.8 18.6))
			(translate [5.62 -0.58 10.7]
				(cube 0.6 0.8 18.6))
			(translate [0 4.5 20]
				(cube 2 5 5))))

	(def trrs-hole
		(union
			(translate [0 0 10]
				(binding [*fn* 100] (cylinder 3.2 20)))
			(translate [0 0.85 11.2]
				(cube 10.5 8 17.6))
			(translate [0 4.5 20]
				(cube 2 5 5))))

	(def pi-cube
		(union
			(translate [5.7 24.55 -1.5]
				(cube 3 5.9 5))
			(translate [5.7 23.525 2.5]
				(binding [*fn* 100] (cylinder 1 3)))))

	(def pi-hole
		(difference
			(cube 24 55 10)
			(union
				pi-cube
				(mirror [0 1 0]
					pi-cube)
				(mirror [1 0 0]
					pi-cube)
				(rotate [0 0 pi]
					pi-cube))))

	(def ic-wall
		(union
			(translate [0 -2.4 4.35]
				(cube 28.2 2.8 15.2) ;28.2
				(translate [0 0 0]
					(cube 30.9 0.9 15.2)) ;30.9
			)
			(translate [-7.05 -12.8 5] ;-7.05
				(cube 14.1 18 10))
			(translate [7.05 -12.8 5] ;7.05
				(cube 14.1 18 10))
			(translate [0 -50.8 4]
				(cube 28.2 58 8)))) ;28.2

	(def ic-fixture-pre-fix
		(difference
				ic-wall
			(translate [7 -1 6] ;7
				(rotate [(deg2rad 90) 0 0]
					usb-hole))
			(translate [-7 -1 6]
				(rotate [(deg2rad 90) 0 0]
					trrs-hole))
			(translate [0 -50.8 4]
				pi-hole)))

	; fix to compensate insert wall thickness for various sizes
	(if (= nrows 4)
		(def ic-fixture-pre-cut
			(difference
				ic-fixture-pre-fix
				(translate [0 -3.7 -1.625]
					(cube 28.2 0.2 3.25)))))
	(if (= nrows 5)
		(def ic-fixture-pre-cut
			(union
				ic-fixture-pre-fix
				(translate [0 -4.075 -1.625]
					(cube 28.2 0.55 3.25)))))
	(if (= nrows 6)
		(def ic-fixture-pre-cut
			(union
				ic-fixture-pre-fix
				(translate [0 -4.325 -1.625]
					(cube 28.2 1.05 3.25)))))
	
	; ic fixture based on plate outside or inside
	(if plate-outside
		(do
			(def ic-fixture
				(difference
					ic-fixture-pre-cut
					(translate [0 0 -1.625]
						(cube 200 200 3.25)))))
		(do
			(def ic-fixture
				ic-fixture-pre-cut)))


	;;; START other ic holders
	;;; HERE
	;;;


	; blank ic holder, for promicro or smaller
	(def ic-blank
		(difference
			(union
				; case wall
				(translate [0 -2.4 4.35]
					(cube 28.2 2.8 15.2) ;28.2
					(translate [0 0 0]
						(cube 30.9 0.9 15.2)) ;30.9
				)
				; component insert space
				(translate [0 -25 5]
					(cube 28.2 48 10))		
			)
		)
	)

	; usbc cutout from blank
	(def ic-usbc-cut
		(union
			; cutout for pcb
			(union
				(translate [0 -21.8 0]
					(cube 16.5 36 50))
				(translate [0 -21.8 12]
					(cube 18.3 36 10))
				(translate [0 -21.45 8]
					(cube 18.3 38.5 2))
				(translate [0 -3.6 6.25]
					(cube 16.5 2.8 1.5))
				;cube and circle for heat insert
				(translate [0 -35 11]
					(cube 18.3 30 8))
				(translate [0 -44.4 0]
					(binding [*fn* 100] (cylinder 2.1 100)))
			)
			; cutout for USB-C connector
			(union
				(translate [0 0 5.4]
					(rotate [(deg2rad 90) 0 0]
						(minkowski
							(binding [*fn* 100] (cylinder 0.95 10))
							(cube 7.34 1.56 10)
						)
					)
				)
			)
		)
	)

	; trrs cutout from blank
	(def ic-trrs-cut
		; trrs insert
		(union
			(translate [10 -11.8 7.4]
				(cube 6.1 16 10)
			)
			(translate [10 -10.6 5.4]
				(cube 6.1 17.5 6)
			)
			;hole for the cover
			(translate [10 -30 8.5]
				(cube 6.1 40 3))
			(translate [8 -42 8.5]
				(cube 10 14 3))

			(translate [10 -5 5.4]
				(rotate [(deg2rad 90) 0 0]
					(binding [*fn* 50] (cylinder 2.85 10)))
			)
		)
	)

	; a insert to hold everything from falling out with trrs and promicro
	(def ic-cover-trrs
		(translate [0 0 0]
			(difference
				(union
					; main ic holding cube
					(translate [0 7.25 0]
					(cube 18.2 30 3))
					; side lever
					(translate [12.65 9.075 0]
						(cube 6 33.65 3))
					; small protrusion connecting side lever and main ic holder
					(translate [9.4 -1.25 0]
						(cube 0.6 13 3))
					; the thing that keeps trrs connector from sliding back
					(translate [12.65 23.45 -3.85]
						(cube 6 3.9 4.7))
				)
				(union
					(translate [0 7 0]
						(translate [0 0 -1]
								(cube 18.2 3 1))
						(translate [6.1 0 0]
							(cube 6 3 10))
						(translate [-6.1 0 0]
							(cube 6 3 10))
						(translate [0 16.5 0]
							(cube 18.2 30 3))
					)
					; screw hole
					(translate [0 -2.75 0]
						(binding [*fn* 100] (cylinder [1.75 3.25] 3)))
				)
			)
		)
	)

	; a insert to hold everything from falling out, xiao seeeduino
	(def ic-cover-xiao
		(translate [0 0 0]
			(difference
				(union
					; main ic holding cube
					(translate [0 7.25 0]
					(cube 18.2 30 3))
					
				)
				(union
					; modify Y value for IC size
					(translate [0 19.3 0]
						(translate [0 0 -1]
								(cube 18.2 3 1))
						(translate [6.1 0 0]
							(cube 6 3 10))
						(translate [-6.1 0 0]
							(cube 6 3 10))
						(translate [0 16.5 0]
							(cube 18.2 30 3))
					)
					; screw hole
					(translate [0 -2.75 0]
						(binding [*fn* 100] (cylinder [1.75 3.25] 3)))
				)
			)
		)
	)

	; ic-ble
	(def ic-ble
		(difference
			ic-blank
			ic-usbc-cut
		)
	)

	; ic-pro-trrs
	(def ic-pro-trrs
		(difference
			ic-blank
			(translate [-4.45 0 0]
				ic-usbc-cut)
			(translate [-1.75 0 0]
				ic-trrs-cut)
		)
	)

	; fix to compensate insert wall thickness for various sizes
	(if (= nrows 4)
		(def ic-ble-fix
			(difference
				ic-ble
				(translate [0 -3.7 -1.625]
					(cube 28.2 0.2 3.25)))))
	(if (= nrows 5)
		(def ic-ble-fix
			(union
				ic-ble
				(translate [0 -4.075 -1.625]
					(cube 28.2 0.55 3.25)))))
	(if (= nrows 6)
		(def ic-ble-fix
			(union
				ic-ble
				(translate [0 -4.325 -1.625]
					(cube 28.2 1.05 3.25)))))
	
	; ic fixture based on plate outside or inside
	(if plate-outside
		(do
			(def ic-ble-fin
				(difference
					ic-ble-fix
					(translate [0 0 -1.625]
						(cube 200 200 3.25)))))
		(do
			(def ic-ble-fin
				ic-ble-fix)))

	; fix to compensate insert wall thickness for various sizes
	(if (= nrows 4)
		(def ic-pro-trrs-fix
			(difference
				ic-pro-trrs
				(translate [0 -3.7 -1.625]
					(cube 28.2 0.2 3.25)))))
	(if (= nrows 5)
		(def ic-pro-trrs-fix
			(union
				ic-pro-trrs
				(translate [0 -4.075 -1.625]
					(cube 28.2 0.55 3.25)))))
	(if (= nrows 6)
		(def ic-pro-trrs-fix
			(union
				ic-pro-trrs
				(translate [0 -4.325 -1.625]
					(cube 28.2 1.05 3.25)))))
	
	; ic fixture based on plate outside or inside
	(if plate-outside
		(do
			(def ic-pro-trrs-fin
				(difference
					ic-pro-trrs-fix
					(translate [0 0 -1.625]
						(cube 200 200 3.25)))))
		(do
			(def ic-pro-trrs-fin
				ic-pro-trrs-fix)))

	(spit "things/custom/ic-ble.scad"
		(write-scad
			ic-ble-fin))

	(spit "things/custom/ic-pro-trrs.scad"
		(write-scad
			ic-pro-trrs-fin))

	(spit "things/custom/ic-cover-trrs.scad"
		(write-scad
			ic-cover-trrs))

	(spit "things/custom/ic-cover-xiao.scad"
		(write-scad
			ic-cover-xiao))
		
;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;## Hand rest generation ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;

	; shape for magnet insert on case - to cut the case
	(def insert-shape
		(translate [0 2 6.4] ; 0 2 6.4
			(rotate [(deg2rad 90) 0 0]
				(binding [*fn* 30] (cylinder 2.7 5.5))
				(translate [0 2 -2.25]
					(cube 1 5 10)))))

	; shape for magnet insert on case - to extend from case
	(def insert-socket
		(difference
			(translate [0 2 6.4]
				(rotate [(deg2rad 90) 0 0]
					(binding [*fn* 30] (cylinder 3.5 5.5))))
			insert-shape
			(translate [0 0 10]
				(cube 1 15 10))))

	; tunnel shape to flaten the slope on hand rest
	(def wrist-attach-rest
		(translate [0 0 2.9]
			(union
				(translate [0 0 2.9]
					(rotate [(/ pi 2) 0 0]
						(binding [*fn* 100] (cylinder 4 20))))
				(cube 8 20 5.8))))

	; exact position of the hand rest slope flatenning shape
	(def seats-on-rest
		(union
			(translate [hook1-placement-x (- (+ hand-rest-position-y (/ hand-rest-width 2)) 10) 0]
				wrist-attach-rest)
			(translate [hook2-placement-x (- (+ hand-rest-position-y (/ hand-rest-width 2)) 10) 0]
				wrist-attach-rest)))

	; universal joiner - the tunnel that extends from the wrist rest
	(defn joiner [length]
		(translate [0 0 2.9]
			(difference
				(union
					(translate [0 0 2.9]
						(rotate [(/ pi 2) 0 0]
							(binding [*fn* 100] (cylinder 4 length))))
					(cube 8 length 5.8))
				(union	;a slope at the end of magnets for proper aligning with the case
					(translate [0 (+ (/ length 2) 2.3) 5]	;at 6 degree rotation 3.34 is right at the tip
						(rotate [(deg2rad -6) 0 0]
							(cube 20 5 20))))
			)
		)
	)

	; the joiner (tunnel) hole for magnet - separate to cut into the hand rest if short tunnel
	(defn joiner-hole [length]
		(translate [0 0 2.9]
			(translate [0 (- (/ length 2) 2.65) 3.65]
				(rotate [(/ pi 2) 0 0]
					(binding [*fn* 100] (cylinder 2.7 5.3))))))

	; placing the magnet holes cut on the case
	(def wrist-attach-case
		(union
			(translate [hook1-placement-x hook1-placement-y 0]
				insert-shape)
			(translate [hook2-placement-x hook2-placement-y 0]
				insert-shape)))

	; placing the magnet holes extensions on the case
	(def wrist-attach-case-socket
		(union
			(translate [hook1-placement-x hook1-placement-y 0]
				insert-socket)
			(translate [hook2-placement-x hook2-placement-y 0]
				insert-socket)))

	; joiner (tunnel) calculation based on keyboard position (size and options) and hand rest postion
	(def length1  (* (- (+ hand-rest-position-y (/ hand-rest-width 2)) hook1-placement-y) -1))
	(def length2  (* (- (+ hand-rest-position-y (/ hand-rest-width 2)) hook2-placement-y) -1))

	; placing the actual joiners to correct position
	(def joiners
		(union
			(translate [hook1-placement-x (+ hand-rest-position-y (+ (/ length1 2) (/ hand-rest-width 2))) 0]
				(joiner (+ length1 0))) ; shorter one
			(translate [hook2-placement-x (+ hand-rest-position-y (+ (/ length2 2) (/ hand-rest-width 2))) 0]
				(joiner (+ length2 0))) )) ; longer one

	; placing the joiner holes to correct position
	(def joiner-holes
		(union
			(translate [hook1-placement-x (+ hand-rest-position-y (+ (/ length1 2) (/ hand-rest-width 2))) 0]
				(joiner-hole (+ length1 0))) ; shorter one
			(translate [hook2-placement-x (+ hand-rest-position-y (+ (/ length2 2) (/ hand-rest-width 2))) 0]
				(joiner-hole (+ length2 0))) ; longer one
		)
	)

	; making of the sloped sides hand rest
	(def hand-rest-cube
		(extrude-linear {:height hand-rest-height :scale hand-rest-top :fn 100}
			(minkowski
				(square (- hand-rest-length (* hand-rest-radius 2)) (- hand-rest-width (* hand-rest-radius 2)))
				(binding [*fn* 20] (circle hand-rest-radius)))))

	; the slope on top of the hand rest
	(def hand-rest-slope
		(translate [0 0 
			(+ 0.00 
				(- (/ hand-rest-height 2)
					(+ (* (Math/tan (deg2rad hand-rest-y))
						(/ (* hand-rest-top hand-rest-length) 2))
							(* (Math/tan (deg2rad hand-rest-x))
								(/ (* hand-rest-top hand-rest-width) 2)))))]
			(rotate [(deg2rad hand-rest-x) (deg2rad hand-rest-y) 0]
				(translate [0 0 (* 25 hand-rest-height)]
					(extrude-linear {:height (* 50 hand-rest-height) :scale 2 :fn 100}
						(square (* 50 hand-rest-length) (* 50 hand-rest-width)))))))

	; the hand rest insert to cut the lip
	(def hand-rest-insert
		(translate [0 0 (* -1 hand-rest-lip-height)]
			(intersection
				(extrude-linear {:height 1000}
					(project
						(translate [0 0 1000]
							(intersection
								(scale [(/ (- hand-rest-length (* 2 hand-rest-lip-width)) hand-rest-length)
												(/ (- hand-rest-width (* 2 hand-rest-lip-width)) hand-rest-width)
												1]
									hand-rest-cube)
								hand-rest-slope))))
				hand-rest-slope)))

	; hand rest insert to print for leather padding
	(def hand-rest-insert-print
		(difference
			(scale [0.975 0.975 0.975]
				hand-rest-insert)
			(translate [0 0 10]
				hand-rest-insert)))

	; putting pads on bottom of hand rests
	(def hand-rest-pads
		(union
			(translate [(- (/ hand-rest-length 2) pad-off)
									(- (/ hand-rest-width 2) pad-off)
									(+ (/ hand-rest-height -2) (/ pad-z 2))]
				(binding [*fn* 100] (cylinder pad-r pad-z)))
			(translate [(+ (/ hand-rest-length -2) pad-off)
									(- (/ hand-rest-width 2) pad-off)
									(+ (/ hand-rest-height -2) (/ pad-z 2))]
				(binding [*fn* 100] (cylinder pad-r pad-z)))
			(translate [(- (/ hand-rest-length 2) pad-off)
									(+ (/ hand-rest-width -2) pad-off)
									(+ (/ hand-rest-height -2) (/ pad-z 2))]
				(binding [*fn* 100] (cylinder pad-r pad-z)))
			(translate [(+ (/ hand-rest-length -2) pad-off)
									(+ (/ hand-rest-width -2) pad-off)
									(+ (/ hand-rest-height -2) (/ pad-z 2))]
				(binding [*fn* 100] (cylinder pad-r pad-z)))))

	; combining all stuff to make the actual hand rest
	(def hand-rest
		(difference
			hand-rest-cube
			hand-rest-slope
			hand-rest-insert
			hand-rest-pads
		)
	)

	; combining the hand rest with joiners and holes and
	; positioning the rest (for image extraction purposes mainly)
	(def hand-rest-final
		(difference
			(union
				(translate [hand-rest-position-x hand-rest-position-y (/ hand-rest-height 2)]
					hand-rest)
				seats-on-rest
				joiners)
			joiner-holes))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;## Bottom plate and according case modification ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

	; case-walls cut thin to use projection instead of cut
	;(projection (cut=true) gives no geometry in some cases)
	(def case-walls-cut
		(difference
			case-walls
			(translate [0 0 100.1]
				(cube 400 400 200))))

	; bottom plate generation
	(if plate-outside
		; generate the plate with invisible edge
		(do
			(def model-right-plate
				(difference
					(fill
						(project case-walls-cut))
					(cut screw-insert-screw-holes))))
		
		; generate the plate with visible edge
		(do
			(def model-right-plate
				(difference
					(offsetd -0.3 ; plate offset from case edge (/2)
						(difference
							(fill
								(project case-walls-cut))
							(project case-walls-cut)))
					(cut screw-insert-screw-holes)))))

	; Pad and text positioning
		; positioning for thumb cluster
		(when (= thumb-style "default")
			(def plate-pad-br-x -85.8)
			(def plate-pad-br-y -71.4)
			(def plate-pad-bm-x -57.1)
			(def plate-pad-bm-y -94.5))
		(when (= thumb-style "mini")
			(def plate-pad-br-x -81.2)
			(def plate-pad-br-y -52.5)
			(def plate-pad-bm-x -59.3)
			(def plate-pad-bm-y -80.7))
		(when (= thumb-style "tightly")
			(def plate-pad-br-x -86.5)
			(def plate-pad-br-y -65.6)
			(def plate-pad-bm-x -26)
			(def plate-pad-bm-y -47.9))

		; positioning for nrows
		(when (= nrows 4)
			(def plate-pad-tr-x -67.5)
			(def plate-pad-tr-y 29.6)
			(def plate-pad-tl-y 17.9)
			(def text-r-y -22)
			(def text-l-y -22))
		(when (= nrows 5)
			(def plate-pad-tr-x -62.1)
			(def plate-pad-tr-y 49.4)
			(def plate-pad-tl-y 38)
			(def text-r-y -12)
			(def text-l-y -12))
		(when (= nrows 6)
			(def plate-pad-tr-x -54.7)
			(def plate-pad-tr-y 65.3)
			(def plate-pad-tl-y 54.2)
			(def text-r-y -2)
			(def text-l-y -2))

		; positioning for ncols
		(when (= ncols 5)
			(def plate-pad-bl-x 29.2)
			(def plate-pad-bl-y -41.9)
			(def plate-pad-tl-x 29.1)
			(def text-r-x -15)
			(def text-l-x -55))
		(when (= ncols 6)
			(def plate-pad-bl-x 50.5)
			(def plate-pad-bl-y -41.9)
			(def plate-pad-tl-x 50.5)
			(def text-r-x -25)
			(def text-l-x -45))
		(when (= ncols 7)
			(def plate-pad-bl-x 72)
			(def plate-pad-bl-y -41.9)
			(def plate-pad-tl-x 72)
			(def text-r-x -36)
			(def text-l-x -36))

		; positioning for pinky 1.5u
		(if pinky-15u
			(def plate-pad-pinky-off 9.9)
			(def plate-pad-pinky-off 0)
			)

		; positining for extra row
		(if extra-row
			(def plate-pad-row-off -20.1)
			(def plate-pad-row-off 0)
			)

	; function for placing pads on plate
	(defn plate-pad [offset]
		(translate offset
			(translate [0 0 (/ pad-z 2)]
				(binding [*fn* 100] (cylinder pad-r pad-z)))))

	; function for placing text on the plate
	(defn plate-text [offset]
		(rotate [0 0 pi]
			(translate offset
				(translate [0 0 (+ -2.5 text-z)]
					(extrude-linear {:height 5}
						(text "K33B" :size 20 :font "Oxanium:style=ExtraBold"))))))

	; generation of actual plate with specified options
	(defn plate-printed [pos]
		(mirror [pos 0 0] ; mirror based on left or right plate
			(rotate [pi 0 0] ; rotate for correct orientation when importing for slicing
				(difference
					(extrude-linear {:height 3 :center false}
						model-right-plate)
					(translate [0 0 0]
						(screw-insert-all-shapes 3.25 1.75 2))
					(union
						(plate-pad [plate-pad-br-x plate-pad-br-y 0])
						(plate-pad [plate-pad-bm-x plate-pad-bm-y 0])
						(plate-pad [(+ plate-pad-bl-x plate-pad-pinky-off) (+ plate-pad-bl-y plate-pad-row-off) 0])
						(plate-pad [plate-pad-tr-x plate-pad-tr-y 0])
						(plate-pad [(+ plate-pad-tl-x plate-pad-pinky-off) plate-pad-tl-y 0])
					(mirror [0 1 0]
						(mirror [pos 0 0]
							(if (= pos 0)
								(plate-text [text-r-x text-r-y 0]) 
								(plate-text [text-l-x text-l-y 0])))))))))

	; edge on case for better plate support
	(def edge
		(extrude-linear {:height 1.5}
			(difference
				(offsetd -1
					(fill
						(project case-walls-cut)))
				(offsetd -2
					(difference
						(fill
							(project case-walls-cut))
						(project case-walls-cut))))))

	; cleaning the inside of the case where the plate goes
	(def edge-cut
		(extrude-linear {:height 6} 
			(difference
				(fill
					(project case-walls-cut))
				(project case-walls-cut))))

	; cutting the case depending on plate type
	(if plate-outside
		(do
			(def cube-cut
				(translate [0 0 -17] (cube 350 350 40))))
		(do
			(def cube-cut
				(translate [0 0 -20] (cube 350 350 40)))))

;;;;;;;;;;;;;;;;;;;;;;;
;## Building halves ;;;
;;;;;;;;;;;;;;;;;;;;;;;

	(def model-right
		(difference	
			(union		
				key-holes
				(if (and extra-row (= ncols 7))	; placing hot swap sockets on 7 ncols keyboards
					[(key-place 4 (- nrows 1) single-plate)
					(key-place 5 (- nrows 1) single-plate)
					(key-place 6 (- nrows 1) single-plate)])
				key-holes-inner
				pinky-connectors
				extra-connectors
				connectors
				inner-connectors
				thumb-type
				thumb-connector-type
				(difference
					(union
						case-walls
						(translate [0 0 3.75] edge)
						(translate [0 0 3] screw-insert-outers)
						wrist-attach-case-socket)
					usb-holder-space
					usb-holder-notch
					(translate [0 0 3] screw-insert-holes)
					wrist-attach-case
					edge-cut
					))
			cube-cut))

	(def model-left
		(mirror [1 0 0]
			(difference
				(union
					key-holes-left
					(if (and extra-row (= ncols 7))	; placing hot swap sockets on 7 ncols keyboards
						[(key-place 4 (- nrows 1) (mirror [1 0 0] single-plate))
						(key-place 5 (- nrows 1) (mirror [1 0 0] single-plate))
						(key-place 6 (- nrows 1) (mirror [1 0 0] single-plate))])
					key-holes-inner
					pinky-connectors
					extra-connectors
					connectors
					inner-connectors
					thumb-type-left
					thumb-connector-type
					(difference
						(union
							case-walls
							(translate [0 0 3.75] edge)
							(translate [0 0 3] screw-insert-outers)
							wrist-attach-case-socket)
						usb-holder-space
						usb-holder-notch
						(translate [0 0 3] screw-insert-holes)
						wrist-attach-case
						edge-cut
						))
				cube-cut)))

;;;;;;;;;;;;;;;;;
;## Exporting ;;;
;;;;;;;;;;;;;;;;;

	(if (= thumb-style "default")
		(def thumb-style-name "6key"))

	(if (= thumb-style "mini")
		(def thumb-style-name "5key"))

	(if (= thumb-style "tightly")
		(def thumb-style-name "3key"))

	(if extra-row
		(def extra-row-name "_ek")
		(def extra-row-name "")
	)

	(if pinky-15u
		(def options-name "pinky"))
	(if plate-outside
		(def options-name "plate"))
	(if (and pinky-15u plate-outside)
		(def options-name "pinky-and-plate"))
	(if (and (not pinky-15u) (not plate-outside))
		(def options-name "default"))

	(if custom
		(do
			(def folder (str "things/custom/")))
		(do
			(def folder (str "things/scad/" print-type "/" thumb-style-name "/" nrows "x" ncols extra-row-name "/" options-name "/"))
			(def img (str "things/web-resource/scad/" thumb-style-name "-" nrows "x" ncols extra-row-name "-" options-name "-"))
			
			(spit (str img "rest.scad")
				(write-scad
					(union
						hand-rest-final
						model-right
						(rotate [(deg2rad 180) 0 0]
						(plate-printed 0)))))

			(spit (str img "right.scad")
				(write-scad
					(union
						model-right)
						(rotate [(deg2rad 180) 0 0]
						(plate-printed 0))))))

	(spit (str folder "ic-right.scad")
		(write-scad
			ic-fixture))

	(spit (str folder "ic-left.scad")
		(write-scad
			(mirror [1 0 0]
				ic-fixture)))

	(spit (str folder "rest-right.scad")
		(write-scad
			(union
				hand-rest-final)))

	(spit (str folder "rest-left.scad")
		(write-scad
			(mirror [1 0 0]
				hand-rest-final)))

	(spit (str folder "right.scad")
		(write-scad
			model-right))

	(spit (str folder "left.scad")
		(write-scad
			model-left))

	(spit (str folder "plate-right.scad")
		(write-scad
			(difference
				(plate-printed 0))))

	(spit (str folder "plate-left.scad")
		(write-scad
			(difference
				(plate-printed 1))))

	(spit "things/rest-insert-right.scad"
		(write-scad
			hand-rest-insert-print))

	; g-code generation files for cnc mill to cut out wooden rests
	(def tool 3.2)
	(spit "things/gcode/rest-right-gcode.scad"
		(write-scad
			(union
				(rotate [0 0 (deg2rad -90)]
					(scale [1.0384 1.0555 1]
						hand-rest-insert)))))

	(spit "things/gcode/rest-left-gcode.scad"
		(write-scad
			(mirror [1 0 0]
				(union
					(rotate [0 0 (deg2rad 0)]
						(scale [1.0384 1.0555 1]
							hand-rest-insert))))))

	(spit "things/gcode/cnc-tool-left.scad"
		(write-scad
			(mirror [1 0 0]
				(difference
					hand-rest-insert
					(cube 200 200 100)
					(translate [0 0 68]
						hand-rest-insert)))))

	(spit "things/gcode/cnc-tool-right.scad"
		(write-scad
			(difference
				hand-rest-insert
				(cube 200 200 100)
				(translate [0 0 68]
					hand-rest-insert))))

;;;;;;;;;;;;;;;;
;## End code ;;;
;;;;;;;;;;;;;;;;

	(defn -main [dum] 1)  ; dummy to make it easier to batch