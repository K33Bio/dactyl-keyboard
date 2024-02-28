#!/bin/bash

# all the options
thumb=(default mini tightly)
nrows=(4 5 6)
ncols=(5 6 7)
pinky=(0 1)
ek=(0 1)
plate=(0 1)
print=(fdm msla)
side=(left right)
part=(ic plate rest kb)

# directories pre string
scad_dir="things/scad/"
stl_dir="things/stl/"

function the_loop {
	for a in "${thumb[@]}"
	do
		for b in "${nrows[@]}"
		do
			for c in "${ncols[@]}"
			do
				for d in "${pinky[@]}"
				do
					for e in "${ek[@]}"
					do
						for f in "${plate[@]}"
						do
							for g in "${print[@]}"
							do
								# options is combining plate and pinky
								if [ $d -eq 0 -a $f -eq 0 ]
								then
									options="default"
								elif [ $d -eq 0 -a $f -eq 1 ]
								then
									options="plate"
								elif [ $d -eq 1 -a $f -eq 0 ]
								then
									options="pinky"
								elif [ $d -eq 1 -a $f -eq 1 ]
								then
									options="pinky-and-plate"
								fi

								# extra keys to bool and name
								if [ $e -eq 0 ]
								then
									er="false"
									ern=""
								elif [ $e -eq 1 ]
								then
									er="true"
									ern="_ek"
								fi
								# pinky to bool
								if [ $d -eq 0 ]
								then
									pin="false"
								elif [ $d -eq 1 ]
								then
									pin="true"
								fi
								# plate to bool
								if [ $f -eq 0 ]
								then
									pla="false"
								elif [ $f -eq 1 ]
								then
									pla="true"
								fi

								# thumb cluster alter name
								if [ $a == "default" ]
								then
									thumb_alt="6key"
								elif [ $a == "mini" ]
								then
									thumb_alt="5key"
								elif [ $a == "tightly" ]
								then
									thumb_alt="3key"
								fi

								# directory creation
								dir=$g"/"$thumb_alt"/"$b"x"$c$ern"/"$options"/"
								scad_dir_final=$scad_dir$dir
								stl_dir_final=$stl_dir$dir

								# option: generate dirs
								if [ $proc -eq 1 ]
								then
									mkdir -p $scad_dir_final
									mkdir -p $stl_dir_final
									mkdir -p "things/web-resource/scad"
									mkdir -p "things/web-resource/img"
									mkdir -p "things/web-resource/stl"
									mkdir -p "things/web-resource/stl-lowpoly"
									mkdir -p "things/custom"
									mkdir -p "things/web-resource/zip"
								fi
								# option: generate .scad
								if [ $proc -eq 2 ]
								then
									echo "(def print-type \""$g"\")\
									(def plate-outside "$pla")\
									(def extra-row "$er")\
									(def pinky-15u "$pin")\
									(def ncols "$c")\
									(def nrows "$b")\
									(def thumb-style \""$a"\")\
									(load-file \"src/dactyl.clj\")" | lein repl
								fi
								# option: zip up all
								if [ $proc -eq 6 ]
								then
									zip_name=$g"-"$thumb_alt"-"$b"x"$c$ern"-"$options".zip"
									stl_location=$stl_dir_final"*"
									zip -j things/web-resource/zip/$zip_name $stl_location
								fi

								for h in "${side[@]}"
								do
									for i in "${part[@]}"
									do

										# option: generate .stl
										if [ $proc -eq 3 ]
										then
											file=$i"-"$h
											if [ $i == "kb" ]
											then
												file=$h
											fi

											echo "Starting "$scad_dir_final$file

											openscad \
												--export-format stl -o $stl_dir_final$file.stl \
												--enable=fast-csg \
												--enable=manifold \
												--render \
												--quiet \
											$scad_dir_final$file".scad"

											echo "--- "$stl_dir_final$file" done!"
										fi

									done
								done
							done
						done
					done
				done
			done
		done
	done
}

function web_export_stl {
	readdir="things/web-resource/scad/"
	savedir="things/web-resource/stl/"

	# file name: thumb cluster
	for k in {1..3}
	do
	if [ $k -eq 1 ]
	then
		thumb="6key-"
	elif [ $k -eq 2 ]
	then
		thumb="5key-"
	elif [ $k -eq 3 ]
	then
		thumb="3key-"
	fi

		# file name: nrows
		for i in {1..3}
		do
		if [ $i -eq 1 ]
		then
			nrows="4x"
		elif [ $i -eq 2 ]
		then
			nrows="5x"
		elif [ $i -eq 3 ]
		then
			nrows="6x"
		fi

			# file name: ncols
			for a in {1..3}
			do
			if [ $a -eq 1 ]
			then
				ncols="5"
			elif [ $a -eq 2 ]
			then
				ncols="6"
			elif [ $a -eq 3 ]
			then
				ncols="7"
			fi

				# file name: extra row
				for x in {1..2}
				do
				if [ $x -eq 1 ]
				then
					extra="_ek-"
				elif [ $x -eq 2 ]
				then
					extra="-"
				fi

					# file name: options
					for d in {1..4}
					do
					if [ $d -eq 1 ]
					then
						options="pinky-"
					elif [ $d -eq 2 ]
					then
						options="plate-"
					elif [ $d -eq 3 ]
					then
						options="pinky-and-plate-"
					elif [ $d -eq 4 ]
					then
						options="default-"
					fi

						# file name: with rest or without
						for w in {1..2}
						do
						if [ $w -eq 1 ]
						then
							rest="rest"
						elif [ $w -eq 2 ]
						then
							rest="right"
						fi

							camera=-30,20,0,40,0,30,600

							file=$thumb$nrows$ncols$extra$options$rest
							
							echo "Starting "$file

							openscad \
								--export-format stl -o $savedir$file.stl \
								--enable=fast-csg \
								--enable=manifold \
								--render \
								--quiet \
								$readdir$file".scad"

							echo "---" $file" done!"

						done
					done
				done
			done
		done
	done
}

function web_export_webp {
	readdir="things/web-resource/scad/"
	savedir="things/web-resource/img/"

	# file name: thumb cluster
	for k in {1..3}
	do
	if [ $k -eq 1 ]
	then
		thumb="6key-"
	elif [ $k -eq 2 ]
	then
		thumb="5key-"
	elif [ $k -eq 3 ]
	then
		thumb="3key-"
	fi

		# file name: nrows
		for i in {1..3}
		do
		if [ $i -eq 1 ]
		then
			nrows="4x"
		elif [ $i -eq 2 ]
		then
			nrows="5x"
		elif [ $i -eq 3 ]
		then
			nrows="6x"
		fi

			# file name: ncols
			for a in {1..3}
			do
			if [ $a -eq 1 ]
			then
				ncols="5"
			elif [ $a -eq 2 ]
			then
				ncols="6"
			elif [ $a -eq 3 ]
			then
				ncols="7"
			fi

				# file name: extra row
				for x in {1..2}
				do
				if [ $x -eq 1 ]
				then
					extra="_ek-"
				elif [ $x -eq 2 ]
				then
					extra="-"
				fi

					# file name: options
					for d in {1..4}
					do
					if [ $d -eq 1 ]
					then
						options="pinky-"
					elif [ $d -eq 2 ]
					then
						options="plate-"
					elif [ $d -eq 3 ]
					then
						options="pinky-and-plate-"
					elif [ $d -eq 4 ]
					then
						options="default-"
					fi

						# file name: with rest or without
						for w in {1..2}
						do
						if [ $w -eq 1 ]
						then
							rest="rest"
						elif [ $w -eq 2 ]
						then
							rest="right"
						fi

							camera=-30,20,0,40,0,30,600

							file=$thumb$nrows$ncols$extra$options$rest
							
							echo "Starting "$file

							openscad \
								--export-format png -o $savedir$file.png \
								--enable=fast-csg \
								--enable=manifold \
								--render \
								--colorscheme PNGExport \
								--imgsize 1500,1500 \
								--camera $camera \
								--quiet \
								$readdir$file".scad"

							background=$(gimp --no-interface -i -b "(bgbegone \"$file\" \".png\" \"$savedir\")" -b "(gimp-quit 0)") #2>/dev/null)

							rm $savedir$file.png # remove the png file

							echo "---" $file" done!"

						done
					done
				done
			done
		done
	done
}


proc=0
PS3='Please enter your choice (7 to quit): '
options=("Generate dirs" "Generate .scad" "Generate .stl" "Generate web .stl" "Generate web .webp" "Zip up models" "Quit")
select opt in "${options[@]}"
do
    case $opt in
        "Generate dirs")
            echo "Generating all directories..."
						proc=1
						the_loop
            ;;
        "Generate .scad")
            echo "Starting to generate .scad files"
						proc=2
						the_loop
            ;;
        "Generate .stl")
            echo "Starting to generate .stl files"
						proc=3
						the_loop
            ;;
				"Generate web .stl")
            echo "Starting to generate web .stl files"
						proc=4
						web_export_stl
            ;;
				"Generate web .webp")
            echo "Starting to generate web .webp files"
						proc=5
						web_export_webp
            ;;
				"Zip up models")
            echo "Staring zipping models..."
						proc=6
						the_loop
            ;;
        "Quit")
            break
            ;;
        *) echo "invalid option $REPLY";;
    esac
done
