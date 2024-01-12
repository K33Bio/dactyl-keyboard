#!/bin/bash

# a script that exports png files from openscad (.scad) files and uses gimp scripts to make it's background transparent

readdir="things/scad/"

savedir="things/stl/"

# print type directory
for k in {1..2}
do
if [ $k -eq 1 ]
then
	print="fdm/"
elif [ $k -eq 2 ]
then
	print="msla/"
fi

	# thumb cluster directory
	for i in {1..3}
	do
	if [ $i -eq 1 ]
	then
		thumb="6key/"
	elif [ $i -eq 2 ]
	then
		thumb="5key/"
	elif [ $i -eq 3 ]
	then
		thumb="3key/"
	fi

		# keyboard nrows directory
		for a in {1..3}
		do
		if [ $a -eq 1 ]
		then
			nrows="4x"
		elif [ $a -eq 2 ]
		then
			nrows="5x"
		elif [ $a -eq 3 ]
		then
			nrows="6x"
		fi
			# keyboard ncols directory
			for b in {1..3}
			do
			if [ $b -eq 1 ]
			then
				ncols="5"
			elif [ $b -eq 2 ]
			then
				ncols="6"
			elif [ $b -eq 3 ]
			then
				ncols="7"
			fi
				# keyboard extra row directory
				for c in {1..2}
				do
				if [ $c -eq 1 ]
				then
					extrarow="_ek/"
				elif [ $c -eq 2 ]
				then
					extrarow="/"
				fi

					# options directory
					for d in {1..4}
					do
					if [ $d -eq 1 ]
					then
						options="pinky/"
					elif [ $d -eq 2 ]
					then
						options="plate/"
					elif [ $d -eq 3 ]
					then
						options="pinky-and-plate/"
					elif [ $d -eq 4 ]
					then
						options="default/"
					fi

						# file name (left-right)
						for x in {1..2}
						do
						if [ $x -eq 1 ]
						then
							pos="right"
						else
							pos="left"
						fi
							# file name (ic-plate-rest-keyboard)
							for j in {1..4}
							do
							if [ $j -eq 1 ]
							then
								part="ic-"
							elif [ $j -eq 2 ]
							then
								part="plate-"
							elif [ $j -eq 3 ]
							then
								part="rest-"
							elif [ $j -eq 4 ]
							then
								part=""
							fi

								dir=$print$thumb$nrows$ncols$extrarow$options
								file=$part$pos

								echo "Starting "$dir$file

								openscad \
									--export-format stl -o $savedir$dir$file.stl \
									--enable=fast-csg \
									--enable=manifold \
									--render \
									--quiet \
									$readdir$dir$file".scad"

								echo "---" $dir$file" done!"

							done
						done
					done
				done
			done
		done
	done
done
