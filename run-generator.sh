#!/bin/bash

# loop for thumb selection
for i in {1..3}
do

	if [ $i -eq 1 ]
	then
		thumb="(def thumb-style \"default\")"
	elif [ $i -eq 2 ]
	then
		thumb="(def thumb-style \"mini\")"
	elif [ $i -eq 3 ]
	then
		thumb="(def thumb-style \"tightly\")"
	fi

	# loop for nrows selection
	for k in {1..3}
	do

		if [ $k -eq 1 ]
		then
			nrows="(def nrows 4)"
		elif [ $k -eq 2 ]
		then
			nrows="(def nrows 5)"
		elif [ $k -eq 3 ]
		then
			nrows="(def nrows 6)"
		fi

		# loop for ncols selection
		for j in {1..3}
		do
			if [ $j -eq 1 ]
			then
				ncols="(def ncols 5)"
			elif [ $j -eq 2 ]
			then
				ncols="(def ncols 6)"
			elif [ $j -eq 3 ]
			then
				ncols="(def ncols 7)"
			fi

			# loop for pinky size selection
			for x in {1..2}
			do
				if [ $x -eq 1 ]
				then
					pinky="(def pinky-15u false)"
				elif [ $x -eq 2 ]
				then
					pinky="(def pinky-15u true)"
				fi

				# loop for extra-row selection
				for y in {1..2}
				do
					if [ $y -eq 1 ]
					then
						extrarow="(def extra-row false)"
					elif [ $y -eq 2 ]
					then
						extrarow="(def extra-row true)"
					fi

					# loop for plate position selection
					for z in {1..2}
					do
						if [ $z -eq 1 ]
						then
							plate="(def plate-outside false)"
						elif [ $z -eq 2 ]
						then
							plate="(def plate-outside true)"
						fi
						
						# loop for extra-row selection
						for f in {1..2}
						do
							if [ $f -eq 1 ]
							then
								print="(def print-type \"fdm\")"
							elif [ $f -eq 2 ]
							then
								print="(def print-type \"msla\")"
							fi

							echo $thumb$nrows$ncols$pinky$extrarow$plate$print"(load-file \"src/dactyl.clj\")" | lein repl
						
						done
					done
				done
			done
		done
	done
done
