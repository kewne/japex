#!/usr/bin/awk -f

BEGIN { FS = ":" }

{ print $2
	getline user_guess < "/dev/stderr"
	print user_guess " -> " $1 }
