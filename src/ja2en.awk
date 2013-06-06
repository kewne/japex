#!/usr/bin/awk -f

BEGIN { FS = ":" }

{ print $1
	getline user_guess < "/dev/stderr"
	print user_guess " -> " $2 }
