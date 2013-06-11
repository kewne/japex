#!/usr/bin/awk -f

BEGIN { FS = ":"
	if (REVERSE) { original=2; translated=1 }
	else { original=1;translated=2 }
}

{ print $original
	getline user_guess < "/dev/stderr"
	print user_guess " -> " $translated
}
