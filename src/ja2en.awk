#!/usr/bin/awk -f

BEGIN { FS = ":"
	if (REVERSE) { original=2; translated=1 }
	else { original=1;translated=2 }
}

{ print $original
	getline user_guess < "/dev/stderr"
	user_guesses[$original] = user_guess
	answers[$original] = $translated
}

END { 
	for (i in user_guesses)
	{
		print i " -> " answers[i] ", you answered " user_guesses[i]
	}
}
