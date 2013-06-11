#!/usr/bin/awk -f

BEGIN { FS = ":"
	NUM_CATS = split(CATEGORIES, cats, ",")
	print "Filtering with categories..." > "/dev/stderr"
	for (c in cats) { print cats[c] > "/dev/stderr" }
	print > "/dev/stderr"
}

function matches_category(category_string,   matched_categories)
{
	split(category_string, item_cats, ",")
	matched_categories = 0
	for (c in cats)
	{
		for (i in item_cats)
		{
			if (cats[c] == item_cats[i])
			{
				matched_categories++
				break
			}
		}
	}
	return matched_categories == NUM_CATS
}

matches_category($3) { print }
