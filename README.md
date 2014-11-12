# groupsitesr

This is code to group sites, in R.  Hence, groupsitesr!

# travis

I'm going to try to use travis ci

[![Build Status](https://travis-ci.org/jmarca/groupsitesr.png?branch=master)](https://travis-ci.org/jmarca/groupsitesr)

# caveat emptor

This code is for me to use really, but you're welcome to it.

# what it does

The idea is to group sites together so that you can pick out a minimum
set of sites, convert those to the new classifying VDS type, and then
improve the overall source data for CalVAD.

The code allows setting an initial number of sites to convert, and a
preferred distance between sites in order to "belong" to a converted
site.  Basically the solution is to use an heuristic that adds to the
"conversion" set the single site that is closest to the most
remaining, non-covered sites.  A set covering problem solution.

This solution is completely different from one that looks at the best
locations from the perspective of capturing flow patterns.

# license

GPLV2
