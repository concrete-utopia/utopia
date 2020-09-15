string-hash
===========

A fast string hashing function for Node.JS. The particular algorithm is quite
similar to `djb2`, by Dan Bernstein and available
[here](http://www.cse.yorku.ca/~oz/hash.html). Differences include iterating
over the string *backwards* (as that is faster in JavaScript) and using the XOR
operator instead of the addition operator (as described at that page and
because it obviates the need for modular arithmetic in JavaScript).

The hashing function returns a number between 0 and 4294967295 (inclusive).
