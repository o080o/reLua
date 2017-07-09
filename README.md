# About

reLua is a pure lua regular expression library that uses a breadth-first NFA algorithm to match in linear time with respect to the input string, avoiding pathological exponential running times of most common regex algorithms. Submatches are supported using parentheses, as well as alternations, kleen star, lazy repetitions, and character groups, to name a few features. The algorithm can easily be extended to include back references and recursion, however it is not currently supported.

reLua exists for people who want regex's with more power than Lua's built-in pattern matching, but don't need all the myriad of features of something like PCRE. reLua is written entirely in Lua, so it can be used in all your projects!

This is written by myself, for myself, and the code is here for other people's convenience. If you like it, let me know! I'd love to hear about it!

## Limitations

This project only supports *basic* regex syntax. namely, "()", "?" "\*", "|",
character classes via "[...]", "+", "-".

For a full list, check out the *grammar* object in  reParse.lua

in addition, at the moment, matches always start at the begining of the string, and match
until the end. (so, anchors like "^" are basically always on... maybe add ".-" to
the start of a regex?)

basic usage:
```
  re = require("re")
  local regex = re.compile("r(e*)gex?")
  local match = regex:execute("reeeeegex")
  print( match ) -- note that 'match' will be nil if there was no match. Also,
	  --all blocks that match a variable number of characters such as "*" or "?"
	  --automatically create a capture group as part of the matching algorithm.
	  --Rather than throw than information away, it is saved in the resulting match
	  --object.
```
The code is licensed under the ZLib license.
