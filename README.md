# xml-prettify-text

[WIP] XML pretty printer based on [xml-prettify](https://github.com/rosenbergdm/xml-prettify) by David M. Rosenberg. Modified to work with `Text` instead of `String` and tuned for improved performance.

[![Build Status](https://app.travis-ci.com/MrcJkb/xml-prettify-text.svg?branch=master)](https://app.travis-ci.com/MrcJkb/xml-prettify-text)
[![Hackage](https://img.shields.io/hackage/v/xml-prettify-text.svg?logo=haskell)](https://hackage.haskell.org/package/xml-prettify-text)
[![Stackage Lts](http://stackage.org/package/xml-prettify-text/badge/lts)](http://stackage.org/lts/package/xml-prettify-text)
[![Stackage Nightly](http://stackage.org/package/xml-prettify-text/badge/nightly)](http://stackage.org/nightly/package/xml-prettify-text)
[![GPL-2.0-only license](https://img.shields.io/badge/license-GPL--2.0--only-blue.svg)](LICENSE)

See README for more info

## Motivation ##
Why re-implement [xml-prettify](https://github.com/rosenbergdm/xml-prettify)? 
- I am not satisfied with the [Text.PrettyPrint](https://hackage.haskell.org/package/pretty-1.1.3.6/docs/Text-PrettyPrint.html#t:Doc) output.
- `xml-prettify` produces good output, but with poor performance.
- `xml-prettify` appears not to be maintained (I haven't been able to use it as a library in modern projects).
- Some more configurability (see TODOs) would be great.
- A good project for me to practise profiling and benchmarking in Haskell.

## TODO ##
- [x] Port xml-prettify for use with Text
- [x] Refactor for better readability
- [x] Implement golden tests
- [ ] Profile, benchmark & squeeze performance
- [ ] Add options for specifying newlines, indent-style, indent-size, etc.
- [ ] Create app

