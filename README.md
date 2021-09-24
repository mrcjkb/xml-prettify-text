# xml-prettify-text

XML pretty printer based on [xml-prettify](https://github.com/rosenbergdm/xml-prettify) by David M. Rosenberg. Modified to work with `Text` instead of `String` and tuned for improved performance.

[![Build status](https://img.shields.io/travis/MrcJkb/xml-prettify-text.svg?logo=travis)](https://app.travis-ci.com/MrcJkb/xml-prettify-text)
[![Hackage](https://img.shields.io/hackage/v/xml-prettify-text.svg?logo=haskell)](https://hackage.haskell.org/package/xml-prettify-text)
[![Stackage Lts](http://stackage.org/package/xml-prettify-text/badge/lts)](http://stackage.org/lts/package/xml-prettify-text)
[![Stackage Nightly](http://stackage.org/package/xml-prettify-text/badge/nightly)](http://stackage.org/nightly/package/xml-prettify-text)
[![GPL-2.0-only license](https://img.shields.io/badge/license-GPL--2.0--only-blue.svg)](LICENSE)

## Usage ##
```bash
Usage: xml-prettify ((-f|--file FILE_NAME) | (-t|--text XML_TEXT))
                    [(-o|--out FILE_NAME) | (-c|--console)]
                    [--indent-style <TAB | SPACE SIZE>] [--eol <LF | CR | CRLF>]
  Pretty-prints XML text

Available options:
  -f,--file FILE_NAME      XML file to pretty-print
  -t,--text XML_TEXT       XML text to pretty-print
  -o,--out FILE_NAME       XML file to pretty-print to
  -c,--console             Output the pretty-printed XML to the console
  --indent-style <TAB | SPACE SIZE>
                           The indent style (TAB or SPACE
                           INDENT_SIZE) (default: SPACE 2)
  --eol <LF | CR | CRLF>   The line-break style: Line Feed (LF), Carriage Return
                           (CR), or both (CRLF) (default: LF)
  -h,--help                Show this help text
  ```

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
- [x] Profile, benchmark & squeeze performance
- [x] Add options for specifying newlines, indent-style, indent-size, etc.
- [x] Create cli app

