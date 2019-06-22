# eps
[![Build Status](https://travis-ci.org/rl-king/eps.svg?branch=master)](https://travis-ci.org/rl-king/eps)

## Getting started
Run `make haskell` to build and `make server` to start the server.
`make server` will check `./cache` directory for package files and
fetch those if missing.

```
all -- compile haskell and elm
clearcache -- remove all files in ./cache
elm -- compile elm to ./index.html
haskell -- compile haskell
server -- start haskell server
watch -- watch haskell files and compile on change
```

## Todo
* Filter code out of docs
* Extract factual information to help rank packages
  * How complete are the docs
    * Examples
  * Does it have tests
  * Dependencies
* Maybe add more endpoints with indexed information
* Search for uses of a data type

## Typeface
https://github.com/be5invis/Iosevka

## Idea
https://github.com/elm/projects#package-search
