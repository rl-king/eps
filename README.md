# eps
[![Build Status](https://travis-ci.org/rl-king/eps.svg?branch=master)](https://travis-ci.org/rl-king/eps)

## Getting started
Run `make haskell` to build and `make server` to start the server.

For local dev, and not hitting the official package server all the time,
run `Mock.cachePackages` on ghci. This Downloads all docs to `./cache`
which can than be served by just running eps.

```
all -- compile haskell and elm
clearcache -- remove all files in ./cache
elm -- compile elm
haskell -- compile haskell
server -- start eps server
watch -- watch haskell files and compile on change
```

## Todo
* Extract factual information to help rank packages
  * How complete are the docs
    * Examples
  * Does it have tests
  * Dependencies
* Search for uses of a data type

## Idea
https://github.com/elm/projects#package-search
