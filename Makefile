default: help

all: haskell elm

clearcache:
	@rm ./cache/*

elm:
	@elm make ./src/elm/Main.elm --output=./index.html

haskell:
	@stack build

help:
	@echo "all -- compile haskell and elm"
	@echo "clearcache -- remove all files in ./cache"
	@echo "elm -- compile elm to ./index.html"
	@echo "haskell -- compile haskell"
	@echo "server -- start haskell server"
	@echo "watch -- watch haskell files and compile on change"

server:
	@stack exec eps

watch:
	@stack build --file-watch
