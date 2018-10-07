default: all

all: haskell elm

clearcache:
	@rm ./cache/*

elm:
	@elm make ./src/elm/Main.elm --output=./index.html

haskell:
	@stack build

run:
	@stack exec eps

watch:
	@stack build --file-watch
