default: all

all: build

build:
	@stack build

clearcache:
	@rm cache/all.json
	@rm cache/search.json

elm:
	@elm make src/elm/Main.elm --output=elm.js

run:
	@stack exec eps

watch:
	@stack build --file-watch
