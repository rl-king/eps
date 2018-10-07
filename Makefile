default: all

all: build

build:
	@stack build

clearcache:
	@rm cache/all.json
	@rm cache/search.json

run:
	@stack exec eps

watch:
	@stack build --file-watch
