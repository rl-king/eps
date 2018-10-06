default: all

all: build

build:
	@stack build

run:
	@stack exec eps

watch:
	@stack build --file-watch
