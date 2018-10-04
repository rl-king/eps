default: all

all: build

build:
	@stack build

watch:
	@stack build --file-watch
