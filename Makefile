.PHONY: build

build:
	docker run -it -v `pwd`:/source -v `pwd`/.cache:/root/.elm -u $(id -u ${USER}):$(id -g ${USER}) infomark/elm:v2
