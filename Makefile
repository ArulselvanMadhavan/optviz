CURRENT_UID := $(shell id -u)
CURRENT_GID := $(shell id -g)
MODELS_DIR ?= $(shell pwd)
PORT ?= 9990
OCAML_COMPILER := 4.14.1

DC_RUN_VARS := USER_NAME=${USER} \
	CURRENT_UID=${CURRENT_UID} \
	CURRENT_GID=${CURRENT_GID} \
	OCAML_COMPILER=${OCAML_COMPILER} \
	MODELS_DIR=${MODELS_DIR} \
	PORT=${PORT}

DOCKER_COMPOSE_CMD := $(shell if docker compose version > /dev/null ; then echo "docker compose"; else echo "docker-compose"; fi)

all:
	@dune build @all

format:
	@dune build @fmt --auto-promote

WATCH ?= @all
watch:
	@dune build $(WATCH) -w

clean:
	@dune clean

run:
	@dune exec optviz

.PHONY: optviz
optviz:
	sudo ${DC_RUN_VARS} ${DOCKER_COMPOSE_CMD} -f docker-compose.yml run --service-ports optviz bash

.PHONY: diffusers-ocaml-rebuild
optviz-rebuild:
	sudo ${DC_RUN_VARS} ${DOCKER_COMPOSE_CMD} -f docker-compose.yml build

.PHONY: kill
kill:
	sudo docker kill $(shell docker ps -q)
