## ===============================================================================================================
## This makefile is here to centralize project commands.
## It does not actually 'make' files.
##
## A. Buchet - April 2025
## ===============================================================================================================

# Determine the operating system
OS_NAME := $(shell uname -s)

## Define root
SKILL_SHARP_ROOT := $(shell pwd)
export SKILL_SHARP_ROOT

SHELLSPEC_EXTRA_ARGS := $(shell echo "$$SHELLSPEC_EXTRA_ARGS")
export SHELLSPEC_EXTRA_ARGS

## =======================================================
## Help is actually parsing the current file to document
## available commands semi=automatically.
##
## It has to be the first target so it is run by default.
## =======================================================

## Find maximum target length
help: MAX_LEN := $(shell grep -Eo '^[a-zA-Z0-9_-]+:' $(MAKEFILE_LIST) | awk '{ if (length > max) max = length } END { print max }')

.PHONY: help
help: ## Display help and list available commands, but you probably figured that out...
	@echo "Available commands:"
	@echo
	@grep -E '^[a-zA-Z0-9_-]+:.*#' $(MAKEFILE_LIST) | sort | sed 's/:.*##* */:/g' | \
     env awk -F: '{printf "  \033[36m%-'$(MAX_LEN)'s\033[0m: %s\n", $$1, $$2}'
	@echo


## =======================================================
## centos7-skill Docker container
## =======================================================

.PHONY: container
container: ## Make sure centos7-skill Docker container is up and running
  ## From macOS
ifeq ($(OS_NAME),Darwin)
  ## Check that docker daemon is running, otherwise start it and wait until it answers
	@test -z "`bash -c 'docker version >/dev/null' 2>/dev/stdout`" ||                 \
    open /Applications/Docker.app                                &&                 \
    SECONDS=0                                                    &&                 \
    until test -z "`bash -c 'docker version >/dev/null' 2>/dev/stdout`" ; do        \
      sleep 2 ;                                                                     \
      (( SECONDS < 15 )) || exit 1 ;                                                \
      done
  ## Start docker only when necessary. xargs is necessary to trim whitespace and carriage returns.
	@test -n "`docker container ls -q -f name='centos7-skill' | xargs`" ||            \
    docker run -dit --rm                                                            \
      --platform linux/amd64                                                        \
      --network none                                                                \
      --volume ~/projects/skill#:/skill#                                            \
      --volume ~/Docker/centos7/Cadence/installs/IC618:/eda/cadence/installs/IC618  \
      --env    CDS_INST_DIR=/eda/cadence/installs/IC618                             \
      --name centos7-skill                                                          \
      centos7-skill
  ## Check if docker is running
  # test "$( docker container inspect -f '{{.State.Running}}' centos7-skill )" = "true"
endif

## =======================================================
## Format code
## =======================================================

.PHONY: format
format: ## Automatically format source code files.
## Remove whitespace
	grep -REl --exclude-dir='.git' '\s+$$' | xargs sed -e 's/\s\+$$'//g -i

## =======================================================
## Run tests
## =======================================================

.PHONY: test
test: ## Run shellspec tests
ifeq ($(OS_NAME),Linux)
	@bash -c 'test -n "$$CDS_INST_DIR" || { >&2 echo "Error: unable to locate Virtuoso, \$$CDS_INST_DIR" is undefined. ; exit 1  ; }'
	@env                                          \
    PATH="$$HOME/.local/bin:$$PATH"             \
    EXAMPLE="$(filter-out $@, $(MAKECMDGOALS))" \
    bash -c 'shellspec --shell /bin/bash --color --quick --fail-fast -fd $${EXAMPLE:+--example }$$EXAMPLE $$SHELLSPEC_EXTRA_ARGS'
else
	@make container
	@docker exec                   \
    --env "SHELLSPEC_EXTRA_ARGS" \
    --env "SKILL_SHARP_DEBUG"    \
    centos7-skill bash -c 'cd /skill# ; make test $(filter-out $@, $(MAKECMDGOALS))'
endif

.PHONY: debug
debug: ## Run last failed shellspec test in debug mode
	@env                                    \
    SKILL_SHARP_DEBUG=TRUE                \
    SHELLSPEC_EXTRA_ARGS="$$SHELLSPEC_EXTRA_ARGS --next-failure" \
    bash -c 'make test $(filter-out $@, $(MAKECMDGOALS))'

.PHONY: test_all
test_all: ## Run all shellspec tests
	@rm -f .shellspec-quick.log
	@env                                    \
    SHELLSPEC_EXTRA_ARGS="$$SHELLSPEC_EXTRA_ARGS --no-fail-fast --jobs 8" \
    bash -c 'make test $(filter-out $@, $(MAKECMDGOALS))'


## =======================================================
## Docker bash shell
## =======================================================

.PHONY: bash
bash: container
	@make container
	docker exec -it centos7-skill bash


## =======================================================
## SKILL# shell
## =======================================================

.PHONY: shell
shell: container ## Run SKILL interpreter in Shell mode.
ifeq ($(OS_NAME),Linux)
	@/skill#/bin/shell
  #@rlwrap docker exec -it centos7-skill /skill#/bin/shell
else
	@make container
	@rlwrap -a docker exec -it --env "SKILL_SHARP_DEBUG" centos7-skill /skill#/bin/shell
endif

.PHONY: shell_debug
shell_debug: container ## Run SKILL interpreter in Shell mode with debugging options enabled.
	@env "SKILL_SHARP_DEBUG=TRUE" make shell


## =======================================================
## Run sharp as an executable
## =======================================================

.PHONY: sharp
sharp: ## Run SKILL Sharp executable
ifeq ($(OS_NAME),Linux)
	@/skill#/bin/sharp $(filter-out $@, $(MAKECMDGOALS))
else
	@make container
	@docker exec                            \
    --env "SKILL_SHARP_DEBUG"             \
    --env "SKILL_SHARP_LINT_FILE_BY_FILE" \
    centos7-skill bash -c 'cd /skill# ; make sharp $(filter-out $@, $(MAKECMDGOALS))'
endif

.PHONY: sharp_debug
sharp_debug: ## Run SKILL Sharp executable
	@env "SKILL_SHARP_DEBUG=TRUE" make sharp $(filter-out $@, $(MAKECMDGOALS))


## =======================================================
## Filter any other target
## =======================================================

%:
	@:

