THIS_FILE := $(lastword $(MAKEFILE_LIST))

.DEFAULT_GOAL := help

build: ## Buils the project using Stack
	@stack build

test: ## Runs the HSpec tests
	@stack test

run: ## Runs the app using the input `resources/input.txt`
	@rm -f test/output.txt
	@time stack build --exec tetris-exe < resources/input.txt > resources/output.txt
	@echo "Output file was created: resources/output.txt"


.PHONY: help test run

help:
	@grep -E '^[a-zA-Z_-]+:.*?## .*$$' $(MAKEFILE_LIST) | sort | awk 'BEGIN {FS = ":.*?## "}; {printf "\033[36m%-30s\033[0m %s\n", $$1, $$2}'
