# Vairables that might need to be overriden.
ROOT_DIR ?= $(shell pwd)
BUILD_DIR ?= $(ROOT_DIR)/.build
DIST_DIR ?= $(ROOT_DIR)/dist
OUTPUT_DIR ?= $(ROOT_DIR)/output
PARCEL_DIR ?= $(BUILD_DIR)/parcel
RTS_ARGS ?=
SRC_DIR ?= $(ROOT_DIR)/src
UI_GUIDE_DIR ?= $(ROOT_DIR)/ui-guide

# Variables that we control
CLEAN_DEPS :=
BUILD_DEPS := build-ui
DEPS := $(BUILD_DIR)/.deps
FIND_SRC_FILES_ARGS := \( -name '*.purs' -o -name '*.js' \) -type f
NODE_MODULES := $(ROOT_DIR)/node_modules/.stamp
PACKAGE_JSON := $(ROOT_DIR)/package.json
SRC_FILES := $(shell find $(SRC_DIR) $(FIND_SRC_FILES_ARGS))
UI_GUIDE_FILES := $(shell find $(UI_GUIDE_DIR) $(FIND_SRC_FILES_ARGS))
YARN_LOCK := $(ROOT_DIR)/yarn.lock

YARN := cd $(ROOT_DIR) && yarn

-include $(ROOT_DIR)/css/Makefile

.DEFAULT_GOAL := build

$(BUILD_DIR) $(DIST_DIR) $(PARCEL_DIR):
	mkdir -p $@

$(DEPS): packages.dhall spago.dhall $(NODE_MODULES) | $(BUILD_DIR)
	$(YARN) run spago install $(RTS_ARGS)
	touch $@

$(DIST_DIR)/bundled.js: $(OUTPUT_DIR)/Main/index.js
	$(YARN) run purs bundle $(OUTPUT_DIR)/*/*.js \
		--main Main \
		--module Main \
		--output $@ \
		$(RTS_ARGS)

$(DIST_DIR)/index.js: $(OUTPUT_DIR)/Main/index.js
	$(YARN) run browserify dist/main.js --outfile $@

$(NODE_MODULES): $(PACKAGE_JSON) $(YARN_LOCK)
	$(YARN) install
	touch $@

$(OUTPUT_DIR)/Main/index.js: $(SRC_FILES) $(UI_GUIDE_FILES) $(DEPS)
	$(YARN) run spago build -p "$(UI_GUIDE_DIR)/**/*.purs" -u "$(RTS_ARGS)"

.PHONY: build
build: $(BUILD_DEPS)

.PHONY: build-ui
build-ui: $(DIST_DIR)/index.js

.PHONY: clean
clean: $(CLEAN_DEPS)
	rm -fr \
		$(BUILD_DIR) \
		$(DIST_DIR)/bundled.js \
		$(DIST_DIR)/index.js \
		$(OUTPUT_DIR) \
		$(ROOT_DIR)/.spago \
		$(ROOT_DIR)/node_modules

.PHONY: ui-guide
ui-guide: build-css $(OUTPUT_DIR)/Main/index.js $(NODE_MODULES) | $(PARCEL_DIR)
	npx parcel $(DIST_DIR)/parcel.html --out-dir $(PARCEL_DIR) --no-cache
