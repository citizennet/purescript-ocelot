# Variables that might need to be overriden.
ROOT_DIR ?= $(shell cd .. && pwd)
CSS_DIR ?= $(ROOT_DIR)/css
DIST_DIR ?= $(ROOT_DIR)/dist

# Variables that we control.
CSS_NODE_MODULES := $(CSS_DIR)/node_modules/.stamp
CSS_PACKAGE_JSON := $(CSS_DIR)/package.json
CSS_YARN_LOCK := $(CSS_DIR)/yarn.lock
DIST_CSS_FILENAME := cn-tailwind
DIST_CSS_DEV_GLOBAL := $(DIST_DIR)/$(DIST_CSS_FILENAME).css
DIST_CSS_DEV_SCOPED := $(DIST_DIR)/$(DIST_CSS_FILENAME).scoped.css
DIST_CSS_PROD_GLOBAL := $(DIST_DIR)/$(DIST_CSS_FILENAME).min.css
DIST_CSS_PROD_SCOPED := $(DIST_DIR)/$(DIST_CSS_FILENAME).scoped.min.css

CSS_YARN := cd $(CSS_DIR) && yarn

.DEFAULT_GOAL := build-css

# Variables we add to

BUILD_DEPS += build-css
CLEAN_DEPS += clean-css

$(CSS_NODE_MODULES): $(CSS_PACKAGE_JSON) $(CSS_YARN_LOCK)
	$(CSS_YARN) install
	touch $@

$(DIST_CSS_DEV_GLOBAL): $(CSS_NODE_MODULES) | $(DIST_DIR)
	$(CSS_YARN) run webpack

$(DIST_CSS_DEV_SCOPED): $(CSS_NODE_MODULES) | $(DIST_DIR)
	$(CSS_YARN) run webpack --env=scoped

$(DIST_CSS_PROD_GLOBAL): $(CSS_NODE_MODULES) | $(DIST_DIR)
	$(CSS_YARN) run webpack --env=prod

$(DIST_CSS_PROD_SCOPED): $(CSS_NODE_MODULES) | $(DIST_DIR)
	$(CSS_YARN) run webpack --env=prod,scoped

$(DIST_DIR):
	mkdir -p $@

.PHONY: build-css
build-css: build-css-dev build-css-prod ## Build all the CSS, installing any missing dependencies along the way

.PHONY: build-css-dev
build-css-dev: build-css-dev-global build-css-dev-scoped ## Build all un-minified CSS — both global and scoped

.PHONY: build-css-dev-global
build-css-dev-global: $(DIST_CSS_DEV_GLOBAL) ## Build un-minified CSS

.PHONY: build-css-dev-scoped
build-css-dev-scoped: $(DIST_CSS_DEV_SCOPED) ## Build un-minified CSS, with all rules scoped to `.ocelot-scoped` selector

.PHONY: build-css-prod ## Build all minified CSS — both global and scoped
build-css-prod: build-css-prod-global build-css-prod-scoped

.PHONY: build-css-prod-global
build-css-prod-global: $(DIST_CSS_PROD_GLOBAL) ## Build minified CSS

.PHONY: build-css-prod-scoped
build-css-prod-scoped: $(DIST_CSS_PROD_SCOPED) ## Build minified CSS, with all rules scoped to `.ocelot-scoped` selector

.PHONY: clean-css
clean-css: ## Remove all CSS dependencies and build artifacts, starting over with a clean slate
	rm -fr \
		$(CSS_DIR)/node_modules \
		$(DIST_CSS_DEV_GLOBAL) \
		$(DIST_CSS_DEV_SCOPED) \
		$(DIST_CSS_PROD_GLOBAL) \
		$(DIST_CSS_PROD_SCOPED)