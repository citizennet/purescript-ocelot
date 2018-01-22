# Do not consider these actual files
.PHONY: clean build all

# Test backend, then build backend;
# Run backend, test frontend on it, then build frontend;
# Build frontend and move all files into dist location
all: \
	clean-install \
	build


#
# Main
#

# Use to clean components and docs and re-install
clean-install: clean-components clean-documentation install-components install-documentation

# Use to build everything without cleaning first
build: build-components build-documentation move-documentation


#
# Helpers
#

clean-components:
	cd ./halogen-components; \
	rm -rf bower_components; \
	rm -rf node_modules; \
	rm -rf output; \
	echo "Components directory cleaned.\n"

clean-documentation:
	cd ./style-guide
	rm -rf bower_components; \
	rm -rf node_modules; \
	rm -rf output; \
	echo "Documentation directory cleaned.\n"

install-components:
	cd ./halogen-components; \
	bower i --silent && npm i --silent; \
	echo "Installation complete in components directory.\n"

install-documentation:
	cd ./style-guide; \
	bower i --silent && npm i --silent; \
	echo "Installation complete in documentation directory.\n"

build-components:
	cd ./halogen-components; \
	npm run build; \
	echo "Components directory built successfully.\n"

build-documentation:
	cd ./style-guide; \
	npm run build; \
	echo "Documentation directory built successfully.\n"

move-documentation:
	cp -r ./style-guide/dist docs/; \
	echo "Dist folder copied to docs./n"
