.PHONY: main
main: build


# Continously build website
.PHONY: watch
watch: stylus-watch docker-serve


# Continously build website
.PHONY: stylus-watch
stylus-watch: styl/screen.styl
	stylus \
		--watch \
		--include-css \
		--out \
		./css $<


# Serve website with docker
.PHONY: docker-serve
docker-serve:
	docker run \
		--interactive \
		--tty \
		--volume "$$PWD":/usr/src/app \
		--publish "4000:4000" \
		starefossen/github-pages


# Install dependencies
.PHONY: install
install:
	npm install
	# bundle install


# Build deployable files
.PHONY: build
build: build-resume css build-page


# Build page from markdown and template files
.PHONY: build-page
build-page:
	bundler exec jekyll build


# Build resume from resume.json
.PHONY: build-resume
build-resume: ./_resume/index.js
	node $<


# Build CSS files
css: styl/screen.styl
	stylus \
		--compress \
		--include-css \
		--out $@ \
		$<


# Serve files with local jekyll server
.PHONY: serve
serve:
	bundle exec jekyll serve --incremental


# Serve files including drafts
.PHONY: serve-drafts
serve-drafts:
	bundle exec jekyll serve \
		--trace \
		--strict_front_matter \
		--future \
		--drafts \
		--unpublished \
		--watch \
		--incremental \
		--livereload \
		--open-url


# Deploy website to surge.sh
.PHONY: deploy
deploy: build
	surge _site adriansieber.com


# Remove all build artifacts
.PHONY: clean
clean:
	-rm -r _site


# Upgrade bundler (ruby) dependencies
.PHONY: upgrade
upgrade:
	bundle update github-pages
