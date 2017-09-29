# Continously build website
watch: stylus-watch docker-serve


# Continously build website
stylus-watch: styl/screen.styl
	stylus \
		--watch \
		--include-css \
		--out \
		./css $<


# Serve website with docker
docker-serve:
	docker run \
		--interactive \
		--tty \
		--volume "$$PWD":/usr/src/app \
		--publish "4000:4000" \
		starefossen/github-pages


# Install dependencies
install:
	npm install
	# bundle install


# Build deployable files
build: build-resume css


# Build resume from resume.json
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
serve:
	bundle exec jekyll serve


# Serve files including drafts
serve-drafts:
	bundle exec jekyll serve --trace --drafts


# Remove all build artifacts
clean:
	-rm -r _site


.PHONY: watch stylus-watch docker-serve install \
	build build-resume serve serve-drafts
