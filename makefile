.PHONY: main
main: _site


# Install dependencies
node_modules: package.json package-lock.json
	npm install
	# bundle install


# Build page from markdown and template files
_site: index.html resume.html _data/resume.yaml css/screen.css
	bundler exec jekyll build


# Build CSS files
css/screen.css: styl/* | css
	npx stylus \
		--compress \
		--include-css \
		--out css/screen.css \
		styl/screen.styl

# Build CSS directory
css:
	mkdir -p $@



# Continously build website
.PHONY: watch
watch: stylus-watch docker-serve


# Continously build website
.PHONY: stylus-watch | css
stylus-watch: styl/screen.styl
	npx stylus \
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
deploy: _site
	surge _site adriansieber.com


# Remove all build artifacts
.PHONY: clean
clean:
	-rm -r _site css


# Upgrade bundler (ruby) dependencies
.PHONY: upgrade
upgrade:
	bundle update github-pages
