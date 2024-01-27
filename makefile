.PHONY: help
help: makefile
	@tail -n +4 makefile | grep ".PHONY"


.PHONY: build
build: public


# Install dependencies
node_modules: package.json package-lock.json
	npm install


content/resume/resume.json: content/resume/resume.yaml
	npx @adius/yaml2json < $< > $@


# Build page from markdown and template files
public: content static sass content/resume/resume.json
	zola build


# Serve website at http://127.0.0.1:1111
.PHONY: serve
serve:
	zola serve


.PHONY: deploy
deploy: public
	@echo "1. Open https://app.netlify.com/sites/adriansieber/deploys"
	@echo "2. Drag & drop the ./public directory"
	@echo "3. Promote any new posts on:\n\
		- twitter.com\n\
		- reddit.com\n\
		- news.ycombinator.com\n\
		- mailchimp.com\n\
		- hackaday.com\n\
		"


# Remove all build artifacts
.PHONY: clean
clean:
	-rm -r public
	-rm -r node_modules
	-rm content/resume/resume.json
