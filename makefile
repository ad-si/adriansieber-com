.PHONY: help
help: makefile
	@tail -n +4 makefile | grep ".PHONY"


.PHONY: build
build: public


.PHONY: test
test: build


.PHONY: format
format:
	echo "TODO"


# Install dependencies
node_modules: package.json package-lock.json
	npm install


content/resume/resume.json: content/resume/resume.yaml
	npx @adius/yaml2json < $< > $@


# Build page from markdown and template files
public: content static sass content/resume/resume.json
	zola build


.PHONY: dev  # Serve website at http://127.0.0.1:1111
dev:
	zola serve


.PHONY: deploy
deploy: public
	netlify deploy --prod --dir public --site adriansieber
	@echo -e "Promote any new posts on:\n\
		- x.com\n\
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
