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


# Deploy website to surge.sh
.PHONY: deploy
deploy: public
	npx surge public adriansieber.com

	@printf "\
	Promote the new post on: \n\
	- reddit.com \n\
	- twitter.com \n\
	- news.ycombinator.com \n\
	- mailchimp.com \n\
	- hackaday.com \n\
	"


# Remove all build artifacts
.PHONY: clean
clean:
	-rm -r public
	-rm -r node_modules
	-rm content/resume/resume.json
