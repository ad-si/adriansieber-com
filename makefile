.PHONY: main
main: public


# Install dependencies
node_modules: package.json package-lock.json
	npm install


content/resume/resume.json: content/resume/resume.yaml
	npx @adius/yaml2json < $< > $@


# Build page from markdown and template files
public: content static sass content/resume/resume.json
	zola build
	-mv public/rss.xml public/atom.xml


# Deploy website to surge.sh
.PHONY: deploy
deploy: public
	surge public adriansieber.com


# Remove all build artifacts
.PHONY: clean
clean:
	-rm -r public
	-rm -r node_modules
	-rm content/resume/resume.json
