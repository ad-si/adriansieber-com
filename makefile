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


# Serve website at http://127.0.0.1:1111
.PHONY: serve
serve:
	zola serve


# Deploy website to surge.sh
.PHONY: deploy
deploy: public
	surge public adriansieber.com
	echo "Promote the new post on:"
	echo "- reddit.com"
	echo "- twitter.com"
	echo "- news.ycombinator.com"
	echo "- mailchimp.com"
	echo "- hackaday.com"


# Remove all build artifacts
.PHONY: clean
clean:
	-rm -r public
	-rm -r node_modules
	-rm content/resume/resume.json
