.PHONY: test
test: video-small.mp4
	open $<


.PHONY: build
build: video.mp4
	open $<


video-small.mp4: generate-video.ts
	bun $< --small


video.mp4: generate-video.ts
	bun $<
