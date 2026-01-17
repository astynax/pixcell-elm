TAG := $(shell grep -E -o -m 1 'v\d+\.\d+(\.\d+)?$$' CHANGELOG.md)

docs/index.html:
	elm make --optimize --output="$@" src/Main.elm

releases/pixcell-$(TAG).zip: docs/index.html releases/
	test -f $@ && rm $@ || true
	zip -j $@ $^

releases/:
	mkdir releases

.PHONY: release
release: releases/pixcell-$(TAG).zip CHANGELOG.md
	gh release create $(TAG) -F CHANGELOG.md $<
