TAG := $(shell releases/version.sh)

docs/index.html:
	elm make --optimize --output="$@" src/Main.elm

releases/pixcell-$(TAG).zip: docs/index.html
	test -f $@ && rm $@ || true
	zip -j $@ $^

.PHONY: release
release: releases/pixcell-$(TAG).zip CHANGELOG.md
	gh release create $(TAG) -F CHANGELOG.md $<
