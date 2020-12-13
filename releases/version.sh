#!/bin/sh
grep -m 1 -P --color=never -o '(?<=# )v\d+\.\d+' "$(dirname $0)/../CHANGELOG.md"
