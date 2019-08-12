#!/bin/bash
# Fetch the unicode database

VERSION=12.1.0
URL=ftp://www.unicode.org/Public/zipped/$VERSION/


wget -r -nd $URL
