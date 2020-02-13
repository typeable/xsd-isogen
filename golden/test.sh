#!/bin/bash

for file in *.xsd; do
	cabal run -v0 xsd-isogen -- -i $file > $file.hs
done
