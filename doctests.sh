#!/bin/bash

set -o pipefail

cabal test 2>&1 | sed -r 's/^### Failure in //; s/<interactive>:[0-9]+:[0-9]+://'
