#!/bin/bash

cabal update
cabal sandbox init
cabal configure
cabal-bounds drop --upper --library rethinkdb.cabal
cabal install --only-dependencies
cabal-bounds update --upper --library rethinkdb.cabal dist/setup-config
