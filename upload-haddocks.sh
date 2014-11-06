#!/bin/bash

package=rethinkdb
version=`grep '^version: ' $package.cabal | cut -f 2 -d ' '`

echo -n 'Hackage user name: '
read -r user

cabal haddock --hoogle --hyperlink-source --html-location='/package/$pkg/docs' --contents-location='/package/$pkg'

cd dist/doc/html

cp -R $package $package-$version-docs

tar cvz --format=ustar -f $package-$version-docs.tar.gz $package-$version-docs

curl -X PUT -H Content-Type: application/x-tar -H Content-Encoding: gzip -u $user --data-binary @$package-$version-docs.tar.gz https://hackage.haskell.org/package/$package/docs
