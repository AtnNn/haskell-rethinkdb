#!/usr/bin/env bash

required_instances () {
    cat <<EOF | sort
[a]
Datum
(a, b, c, d, e)
(a, b, c, d)
(a, b, c)
(a, b)
()
Bool
Char
Double
Float
Int
Integer
(Vector a)
SB.ByteString
LB.ByteString
ST.Text
LT.Text
(Map ST.Text a)
(Map [Char] a)
(HashMap ST.Text a)
(HashMap [Char] a)
Int8
Int16
Int32
Int64
Word
Word8
Word16
Word32
Word64
ZonedTime
UTCTime
Value
(Ratio Integer)
(Maybe a)
(Either a b)
(Set a)
LonLat
EOF

}

instances () {
    cmds="
:load Database.RethinkDB.NoClash
import Database.RethinkDB as R
import Data.Vector
import Data.Set
import Data.ByteString as SB
import Data.ByteString.Lazy as LB
import Data.Text as ST
import Data.Text.Lazy as LT
import Data.Map
import Data.HashMap.Strict
import Data.Int
import Data.Word
import Data.Time
import Data.Aeson
import Data.Ratio
:info $1
"
    cabal repl <<< "$cmds" \
        | awk '/^[^ ]/ { print "" }{ printf $0 }' \
        | grep ^instance \
        | sed 's/^instance *//; s/\[overlap ok\] *//; s/.*=> *//; s/ *--.*//' \
        | sed "s/$1 *//" \
        | sort
}

for class in Expr R.Result ToDatum FromDatum; do
    echo Missing $class instances:
    diff -u <(instances $class) <(required_instances) | grep '^\+[^+]' | sed 's/^+//'
done
