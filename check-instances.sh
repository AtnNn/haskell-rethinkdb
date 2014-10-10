#!/usr/bin/env bash

required_instances () {
    cat <<EOF | sort
[a]
[Char]
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
Data.Vector.Vector a
Data.ByteString.ByteString
Data.ByteString.Lazy.ByteString
Data.Text.Text
Data.Text.Lazy.Text
Data.Map.Map k v
Data.HashMap.Strict.HashMap k v
Data.Int.Int8
Data.Int.Int16
Data.Int.Int32
Data.Int.Int64
Data.Word.Word
Data.Word.Word8
Data.Word.Word16
Data.Word.Word32
Data.Word.Word64
Data.Time.ZonedTime
Data.Time.UTCTime
Data.Aeson.Value
Ratio Integer
Maybe a
Data.Set.Set

EOF

}

instances () {
    cmds="
:info $1
"
    cabal repl <<< "$cmds" \
        | awk '/^[^ ]/ { print "" }{ printf $0 }' \
        | grep ^instance \
        | sed 's/^instance *//; s/\[overlap ok\] *//; s/.*=> *//; s/ *--.*//' \
        | sed "s/$1 *//" \
        | sort
}

for class in Expr Result ToDatum FromDatum; do
    echo Missing $class instances:
    diff -u <(instances $class) <(required_instances) | grep '^\+[^+]' | sed 's/^+//'
done
