# purescript-byte-codec

JavaScript's `ArrayBuffer` can't operate at the bit level, but it _can_ operate at the byte level.
This is a system for treating sub-byte codecs as unions of their values.
