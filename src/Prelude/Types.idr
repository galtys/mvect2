module Prelude.Types

%default total


-- This works quickly because when string-concat builds the result, it allocates
-- enough room in advance so there's only one allocation, rather than lots!
--
-- Like fastUnpack, this function won't reduce at compile time.
-- If you need to concatenate strings at compile time, use Prelude.concat.
%foreign
  "pygen:fastConcat"
export
fastConcat : List String -> String

%foreign
    "pygen:fastPack"
export
fastPack : List Char -> String


-- This function runs fast when compiled but won't compute at compile time.
-- If you need to unpack strings at compile time, use Prelude.unpack.
%foreign
  "pygen:fastUnpack"
export
fastUnpack : String -> List Char

