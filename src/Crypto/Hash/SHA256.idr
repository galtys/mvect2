module Crypto.Hash.SHA256

%foreign "C:test_sha256_abc,libsha256"
         "javascript:lambda:x=>sha256(x)"
prim__sha256 : String -> String

public export
H256 : Type
H256 = String

public export
sha256 : String -> H256
sha256 x = prim__sha256 x

