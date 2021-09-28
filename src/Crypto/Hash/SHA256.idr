module Crypto.Hash.SHA256

public export
%foreign "C:test_sha256_abc,libsha256"
sha256 : String -> String
