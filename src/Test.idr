module Test

test:String
test="hello idris2"

%foreign "javascript:lambda: x => console.log(x)"
prim__consoleLog : String -> PrimIO ()

consoleLog : HasIO io => String -> io ()
consoleLog x = primIO $ prim__consoleLog x

main : IO ()
main = consoleLog test
