module Test1

import System.FFI

data MG_MGR : Type where

data MG_CONNECTION : Type where

data EV_DATA : Type where

data FN_DATA : Type where 

data MG_HTTP_SERVE_OPTS : Type where

----- MG HTTP
--data SIZE_T : Type where

MG_STR : Type
MG_STR
    = Struct "MG_STR"
            [("ptr", Ptr String),
             ("len", Int)]

MG_HTTP_HEADER : Type
MG_HTTP_HEADER
    = Struct "MG_HTTP_HEADER"
            [("name", MG_STR),
             ("value", MG_STR)]
{-
MG_HEADERS : Type
    = Struct "MG_HEADERS"
            [("h1",MG_HTTP_HEADER),
             ("h2",MG_HTTP_HEADER),
             ("h3",MG_HTTP_HEADER),
             ("h4",MG_HTTP_HEADER),
             ("h5",MG_HTTP_HEADER),
             ("h6",MG_HTTP_HEADER),
             ("h7",MG_HTTP_HEADER),
             ("h8",MG_HTTP_HEADER),
             ("h9",MG_HTTP_HEADER),
             ("h10",MG_HTTP_HEADER),
             ("h11",MG_HTTP_HEADER),
             ("h12",MG_HTTP_HEADER),
             ("h13",MG_HTTP_HEADER),
             ("h14",MG_HTTP_HEADER),
             ("h15",MG_HTTP_HEADER),
             ("h16",MG_HTTP_HEADER),
             ("h17",MG_HTTP_HEADER),
             ("h18",MG_HTTP_HEADER),
             ("h19",MG_HTTP_HEADER),
             ("h20",MG_HTTP_HEADER)]
-}
                          
MG_HTTP_MESSAGE : Type
MG_HTTP_MESSAGE
    = Struct "MG_HTTP_MESSAGE"
           [("method", MG_STR),
            ("uri", MG_STR),
            ("query", MG_STR),
            ("proto", MG_STR),
            ("h1",MG_HTTP_HEADER),
             ("h2",MG_HTTP_HEADER),
             ("h3",MG_HTTP_HEADER),
             ("h4",MG_HTTP_HEADER),
             ("h5",MG_HTTP_HEADER),
             ("h6",MG_HTTP_HEADER),
             ("h7",MG_HTTP_HEADER),
             ("h8",MG_HTTP_HEADER),
             ("h9",MG_HTTP_HEADER),
             ("h10",MG_HTTP_HEADER),
             ("h11",MG_HTTP_HEADER),
             ("h12",MG_HTTP_HEADER),
             ("h13",MG_HTTP_HEADER),
             ("h14",MG_HTTP_HEADER),
             ("h15",MG_HTTP_HEADER),
             ("h16",MG_HTTP_HEADER),
             ("h17",MG_HTTP_HEADER),
             ("h18",MG_HTTP_HEADER),
             ("h19",MG_HTTP_HEADER),
             ("h20",MG_HTTP_HEADER),
            ("body", MG_STR),
            ("message", MG_STR)]


%foreign "C:add,libmongoose"
add : Int -> Int -> Int

%foreign "C:get_len,libmongoose"
get_len : String -> Int


%foreign "C:test_sha256_abc,libsha256"
sha256 : String -> String

%foreign "C:get_and_malloc__mg_mgr,libmongoose"
prim__get_and_malloc__mg_mgr : PrimIO (Ptr MG_MGR)

get_and_malloc__mg_mgr : HasIO io => io (Ptr MG_MGR)
get_and_malloc__mg_mgr = primIO $ prim__get_and_malloc__mg_mgr


%foreign "C:is_null_mg_mgr,libmongoose"
is_null_mg_mgr : (Ptr MG_MGR) -> Int


%foreign "C:mg_mgr_init,libmongoose"
prim__mg_mgr_init : (Ptr MG_MGR) -> PrimIO ()

mg_mgr_init : HasIO io => (Ptr MG_MGR) -> io ()
mg_mgr_init p_mgr = primIO $ prim__mg_mgr_init p_mgr

%foreign "C:fn_http_handler,libmongoose"
prim__fn_http_handler : (Ptr MG_CONNECTION) -> Int -> (Ptr EV_DATA) -> (Ptr FN_DATA) -> PrimIO ()


%foreign "C:mg_http_listen,libmongoose"
prim__mg_http_listen : (Ptr MG_MGR) -> String -> ((Ptr MG_CONNECTION) -> Int -> (Ptr EV_DATA) -> (Ptr FN_DATA) -> PrimIO ()) -> (Ptr MG_MGR) -> PrimIO ()

mg_http_listen : HasIO io => (Ptr MG_MGR) -> String -> ((Ptr MG_CONNECTION) -> Int -> (Ptr EV_DATA) -> (Ptr FN_DATA) -> PrimIO ()) -> (Ptr MG_MGR) -> io ()
mg_http_listen p_mgr addr fn p_mgr2 = primIO $ prim__mg_http_listen p_mgr addr fn p_mgr2

%foreign "C:mg_mgr_free,libmongoose"
prim__mg_mgr_free : (Ptr MG_MGR) -> PrimIO ()

mg_mgr_free : HasIO io => (Ptr MG_MGR) -> io ()
mg_mgr_free p_mgr = primIO $ prim__mg_mgr_free p_mgr

%foreign "C:mg_mgr_poll,libmongoose"
prim__mg_mgr_poll : (Ptr MG_MGR) -> Int -> PrimIO ()

mg_mgr_poll : HasIO io => (Ptr MG_MGR) -> Int -> io ()
mg_mgr_poll p_mgr time_out =primIO $ prim__mg_mgr_poll p_mgr time_out


%foreign "C:get_and_malloc__mg_http_serve_opts,libmongoose"
prim__get_and_malloc__mg_http_serve_opts : String -> PrimIO (Ptr MG_HTTP_SERVE_OPTS) 

get_and_malloc__mg_http_serve_opts : HasIO io => String -> io (Ptr MG_HTTP_SERVE_OPTS)
get_and_malloc__mg_http_serve_opts root_dir = primIO $ prim__get_and_malloc__mg_http_serve_opts root_dir

%foreign "C:mg_http_serve_dir,libmongoose"
prim__mg_http_serve_dir : Ptr MG_CONNECTION -> Ptr MG_HTTP_MESSAGE -> Ptr MG_HTTP_SERVE_OPTS -> PrimIO ()

mg_http_serve_dir : HasIO io => Ptr MG_CONNECTION -> Ptr MG_HTTP_MESSAGE -> Ptr MG_HTTP_SERVE_OPTS -> io ()
mg_http_serve_dir p_conn p_ev p_opts = primIO ( prim__mg_http_serve_dir p_conn p_ev p_opts )

%foreign "C:ev_to_http_message,libmongoose"
ev_to_http_message : Ptr EV_DATA -> Ptr MG_HTTP_MESSAGE


x_my_http_handler : HasIO io => Ptr MG_CONNECTION -> Int -> Ptr EV_DATA -> Ptr FN_DATA -> io ()
x_my_http_handler p_conn ev p_ev p_fn = do
       p_opts <- (get_and_malloc__mg_http_serve_opts  "/home/jan/Desktop")
       
       if (ev==8) then mg_http_serve_dir p_conn (ev_to_http_message p_ev) p_opts else pure ()
          

       
my_http_handler : (Ptr MG_CONNECTION) -> Int -> (Ptr EV_DATA) -> (Ptr FN_DATA) -> PrimIO ()
my_http_handler p_conn ev p_ev p_fn = toPrim ( x_my_http_handler p_conn ev p_ev p_fn)

test1: String
test1="mufum"

partial
inf_loop : (Ptr MG_MGR) -> Int -> IO ()
inf_loop p_mgr time_out = do
  mg_mgr_poll p_mgr time_out
  inf_loop p_mgr time_out

main : IO ()
main = do

  printLn (add 4 7)
  printLn (sha256 "čau")
  printLn (get_len "č")
  p_mgr <- get_and_malloc__mg_mgr
  mg_mgr_init p_mgr 
  
  --mg_http_listen p_mgr "0.0.0.0:8000" prim__fn_http_handler p_mgr
  mg_http_listen p_mgr "0.0.0.0:8000" my_http_handler p_mgr
  
  inf_loop p_mgr 100
  mg_mgr_free p_mgr 
  
  printLn test1  
