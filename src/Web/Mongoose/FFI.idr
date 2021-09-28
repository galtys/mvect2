module Web.Mongoose.FFI

import Web.Mongoose.Types

||| String Utils

public export
%foreign "C:get_len,libmongoose"
get_len : String -> Int


||| MG MGR

%foreign "C:get_and_malloc__mg_mgr,libmongoose"
prim__get_and_malloc__mg_mgr : PrimIO (Ptr MG_MGR)

public export
get_and_malloc__mg_mgr : HasIO io => io (Ptr MG_MGR)
get_and_malloc__mg_mgr = primIO $ prim__get_and_malloc__mg_mgr

public export
%foreign "C:is_null_mg_mgr,libmongoose"
is_null_mg_mgr : (Ptr MG_MGR) -> Int

%foreign "C:mg_mgr_init,libmongoose"
prim__mg_mgr_init : (Ptr MG_MGR) -> PrimIO ()

public export
mg_mgr_init : HasIO io => (Ptr MG_MGR) -> io ()
mg_mgr_init p_mgr = primIO $ prim__mg_mgr_init p_mgr

%foreign "C:mg_mgr_free,libmongoose"
prim__mg_mgr_free : (Ptr MG_MGR) -> PrimIO ()

public export
mg_mgr_free : HasIO io => (Ptr MG_MGR) -> io ()
mg_mgr_free p_mgr = primIO $ prim__mg_mgr_free p_mgr

%foreign "C:mg_mgr_poll,libmongoose"
prim__mg_mgr_poll : (Ptr MG_MGR) -> Int -> PrimIO ()

public export
mg_mgr_poll : HasIO io => (Ptr MG_MGR) -> Int -> io ()
mg_mgr_poll p_mgr time_out =primIO $ prim__mg_mgr_poll p_mgr time_out


||| MG Connection

%foreign "C:mg_http_listen,libmongoose"
prim__mg_http_listen : (Ptr MG_MGR) -> String -> ((Ptr MG_CONNECTION) -> Int -> (Ptr EV_DATA) -> (Ptr FN_DATA) -> PrimIO ()) -> (Ptr MG_MGR) -> PrimIO ()

public export
mg_http_listen : HasIO io => (Ptr MG_MGR) -> String -> ((Ptr MG_CONNECTION) -> Int -> (Ptr EV_DATA) -> (Ptr FN_DATA) -> PrimIO ()) -> (Ptr MG_MGR) -> io ()
mg_http_listen p_mgr addr fn p_mgr2 = primIO $ prim__mg_http_listen p_mgr addr fn p_mgr2


||| MG HTTP MESSAGE

%foreign "C:get_and_malloc__mg_http_serve_opts,libmongoose"
prim__get_and_malloc__mg_http_serve_opts : String -> PrimIO (Ptr MG_HTTP_SERVE_OPTS) 

public export
get_and_malloc__mg_http_serve_opts : HasIO io => String -> io (Ptr MG_HTTP_SERVE_OPTS)
get_and_malloc__mg_http_serve_opts root_dir = primIO $ prim__get_and_malloc__mg_http_serve_opts root_dir

public export
%foreign "C:ev_to_http_message,libmongoose"
ev_to_http_message : Ptr EV_DATA -> Ptr MG_HTTP_MESSAGE

||| HTTP
public export
%foreign "C:fn_http_handler,libmongoose"
prim__fn_http_handler : (Ptr MG_CONNECTION) -> Int -> (Ptr EV_DATA) -> (Ptr FN_DATA) -> PrimIO ()

%foreign "C:mg_http_serve_dir,libmongoose"
prim__mg_http_serve_dir : Ptr MG_CONNECTION -> Ptr MG_HTTP_MESSAGE -> Ptr MG_HTTP_SERVE_OPTS -> PrimIO ()

public export
%foreign "C:mg_http_match_uri,libmongoose"
mg_http_match_uri : Ptr MG_HTTP_MESSAGE -> String -> Int


%foreign "C:mg_http_reply,libmongoose"
prim__mg_http_reply : Ptr MG_CONNECTION -> Int -> String -> String -> PrimIO ()

public export
mg_http_reply : HasIO io => Ptr MG_CONNECTION -> Int -> String -> String -> io ()
mg_http_reply p_conn status_code headers body_fmt = primIO ( prim__mg_http_reply p_conn status_code headers body_fmt)

public export
mg_http_serve_dir : HasIO io => Ptr MG_CONNECTION -> Ptr MG_HTTP_MESSAGE -> Ptr MG_HTTP_SERVE_OPTS -> io ()
mg_http_serve_dir p_conn p_ev p_opts = primIO ( prim__mg_http_serve_dir p_conn p_ev p_opts )
