module Web.Mongoose.FFI

import Web.Mongoose.Types

||| String Utils

public export
%foreign "C:get_len,libmongoose"
get_len : String -> Int

||| Get String from MG_STR
%foreign "C:charFromMG_STR,libmongoose"
prim__charFromMG_STR : MG_STR -> PrimIO String

public export
fromMG_STR : HasIO io => MG_STR -> io String
fromMG_STR mg_str = primIO $ prim__charFromMG_STR mg_str


public export
%foreign "C:get_pchar_NULL,libmongoose"
get_pchar_NULL : Ptr String


||| MG MGR

{-
%foreign "C:mg_mgr,libmongoose"
prim__mg_mgr : Ptr String -> PrimIO MG_MGR

public export
mg_mgr : HasIO io => Ptr String -> io MG_MGR
mg_mgr s = primIO (prim__mg_mgr s)
-}

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

%foreign "C:mg_ws_upgrade,libmongoose"
prim__mg_ws_upgrade : Ptr MG_CONNECTION -> MG_HTTP_MESSAGE -> Ptr String -> PrimIO ()

public export
mg_ws_upgrade : HasIO io => Ptr MG_CONNECTION -> MG_HTTP_MESSAGE -> Ptr String -> io ()
mg_ws_upgrade p_conn hm p_fmt = primIO $ prim__mg_ws_upgrade p_conn hm p_fmt

public export
%foreign "C:ev_to_http_message,libmongoose"
ev_to_http_message : Ptr EV_DATA -> MG_HTTP_MESSAGE


%foreign "C:print_hm,libmongoose"
prim__print_hm : MG_HTTP_MESSAGE -> PrimIO ()

public export
print_hm : HasIO io => MG_HTTP_MESSAGE -> io ()
print_hm hm = primIO $ prim__print_hm hm

%foreign "C:get_hm_method,libmongoose"
get_hm_method : MG_HTTP_MESSAGE -> String

%foreign "C:get_hm_uri,libmongoose"
get_hm_uri : MG_HTTP_MESSAGE -> String

%foreign "C:get_hm_query,libmongoose"
get_hm_query : MG_HTTP_MESSAGE -> String

%foreign "C:get_hm_proto,libmongoose"
get_hm_proto : MG_HTTP_MESSAGE -> String

%foreign "C:get_hm_body,libmongoose"
get_hm_body : MG_HTTP_MESSAGE -> String

%foreign "C:get_hm_message,libmongoose"
get_hm_message : MG_HTTP_MESSAGE -> String

%foreign "C:get_hm_ith_header_name,libmongoose"
get_hm_ith_header_name : MG_HTTP_MESSAGE -> Int -> String

%foreign "C:get_hm_ith_header_value,libmongoose"
get_hm_ith_header_value : MG_HTTP_MESSAGE -> Int -> String

%foreign "C:get_MG_MAX_HTTP_HEADERS,libmongoose"
get_MG_MAX_HTTP_HEADERS : Int

parse_hm_headers : MG_HTTP_MESSAGE -> List (String,String)
parse_hm_headers hm =
      let h_i = [(get_hm_ith_header_name hm i, get_hm_ith_header_value hm i) | i<- [0..10] ] in
          h_i

public export
parse_http_message : MG_HTTP_MESSAGE -> MG_HTTP.Message
parse_http_message hm = 
      let method1 = get_hm_method hm
          uri1 = get_hm_uri hm
          query1 = get_hm_query hm
          proto1 = get_hm_proto hm
          h = parse_hm_headers hm
          body1 = get_hm_body hm
          message1 = "" in --get_hm_message hm in
          MG_HTTP.MkHM method1 uri1 query1 proto1 h body1 message1









||| HTTP
public export
%foreign "C:fn_http_handler,libmongoose"
prim__fn_http_handler : (Ptr MG_CONNECTION) -> Int -> (Ptr EV_DATA) -> (Ptr FN_DATA) -> PrimIO ()

%foreign "C:mg_http_serve_dir,libmongoose"
prim__mg_http_serve_dir : Ptr MG_CONNECTION -> MG_HTTP_MESSAGE -> Ptr MG_HTTP_SERVE_OPTS -> PrimIO ()

public export
%foreign "C:mg_http_match_uri,libmongoose"
mg_http_match_uri : MG_HTTP_MESSAGE -> String -> Int


%foreign "C:mg_http_reply,libmongoose"
prim__mg_http_reply : Ptr MG_CONNECTION -> Int -> String -> String -> PrimIO ()

public export
mg_http_reply : HasIO io => Ptr MG_CONNECTION -> Int -> String -> String -> io ()
mg_http_reply p_conn status_code headers body_fmt = primIO ( prim__mg_http_reply p_conn status_code headers body_fmt)

public export
mg_http_serve_dir : HasIO io => Ptr MG_CONNECTION -> MG_HTTP_MESSAGE -> Ptr MG_HTTP_SERVE_OPTS -> io ()
mg_http_serve_dir p_conn p_ev p_opts = primIO ( prim__mg_http_serve_dir p_conn p_ev p_opts )
