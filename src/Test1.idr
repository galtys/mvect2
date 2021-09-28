module Test1

import Web.Mongoose.Types
import Web.Mongoose.FFI
import Crypto.Hash.SHA256

import System.FFI
----- MG HTTP
--data SIZE_T : Type where


showHM : MG_HTTP_MESSAGE -> MG_STR
showHM hm = getField hm "uri"
         --let _method:MG_STR =  in
         --    _method
         --let _uri:MG_STR = getField hm "uri" 
         --method <- fromMG_STR _method
         --uri <- fromMG_STR _uri 
         

json_result : String
json_result = "{\"result\": 332}"

x_my_http_handler : HasIO io => Ptr MG_CONNECTION -> MG_EVENT_TYPE -> Ptr EV_DATA -> Ptr FN_DATA -> io ()
x_my_http_handler p_conn MG_EV_HTTP_MSG p_ev p_fn = do
                    p_opts <- (get_and_malloc__mg_http_serve_opts  "/wd4T")
                    let hm = (ev_to_http_message p_ev)
                    --printLn $ (parse_http_message hm)
                    if (mg_http_match_uri hm "/rest")==1 then
                         mg_http_reply p_conn 200 "Content-Type: application/json\r\n" json_result
                       else if (mg_http_match_uri hm "/websocket")==1 then
                         mg_ws_upgrade p_conn hm get_pchar_NULL
                         --pure ()
                         else
                            pure ()
                    mg_http_serve_dir p_conn hm p_opts 
                    
x_my_http_handler p_conn MG_EV_WS_MSG p_ev p_fn = do
                    let p_wm = (ev_to_ws_message p_ev)
                    ws_test_handler p_conn p_wm
                    pure ()
                    
x_my_http_handler p_conn _ p_ev p_fn = pure ()                    
       
my_http_handler : (Ptr MG_CONNECTION) -> Int -> (Ptr EV_DATA) -> (Ptr FN_DATA) -> PrimIO ()
my_http_handler p_conn ev p_ev p_fn = toPrim ( x_my_http_handler p_conn (fromBits8 ev) p_ev p_fn)

%foreign "C:mg_ws_upgrade,libmongoose"
prim__mg_ws_upgrade : Ptr MG_CONNECTION -> MG_HTTP_MESSAGE -> Ptr String -> PrimIO ()

mg_ws_upgrade : HasIO io => Ptr MG_CONNECTION -> MG_HTTP_MESSAGE -> Ptr String -> io ()
mg_ws_upgrade p_conn p_hm p_s = primIO (prim__mg_ws_upgrade p_conn p_hm p_s)

%foreign "C:get_size_t,libmongoose"
get_size_t : Int -> Bits64

%foreign "C:get_size_int,libmongoose"
get_size_int : Int -> Bits64

%foreign "C:get_MG_MAX_HTTP_HEADERS,libmongoose"
get_size_x : Bits64


test1: String
test1="mufum"

partial
inf_loop : (Ptr MG_MGR) -> Int -> IO ()
inf_loop p_mgr time_out = do
  mg_mgr_poll p_mgr time_out
  inf_loop p_mgr time_out

main : IO ()
main = do
  printLn (get_size_t 0)
  printLn (get_size_int 0)  
  printLn (get_size_x)    
  printLn (sha256 "čau")
  printLn (get_len "č")
  p_mgr <- get_and_malloc__mg_mgr
  mg_mgr_init p_mgr 
  
  --mg_http_listen p_mgr "0.0.0.0:8000" prim__fn_http_handler p_mgr
  mg_http_listen p_mgr "0.0.0.0:8001" my_http_handler p_mgr
  
  inf_loop p_mgr 100
  mg_mgr_free p_mgr 
  
  printLn test1  
