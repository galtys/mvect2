module Test1

import Web.Mongoose.Types
import Web.Mongoose.FFI
import Crypto.Hash.SHA256


----- MG HTTP
--data SIZE_T : Type where











json_result : String
json_result = "{\"result\": 332}"

x_my_http_handler : HasIO io => Ptr MG_CONNECTION -> Int -> Ptr EV_DATA -> Ptr FN_DATA -> io ()
x_my_http_handler p_conn ev p_ev p_fn = do
       
       
       if (ev==8) then do
                  p_opts <- (get_and_malloc__mg_http_serve_opts  "/wd4T")
                  let hm = (ev_to_http_message p_ev)
                  if (mg_http_match_uri hm "/rest")==1 then
                       mg_http_reply p_conn 200 "Content-Type: application/json\r\n" json_result
                    else
                       pure ()
                  mg_http_serve_dir p_conn hm p_opts 
                  else pure ()
          

       
my_http_handler : (Ptr MG_CONNECTION) -> Int -> (Ptr EV_DATA) -> (Ptr FN_DATA) -> PrimIO ()
my_http_handler p_conn ev p_ev p_fn = toPrim ( x_my_http_handler p_conn ev p_ev p_fn)

%foreign "C:mg_ws_upgrade,libmongoose"
prim__mg_ws_upgrade : Ptr MG_CONNECTION -> Ptr MG_HTTP_MESSAGE -> Ptr String -> PrimIO ()

mg_ws_upgrade : HasIO io => Ptr MG_CONNECTION -> Ptr MG_HTTP_MESSAGE -> Ptr String -> io ()
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
