module Test1

import Web.Mongoose.Types
import Web.Mongoose.FFI
import Crypto.Hash.SHA256

import Category.Transaction.Hom

import System.FFI



json_result : String
json_result = "{\"result\": 332}"

WEB_ROOT : String
WEB_ROOT = "/home/jan/github.com/websocket-examples/jsClient"

x_my_http_handler : HasIO io => Ptr MG_CONNECTION -> MG_EVENT_TYPE -> Ptr EV_DATA -> Ptr FN_DATA -> io ()
x_my_http_handler p_conn MG_EV_HTTP_MSG p_ev p_fn = do
                    let hm = (ev_to_http_message p_ev)

                    if (mg_http_match_uri hm "/rest")==1 then
                           mg_http_reply p_conn 200 "Content-Type: application/json\r\n" json_result
                       else if (mg_http_match_uri hm "/websocket")==1 then do
                           mg_ws_upgrade p_conn p_ev get_pchar_NULL
                         else do
                             p_opts <- (get_and_malloc__mg_http_serve_opts  WEB_ROOT)
                             mg_http_serve_dir p_conn hm p_opts 
                    
x_my_http_handler p_conn MG_EV_WS_MSG p_ev p_fn = do
                    let p_wm = (ev_to_ws_message p_ev)
                    msg <- mg_ws_receive_as_String p_conn p_wm                 
                    mg_ws_send_text p_conn msg                    
x_my_http_handler p_conn ev p_ev p_fn = do 
                  pure ()




my_http_handler : (Ptr MG_CONNECTION) -> Int -> (Ptr EV_DATA) -> (Ptr FN_DATA) -> PrimIO ()
my_http_handler p_conn ev p_ev p_fn = toPrim ( x_my_http_handler p_conn (fromBits8 ev) p_ev p_fn)



test1: String
test1="mufum"

partial
inf_loop : (Ptr MG_MGR) -> Int -> IO ()
inf_loop p_mgr time_out = do
  mg_mgr_poll p_mgr time_out
  inf_loop p_mgr time_out

main : IO ()
main = do
  mg_log_set "3"
  printLn (sha256 "čau")
  printLn (get_len "č")
  p_mgr <- get_and_malloc__mg_mgr
  mg_mgr_init p_mgr 
  
  --mg_http_listen p_mgr "0.0.0.0:8000" prim__fn_http_handler p_mgr
  mg_http_listen p_mgr "0.0.0.0:8080" my_http_handler p_mgr
  
  inf_loop p_mgr 1000
  mg_mgr_free p_mgr 
