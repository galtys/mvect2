module WS_Client

import Web.Mongoose.Types
import Web.Mongoose.FFI
import Crypto.Hash.SHA256

import System.FFI

x_my_http_handler : HasIO io => Ptr MG_CONNECTION -> MG_EVENT_TYPE -> Ptr EV_DATA -> Ptr FN_DATA -> io ()
x_my_http_handler p_conn MG_EV_ERROR p_ev p_fn = pure ()
x_my_http_handler p_conn MG_EV_WS_OPEN p_ev p_fn = do
                    mg_ws_send_text p_conn "HELLO"
{-                    
x_my_http_handler p_conn MG_EV_WS_MSG p_ev p_fn = do
                    let hm = (ev_to_http_message p_ev)

                    if (mg_http_match_uri hm "/rest")==1 then
                           mg_http_reply p_conn 200 "Content-Type: application/json\r\n" json_result
                       else if (mg_http_match_uri hm "/websocket")==1 then do
                           mg_ws_upgrade p_conn p_ev get_pchar_NULL
                         else do
                             p_opts <- (get_and_malloc__mg_http_serve_opts  "/wd4T")
                             mg_http_serve_dir p_conn hm p_opts 
-}                    
x_my_http_handler p_conn MG_EV_WS_MSG p_ev p_fn = do
                    let p_wm = (ev_to_ws_message p_ev)                    
                    msg <- mg_ws_receive_as_String p_conn p_wm                 
                    printLn "Received: "
                    printLn msg
                    printLn "\n"
                    set_is_closing p_conn
                    
x_my_http_handler p_conn ev p_ev p_fn = do 
                  pure ()


my_http_handler : (Ptr MG_CONNECTION) -> Int -> (Ptr EV_DATA) -> (Ptr FN_DATA) -> PrimIO ()
my_http_handler p_conn ev p_ev p_fn = toPrim ( x_my_http_handler p_conn (fromBits8 ev) p_ev p_fn)

partial
inf_loop : (Ptr MG_MGR) -> (Ptr MG_CONNECTION) -> Int -> IO ()
inf_loop p_mgr p_conn time_out = do
  printLn $ get_is_closing p_conn
  mg_mgr_poll p_mgr time_out
  
  if (get_is_closing p_conn) /= 0 then 
     pure ()
     else
        inf_loop p_mgr p_conn time_out

s_url : String
s_url = "ws://localhost:8000/websocket"

main : IO ()
main = do
  mg_log_set "3"
  
  p_mgr <- get_and_malloc__mg_mgr
  mg_mgr_init p_mgr 
  
  c <- mg_ws_connect p_mgr s_url my_http_handler get_p_fndata_NULL ""
  
  --mg_http_listen p_mgr "0.0.0.0:8000" prim__fn_http_handler p_mgr
  --mg_http_listen p_mgr "0.0.0.0:8000" my_http_handler p_mgr
  
  inf_loop p_mgr c 1000
  
  mg_mgr_free p_mgr 
