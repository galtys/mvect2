module Libc.Time

import System.FFI
import Generics.Derive
import JSON

%language ElabReflection

  -- https://www.tutorialspoint.com/c_standard_library/c_function_strftime.htm
  -- https://man7.org/linux/man-pages/man3/strptime.3.html
  
  {-                            
struct tm {
   int tm_sec;         /* seconds,  range 0 to 59          */
   int tm_min;         /* minutes, range 0 to 59           */
   int tm_hour;        /* hours, range 0 to 23             */
   int tm_mday;        /* day of the month, range 1 to 31  */
   int tm_mon;         /* month, range 0 to 11             */
   int tm_year;        /* The number of years since 1900   */
   int tm_wday;        /* day of the week, range 0 to 6    */
   int tm_yday;        /* day in the year, range 0 to 365  */
   int tm_isdst;       /* daylight saving time             */	
};  -}                            

namespace Libc
  export
  data ErrorCode = OK | ParsePartial | ParseError |OtherError
  %runElab derive "Libc.ErrorCode" [Generic, Meta, Eq, Ord, Show, EnumToJSON,EnumFromJSON]

  fromBits32 : Bits32 -> ErrorCode
  fromBits32 0 = OK
  fromBits32 1 = ParsePartial
  fromBits32 2 = ParseError
  fromBits32 _ = OtherError

  export
  TmInfo : Type
  TmInfo = Struct "TmInfo" [("tm_sec", Int), 
                              ("tm_min", Int),
                              ("tm_hour",Int),
                              ("tm_mday",Int),
                              ("tm_mon",Int),
                              ("tm_year",Int),
                              ("tm_wday",Int),
                              ("tm_yday",Int),
                              ("tm_isdst",Int)]
  export
  TmInfoEC : Type
  TmInfoEC = Struct "TmInfoEC" [("tm_info",TmInfo),
                                ("e_code",Bits32)]

  public export
  record DateTime where
    constructor MkDateTime
    tm_sec : Int
    tm_min : Int
    tm_hour : Int
    tm_mday : Int
    tm_mon : Int
    tm_year : Int
    tm_wday : Int
    tm_yday : Int
    tm_isdst : Int
  
  export
  toDateTime : Libc.TmInfo -> Libc.DateTime
  toDateTime x = (MkDateTime (getField x "tm_sec") (getField x "tm_min") (getField x "tm_hour")
                      (getField x "tm_mday") (getField x "tm_mon") (getField x "tm_year")
                      (getField x "tm_wday") (getField x "tm_yday") (getField x "tm_isdst"))
  
  
  %runElab derive "Libc.DateTime" [Generic, Meta, Eq, Ord, Show, RecordToJSON,RecordFromJSON]
                              
  public export
  %foreign "C:new_tm_info,libmongoose"
  prim__new_tm_info : PrimIO (Ptr Libc.TmInfo)
  
  public export
  %foreign "C:new_tm_info,libmongoose"
  prim__free_tm_info : (Ptr Libc.TmInfo) -> PrimIO ()
  
  public export
  %foreign "C:read_gmtime,libmongoose"
  prim__read_gmtime :  Int -> PrimIO Libc.TmInfo
    
  %foreign "C:read_time,libmongoose"
  prim__read_time : PrimIO Int
  
  %foreign "C:wrap_strftime,libmongoose"
  strftime : Int -> String -> Libc.TmInfo -> String
  
  %foreign "C:wrap_strptime,libmongoose"
  wrap_strptime : (buf:String) -> (format:String) -> Libc.TmInfoEC --TmInfo
  
  public export
  new_tm_info : HasIO io=> io (Ptr Libc.TmInfo)
  new_tm_info = primIO Libc.prim__new_tm_info
  
  public export
  free_tm_info : HasIO io=> (Ptr Libc.TmInfo) -> io ()
  free_tm_info p_tm = primIO $Libc.prim__free_tm_info p_tm
  
  public export
  read_tm : HasIO io=> (Int) -> io Libc.TmInfo
  read_tm  raw = primIO $Libc.prim__read_gmtime raw

  export
  time : HasIO io => io Int
  time = primIO Libc.prim__read_time
  
  export
  strptime : (buf:String)->(format:String) -> Either Libc.ErrorCode Libc.DateTime
  strptime x y = ret where
       w_ret : Libc.TmInfoEC
       w_ret = wrap_strptime x y
       ec : Libc.ErrorCode
       ec = fromBits32 $getField w_ret "e_code"
       tm : Libc.DateTime
       tm = toDateTime $ getField w_ret "tm_info" 
       ret : Either Libc.ErrorCode Libc.DateTime
       ret = case ec of
          OK => Right tm
          ParsePartial => Right tm
          err => Left err
          
  {-
  export
  strftime : HasIO io => Int -> String -> Libc.TmInfo -> io String
  strftime maxsize format tm = primIO $ Libc.prim__wrap_strftime maxsize format tm
  export
  strptime : HasIO io => (buf:String)->(format:String)-> io ( Libc.TmInfo)
  strptime buf format = primIO $Libc.prim__wrap_strptime buf format
  -}
  export
  test_libc_time : IO ()
  test_libc_time = do
    t<-Libc.time
    --printLn t
    --let --x:Libc.TmInfo4
    p_ti <- Libc.new_tm_info
    x <- Libc.read_tm t
    free_tm_info p_ti
    
    let u:DateTime
        u=toDateTime x 
    
    let ret= strftime 30 "%Y-%m-%d %H:%M:%S" x
    
    printLn ret
    
    let tx =strptime ret "%Y-%m-%d %H:%M:%S"     
    
    printLn tx
  
  
