module Libc.Time

import System.FFI
import Generics.Derive
import JSON
import Libc.DateTypes

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
};
-} 

namespace Libc

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
  {-
  public export
  record YearDateTime where
    constructor MkYearDateTime
    tm_sec : Int
    tm_min : Int
    tm_hour : Int
    tm_mday : Int
    tm_mon : Int
    tm_year : Int
    tm_wday : Int
    tm_yday : Int
    tm_isdst : Int
  %runElab derive "DateTimeTypes.YearDateTime" [Generic, Meta, Eq, Ord, Show, RecordToJSON,RecordFromJSON]
  
  public export
  record YearDate where
    constructor MkYearDate
    tm_mday : Int
    tm_mon : Int
    tm_year : Int
  %runElab derive "DateTimeTypes.YearDate" [Generic, Meta, Eq, Ord, Show, RecordToJSON,RecordFromJSON]
  
  public export
  record YearMonth where
    constructor MkYearMonth
    tm_mon : Int
    tm_year : Int
  %runElab derive "DateTimeTypes.YearMonth" [Generic, Meta, Eq, Ord, Show, RecordToJSON,RecordFromJSON]
  -}
  
  {-
  public export
  record YearWeek where
    constructor MkYearWeek
    tm_week : Int
    tm_year : Int
  %runElab derive "DateTimeTypes.YearWeek" [Generic, Meta, Eq, Ord, Show, RecordToJSON,RecordFromJSON]
-}
  
  {-                              
  public export
  %foreign "C:new_tm_info,libmongoose"
  prim__new_tm_info : PrimIO (Ptr DateTimeTypes.TmInfo)
  
  public export
  %foreign "C:free_tm_info,libmongoose"
  prim__free_tm_info : (Ptr DateTimeTypes.TmInfo) -> PrimIO ()
  -}
  
  
  
  public export
  %foreign "C:read_gmtime,libmongoose"
  prim__read_gmtime_io :  Int -> PrimIO Libc.TmInfo
  
  public export
  %foreign "C:difftime,libc"
  prim__difftime : Int -> Int -> Double
  
  public export
  %foreign "C:wrap_mktime,libmongoose"
  prim_mktime : Int->Int->Int->Int->Int->Int->Int->Int->Int-> Int
  
  public export
  %foreign "C:wrap_strftime2,libmongoose"
  prim__strftime2 : Int -> String -> Int->Int->Int->Int->Int->Int->Int->Int->Int-> String
  {-
  
  public export
  %foreign "C:read_gmtime,libmongoose"
  prim__read_gmtime :  Int -> Libc.TmInfo
    -}
  %foreign "C:read_time,libmongoose"
  prim__read_time : PrimIO Int
  
  %foreign "C:wrap_strftime,libmongoose"
  strftime : Int -> String -> Libc.TmInfo -> String
  
  %foreign "C:wrap_strptime,libmongoose"
  wrap_strptime : (buf:String) -> (format:String) -> Libc.TmInfoEC --TmInfo
  {-
  public export
  new_tm_info : HasIO io=> io (Ptr Libc.TmInfo)
  new_tm_info = primIO Libc.prim__new_tm_info
  
  public export
  free_tm_info : HasIO io=> (Ptr Libc.TmInfo) -> io ()
  free_tm_info p_tm = primIO $Libc.prim__free_tm_info p_tm
  -}
  public export
  read_gmtime : HasIO io=> (Int) -> io Libc.TmInfo
  read_gmtime  raw = primIO $Libc.prim__read_gmtime_io raw

  export
  time : HasIO io => io Int
  time = primIO Libc.prim__read_time
  
  export
  mktime : DateTimeTypes.YearDateTime -> Int
  mktime (MkYearDateTime tm_sec tm_min tm_hour tm_mday tm_mon tm_year tm_wday tm_yday tm_isdst) = 
        prim_mktime tm_sec tm_min tm_hour tm_mday tm_mon tm_year tm_wday tm_yday tm_isdst
  
  export
  strftime2 : Int -> String -> DateTimeTypes.YearDateTime -> String 
  strftime2 sz f (MkYearDateTime tm_sec tm_min tm_hour tm_mday tm_mon tm_year tm_wday tm_yday tm_isdst) =
        prim__strftime2 sz f tm_sec tm_min tm_hour tm_mday tm_mon tm_year tm_wday tm_yday tm_isdst
  
  export
  toYearDateTime : Libc.TmInfo -> DateTimeTypes.YearDateTime
  toYearDateTime x = (MkYearDateTime (getField x "tm_sec") (getField x "tm_min") (getField x "tm_hour")
                      (getField x "tm_mday") (getField x "tm_mon") (getField x "tm_year")
                      (getField x "tm_wday") (getField x "tm_yday") (getField x "tm_isdst"))
  
  export
  strptime : (buf:String)->(format:String) -> Either DateTimeTypes.ErrorCode DateTimeTypes.YearDateTime
  strptime x y = ret where
       w_ret : Libc.TmInfoEC
       w_ret = wrap_strptime x y
       ec : DateTimeTypes.ErrorCode
       ec = fromBits32 $getField w_ret "e_code"
       tm : DateTimeTypes.YearDateTime
       tm = toYearDateTime $ getField w_ret "tm_info" 
       ret : Either DateTimeTypes.ErrorCode DateTimeTypes.YearDateTime
       ret = case ec of
          OK => Right tm
          ParsePartial => Right tm
          err => Left err
          
  export
  fromDateTime : DateTime -> YearDateTime
  fromDateTime (YDT x) = x
  fromDateTime (YD (MkYearDate tm_mday tm_mon tm_year)) = (MkYearDateTime 0 0 0 tm_mday tm_mon tm_year 0 0 0)
  fromDateTime (YM (MkYearMonth tm_mon tm_year)) = (MkYearDateTime 0 0 0 0 tm_mon tm_year 0 0 0)
  fromDateTime (Err x) = errorYearDateTime --(MkYearDateTime 0 0 0 0 0 0 0 0 0)
  fromDateTime (RAW x) = errorYearDateTime --toYearDateTime $ prim__read_gmtime x 
  
  export
  fromYearDateTime : DateFormatCode -> YearDateTime -> DateTime
  fromYearDateTime YDTF x = YDT x
  fromYearDateTime YDF (MkYearDateTime tm_sec tm_min tm_hour tm_mday tm_mon tm_year tm_wday tm_yday tm_isdst) = YD (MkYearDate tm_mday tm_mon tm_year)
  fromYearDateTime YMF (MkYearDateTime tm_sec tm_min tm_hour tm_mday tm_mon tm_year tm_wday tm_yday tm_isdst) = YM (MkYearMonth tm_mon tm_year)
  fromYearDateTime RAWF x = RAW (mktime x)
  
  export
  rf : DateTime -> DateFormatCode -> DateTime
  rf x c = (fromYearDateTime c ) (fromDateTime x)

  
  export
  fromOdooDateTime : String -> DateTime
  fromOdooDateTime buf = 
        case strptime buf DEFAULT_SERVER_DATETIME_FORMAT of
           Left er => Err er
           Right x => YDT x
  export         
  fromOdooDate : String -> DateTime
  fromOdooDate buf = 
        case strptime buf DEFAULT_SERVER_DATE_FORMAT of
           Left er => Err er
           Right x => rf (YDT x) YDF
  export
  FromString DateTime where
    fromString = fromOdooDate
    
  export
  Cast String DateTime where
     cast = fromOdooDate
    
  export
  Cast DateTime String where
     cast x = strftime2 100 DEFAULT_SERVER_DATE_FORMAT (fromDateTime x)
    
  dateTest : DateTime
  dateTest = "2021-12-23"  
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
    
    printLn t
    --let s = mktime $ toYearDateTime x
    --printLn (prim__difftime s t)    
    
    x <- read_gmtime t --prim__read_gmtime_io t --toYearDateTime $        
    printLn $toYearDateTime x
    
    let ret= strftime 30 "%Y-%m-%d %H:%M:%S" x        
    printLn ret
    
    let ocas = MkYearDateTime { tm_sec = 0, tm_min = 0, tm_hour = 1, tm_mday = 1, tm_mon = 0, tm_year = 121, tm_wday = 0, tm_yday = 0, tm_isdst = 0 }
    let t2 =  mktime ocas
    
    --let x2= prim__read_gmtime t2 --toYearDateTime $    
    x2 <- read_gmtime t2
    
    let ret2= strftime 30 "%Y-%m-%d %H:%M:%S" x2        
    printLn ret2
    
    --printLn $ toYearDateTime $ prim__read_gmtime 0
    printLn $ fromOdooDate "2021-12-23"
    printLn dateTest
    printLn "time test end"
    {-
    let ret= strftime 30 "%Y-%m-%d %H:%M:%S" x    
    printLn ret
    
    let tx =strptime ret "%Y-%m-%d %H:%M:%S"         
    printLn tx
    -}
  


