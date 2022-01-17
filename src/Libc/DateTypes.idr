module Libc.DateTypes

import Generics.Derive
import JSON

%language ElabReflection

namespace DateTimeTypes
  public export
  data ErrorCode = OK | ParsePartial | ParseError |OtherError
  %runElab derive "DateTimeTypes.ErrorCode" [Generic, Meta, Eq, Ord, Show, EnumToJSON,EnumFromJSON]

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
  
  
  export
  errorYearDateTime : YearDateTime
  errorYearDateTime = (MkYearDateTime 0 0 0 0 0 0 0 0 0)
  
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

  export        
  DEFAULT_SERVER_DATE_FORMAT : String
  DEFAULT_SERVER_DATE_FORMAT = "%Y-%m-%d"
  export
  DEFAULT_SERVER_TIME_FORMAT : String 
  DEFAULT_SERVER_TIME_FORMAT = "%H:%M:%S"
  export  
  DEFAULT_SERVER_DATETIME_FORMAT : String
  DEFAULT_SERVER_DATETIME_FORMAT = "\{DEFAULT_SERVER_DATE_FORMAT} \{DEFAULT_SERVER_TIME_FORMAT}"
  
  public export
  data DateFormatCode = YDTF | YDF | YMF | RAWF --| YF
  public export
  data DateTime = YDT YearDateTime| YD YearDate | YM YearMonth | Err DateTimeTypes.ErrorCode | RAW Int --| Year Int --tbd to Either? 
  %runElab derive "DateTimeTypes.DateTime" [Generic, Meta, Eq, Ord, Show, ToJSON,FromJSON]
  {-
  public export
  Date : Type
  Date = DateTime
  -}
