module Config

export
DB_URI : String
DB_URI = "postgresql://jan@localhost:5432/pjb-2021-12-03_1306"


namespace SystemState
  export
  HCMD_DIR : String
  HCMD_DIR = "/home/jan/github.com/mvect2/data/hcmd/"

  export
  FX_DIR : String
  FX_DIR = "/home/jan/github.com/mvect2/data/fx/"
  
  export
  ROUTE_DIR : String
  ROUTE_DIR = "/home/jan/github.com/mvect2/data/route/"

  export
  LED_DIR : String
  LED_DIR = "/home/jan/github.com/mvect2/data/led/"

  export
  ROUTE_JOURNAL_DIR : String
  ROUTE_JOURNAL_DIR = "/home/jan/github.com/mvect2/data/route_journal/"
  
  export
  JOURNAL_DIR : String
  JOURNAL_DIR = "/home/jan/github.com/mvect2/data/journal/"

  export
  STATE_DIR : String
  STATE_DIR = "/home/jan/github.com/mvect2/data/state/"
