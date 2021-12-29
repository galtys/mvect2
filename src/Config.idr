module Config

export
DB_URI : String
DB_URI = "postgresql://jan@localhost:5432/pjb-2021-12-03_1306"


namespace SystemState
  export
  ROOT_DIR : String
  ROOT_DIR = "/home/jan/github.com/mvect2/data/"
  export
  HCMD_DIR : String
  HCMD_DIR = ROOT_DIR++"hcmd/"

  export
  FX_DIR : String
  FX_DIR = ROOT_DIR++"fx/"
  
  export
  ROUTE_DIR : String
  ROUTE_DIR = ROOT_DIR++"route/"

  export
  LED_DIR : String
  LED_DIR = ROOT_DIR++"led/"

  export
  ROUTE_JOURNAL_DIR : String
  ROUTE_JOURNAL_DIR = ROOT_DIR++"route_journal/"
  
  export
  JOURNAL_DIR : String
  JOURNAL_DIR = ROOT_DIR++"journal/"

  export
  STATE_DIR : String
  STATE_DIR = ROOT_DIR++"state/"

  export
  BOM_DIR : String
  BOM_DIR = ROOT_DIR++"bom/"
  
  export
  PRODUCT_DIR : String
  PRODUCT_DIR = ROOT_DIR++"product/"  
