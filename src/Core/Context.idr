module Core.Context

import Data.IORef

namespace References
  public export
  data Ref : (l : label) -> Type -> Type where
       [search l]
       MkRef : IORef a -> Ref x a

  export   
  newRef : HasIO io => (x : label) -> t -> io (Ref x t)
  newRef x val
      = do ref <-  (newIORef val)
           pure (MkRef ref)

  export %inline
  get : HasIO io => (x : label) -> {auto ref : Ref x a} -> io a
  get x {ref = MkRef io} = (readIORef io)

  export %inline
  put : HasIO io => (x : label) -> {auto ref : Ref x a} -> a -> io ()
  put x {ref = MkRef io} val = (writeIORef io val)

  export %inline
  update : HasIO io => (x : label) -> {auto ref : Ref x a} -> (a -> a) -> io ()
  update x f
    = do v <- get x
         put x (f v)

  public export
  data MGs : Type where

  public export
  record MGSt where
    constructor MkMGSt
    cn : Int

--fn_data_ref : HasIO io => io (IORef Country)
--fn_data_ref = newIORef UK

