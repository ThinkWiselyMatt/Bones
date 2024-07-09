{-# LANGUAGE ForeignFunctionInterface #-}

module CppFFI
  ( 
    getMessagee,
    add
  ) where

import Foreign.C.String (CString)
import Foreign.C.Types (CInt(..))

-- | Calls the GetMessagee function in the C++ DLL
foreign import ccall "GetMessagee" getMessagee :: IO CString

-- | Calls the Add function in the C++ DLL
foreign import ccall "Add" add :: CInt -> CInt -> IO CInt