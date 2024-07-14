{-# LANGUAGE ForeignFunctionInterface #-}

module CppFFI
  ( 
    getMessagee,
    add
  ) where

import Foreign.C.String (CString, peekCString)
import Foreign.C.Types (CInt(..))
import Foreign.Ptr (Ptr)

-- | Calls the GetMessagee function in the C++ DLL
foreign import ccall "GetMessagee" getMessagee :: IO CString

-- | Calls the Add function in the C++ DLL
foreign import ccall "Add" add :: CInt -> CInt -> IO CInt
