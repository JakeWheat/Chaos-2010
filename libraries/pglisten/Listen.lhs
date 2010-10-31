> {-# LANGUAGE ForeignFunctionInterface #-}
> module Listen (LConn
>               ,disconnect
>               ,connect
>               ,listen
>               ,unlisten
>               ,readNotify) where
>
> import Foreign
> import Foreign.C.Types
> import Foreign.Ptr
> import Foreign.C.String
> import Foreign.Marshal.Alloc
>
> newtype LConn = LConn (Ptr LConn)
>
> foreign import ccall "listen1lib.h pl_disconnect"
>     disconnect :: Ptr LConn -> IO ()
>
> foreign import ccall "listen1lib.h pl_connect"
>     plConnect :: CString -> IO (Ptr LConn)
>
> foreign import ccall "listen1lib.h pl_listen"
>     plListen :: Ptr LConn -> CString -> IO ()
>
> foreign import ccall "listen1lib.h pl_unlisten"
>     plUnlisten :: Ptr LConn -> CString -> IO ()
>
> foreign import ccall "listen1lib.h pl_read"
>     plReadNotify :: Ptr LConn -> IO CString
>
> connect :: String -> IO (Ptr LConn)
> connect cs = withCString cs plConnect
>
> listen :: Ptr LConn -> String -> IO ()
> listen c l = withCString l $ \l1 -> plListen c l1
>
> unlisten :: Ptr LConn -> String -> IO ()
> unlisten c l = withCString l $ \l1 -> plUnlisten c l1
>
> readNotify :: Ptr LConn -> IO String
> readNotify c = do
>                n <-plReadNotify c
>                r <- peekCString n
>                free n
>                return r
