#include <X11/Xlib.h>
#include <X11/keysym.h>

module Graphics.X11.Xim (

  XIM(..)
, XIC(..)
, XNInputStyle(..)
, XrmDatabase(..)

, openIM
, closeIM
, createIC
, destroyIC
, getICValue
, filterEvent
, utf8LookupString
, utf8DrawString

) where

import Graphics.X11.XimTypes
import Codec.Binary.UTF8.String ( decodeString, encodeString )

import Graphics.X11	( Window, XEventPtr, GC, KeySym )
import Graphics.X11.Xlib.Extras	( FontSet(..) )
import Graphics.X11.Xlib.Types	( Display(..) )
import Foreign		( Int32, nullPtr, Ptr, Word32, alloca, peek, Int64,
				Word64,
				allocaBytes, throwIfNull )
import Foreign.C.Types	( CInt, CChar )
import Foreign.C.String	( CString, peekCStringLen, withCString )

-- DATA TYPE

data XIM = XIM { ximPtr :: Ptr XIM }
data XIC = XIC { xicPtr :: Ptr XIC }

data XrmDatabase = XrmDatabase ( Ptr XrmDatabase )

-- FUNCTIONS

foreign import ccall "X11/Xlib.h XOpenIM"	c_XOpenIM	::
	Ptr Display -> Ptr XrmDatabase -> CString -> CString -> IO ( Ptr XIM )
openIM :: Display -> Maybe XrmDatabase -> Maybe String -> Maybe String
							-> IO XIM
openIM ( Display pdpy ) mxd rn rc =
	let pxd = case mxd of
			Nothing                -> nullPtr
			Just ( XrmDatabase p ) -> p
	 in fmap XIM $ withMaybeCString rn $ \crn ->
					withMaybeCString rc $ \crc -> do
		throwIfNull "openIM" $ c_XOpenIM pdpy pxd crn crc

foreign import ccall "X11/Xlib.h XCloseIM"	c_XCloseIM	::
	Ptr XIM -> IO ()
closeIM :: XIM -> IO ()
closeIM = c_XCloseIM . ximPtr

foreign import ccall "X11/Xlib.h XCreateIC"	c_XCreateIC2	::
	Ptr XIM -> CString -> #{ type long } -> CString
		-> Window -> Ptr () -> IO ( Ptr XIC )
createIC :: XIM -> [ XNInputStyle ] -> Window -> IO XIC
createIC ( XIM pim ) xniss win = do
	fmap XIC $ throwIfNull "createIC" $ withCString "inputStyle" $ \is ->
		withCString "clientWindow" $ \cw ->
			c_XCreateIC2 pim is ( getCXNInputStyle xniss )
				cw win nullPtr

foreign import ccall "X11/Xlib.h XDestroyIC"	c_XDestroyIC	::
	Ptr XIC -> IO ()
destroyIC :: XIC -> IO ()
destroyIC = c_XDestroyIC . xicPtr

foreign import ccall "X11/Xlib.h XGetICValues"	c_XGetICValues1	::
	Ptr XIC -> CString -> Ptr #{ type unsigned long }
                -> Ptr () -> IO CString
getICValue :: XIC -> String -> IO #{ type unsigned long }
getICValue ( XIC pic ) fn = withCString fn $ \cfn ->
	alloca $ \p -> do
		r <- c_XGetICValues1 pic cfn p nullPtr
		if r == nullPtr
			then peek p
			else error "bad"

foreign import ccall "X11/Xlib.h XFilterEvent" c_XFilterEvent ::
	XEventPtr -> Window -> IO #{ type Bool }
filterEvent :: XEventPtr -> Window -> IO Bool
filterEvent e w = do
  ret <- c_XFilterEvent e w
  return $ ret == #{ const True }

foreign import ccall "X11/Xlib.h Xutf8LookupString"	c_Xutf8LookupString
	:: Ptr XIC -> XEventPtr -> Ptr CChar -> CInt -> Ptr #{ type KeySym }
		-> Ptr #{ type Status } -> IO CInt
utf8LookupString :: XIC -> XEventPtr -> IO ( Maybe String, Maybe KeySym )
utf8LookupString xic pev = utf8LookupStringGen xic pev 8
utf8LookupStringGen :: XIC -> XEventPtr -> Int -> IO ( Maybe String, Maybe KeySym )
utf8LookupStringGen ( XIC pic ) pev bs = allocaBytes bs $ \buf ->
	alloca $ \ks -> alloca $ \stat -> do
		cnt <- fmap fromIntegral $ c_Xutf8LookupString pic pev buf
						( fromIntegral bs ) ks stat
		cStat <- peek stat
		case fromCXLookupStatus cStat of
			XLookupBoth   -> do
				str <- peekCStringLen ( buf, cnt )
				cks <- peek ks
				return ( Just $ decodeString str, Just cks )
			XLookupChars   -> do
				str <- peekCStringLen ( buf, cnt )
				return ( Just $ decodeString str, Nothing )
			XLookupKeySym   -> do
				cks <- peek ks
				return ( Nothing, Just cks )
			XLookupNone     -> return ( Nothing, Nothing )
			XBufferOverflow -> utf8LookupStringGen
						( XIC pic ) pev ( 2 * bs )


foreign import ccall "X11/Xlib.h Xutf8DrawString"		c_Xutf8DrawString	::
	Ptr Display -> Window -> Ptr FontSet -> GC -> CInt -> CInt
		-> CString -> CInt -> IO ()
utf8DrawString :: Display -> Window -> FontSet -> GC -> Int -> Int -> String ->
	IO ()
utf8DrawString ( Display pdpy ) win ( FontSet pfs ) gc x y str =
	let	utf8Str = encodeString str
	 in withCString utf8Str $ \cstr ->
		c_Xutf8DrawString pdpy win pfs gc ( fromIntegral x )
			( fromIntegral y ) cstr
			( fromIntegral $ length utf8Str )

withMaybeCString :: Maybe String -> ( CString -> IO a ) -> IO a
withMaybeCString Nothing      f = f nullPtr
withMaybeCString ( Just str ) f = withCString str f
