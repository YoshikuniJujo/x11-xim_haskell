module GetXIMStyles (
  getDefs
, getXLookupStatuses
) where

import Text.RegexPR
import Data.Function

main = do
  cnt <- getContents
  lns <- getXLookupStatuses
  mapM_ putStrLn lns -- $ head $ filter ( any includeXLookupBoth ) $ toBlocks $ lines cnt
--  mapM_ putStrLn $ map getDefine $ getDefines $ lines cnt

getXLookupStatuses :: IO [ String ]
getXLookupStatuses = do
  cnt <- readFile "/usr/include/X11/Xlib.h"
  return $ map getDefine $ head $ filter ( any includeXLookupBoth ) $ toBlocks $ lines cnt

includeXLookupBoth :: String -> Bool
includeXLookupBoth = not . null . getbrsRegexPR "#define\\sXLookupBoth"

toBlocks :: [ String ] -> [ [ String ] ]
toBlocks [ ]          = [ ]
toBlocks ( "" : lns ) = toBlocks lns
toBlocks lns          = takeWhile ( not . null ) lns :
				toBlocks ( dropWhile ( not . null ) lns )

getDefs :: IO [ String ]
getDefs = do
  cnt <- readFile "/usr/include/X11/Xlib.h"
  return $ map getDefine $ getDefines $ lines cnt

getDefine :: String -> String
getDefine = (!! 1) . getbrsRegexPR "#define\\s+(\\S+)\\s+\\S+"

getDefines :: [ String ] -> [ String ]
getDefines =
  takeWhile ( not . null ) . dropWhile null . tail . dropWhile ( not . isXIMStyles )

isXIMStyles = not . null . getbrsRegexPR "\\s*}\\s*XIMStyles;\\s*"

isPreedit = not . null . getbrsRegexPR "#define\\sXIMPreedit"

isStatus  = not . null . getbrsRegexPR "#define\\sXIMStatus"

( f1 ||| f2 ) x = on (||) ($ x) f1 f2
