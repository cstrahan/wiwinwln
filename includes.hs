{-# LANGUAGE OverloadedStrings #-}

-- includes.hs
import Text.Pandoc.JSON
import Data.Text

doInclude :: Block -> IO Block
doInclude cb@(CodeBlock (id, classes, namevals) contents) =
  case lookup "include" namevals of
       Just f     -> do
         content <- readFile (unpack f)
         return (CodeBlock (id, classes, namevals) (pack content))
       Nothing    -> return cb
doInclude x = return x

doHtml :: Block -> IO Block
doHtml cb@(CodeBlock (id, classes, namevals) contents) =
  case lookup "literal" namevals of
       Just f     -> do
         content <- readFile (unpack f)
         return (RawBlock "html" (pack content))
       Nothing    -> return cb
doHtml x = return x

main :: IO ()
main = toJSONFilter (\b -> doInclude b >>= doHtml)
