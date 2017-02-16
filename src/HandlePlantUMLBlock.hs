import Text.Pandoc.JSON
import Debug.Trace
import qualified Data.ByteString.Lazy as BS
       (hGetContents, hPut, concat, ByteString)
import Data.ByteString.Lazy.UTF8 (fromString, toString)
import Data.Digest.Pure.SHA (sha1, showDigest)
import System.IO
       (hClose, hPutStr, IOMode(..), openBinaryFile, Handle)
import System.Process

imgContent :: Bool -> String -> BS.ByteString -> IO Inline
imgContent True fmt cnt = return $ RawInline (Format fmt) $ toString cnt
imgContent False fmt cnt = do
  path <- writeImg fmt cnt
  return $ Image nullAttr [] (path, "")

processBlocks :: Maybe Format -> Block -> IO Block
processBlocks (Just f@(Format format)) cb@(CodeBlock (id_, classes, keyValues) contents)
  | elem "plantuml" classes = do
    let fmt = imgFormat format
    cnt <- renderImage fmt contents
    img <- imgContent ("inline" `elem` classes) format cnt
    return $ Para [img]
  | otherwise = return cb
processBlocks _ cb = return cb

imgFormat :: String -> String
imgFormat s ------ << FIXME use correct format srings
  | s `elem` ["pdf", "latex"] = "latex"
  | s `elem` ["html", "markdown"] = "svg"
  | otherwise = "png"

-- Filename can be random or an attribute
writeImg :: String -> BS.ByteString -> IO String
writeImg fmt content = do
  let path = uniqueName content ++ "." ++ fmt
  hFile <- openBinaryFile path WriteMode
  BS.hPut hFile content
  hClose hFile
  return path
  where
    uniqueName :: BS.ByteString -> String
    uniqueName = showDigest . sha1

renderImage :: String -> String -> IO BS.ByteString
renderImage format content = do
  (Just hIn, Just hOut, _, _) <-
    createProcess
      (proc "/bin/sh" ["plantuml", "-pipe", "-t" ++ format]) -- shell won't work; I don't know why
      { std_in = CreatePipe
      , std_out = CreatePipe
      }
  hPutStr hIn content
  hClose hIn
  BS.hGetContents hOut

main :: IO ()
main = toJSONFilter processBlocks
-- Roadmap
-- DONE:0 Image format by target format (eg: eps for Latex and svg for HTML)
-- TODO:0 Use CodeBlock's attributes
-- TODO:10 Embeded images
-- TODO:20 Read Metadata
-- TODO:30 Multiple formats (mermaid, ) - migrate tot pandoc-filter-multi
