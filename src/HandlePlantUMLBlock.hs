import qualified Data.ByteString.Lazy      as BS (ByteString, concat, hGetContents, hPut)
import           Data.ByteString.Lazy.UTF8 (fromString, toString)
import           Data.Digest.Pure.SHA      (sha1, showDigest)
import           Debug.Trace
import           System.IO                 (Handle, IOMode (..), hClose, hPutStr, openBinaryFile)
import           System.Process
import           Text.Pandoc.JSON

imgContent :: Bool -> String -> String -> BS.ByteString -> IO Inline
imgContent True _ fmt cnt = return $ RawInline (Format fmt) $ toString cnt
imgContent False name fmt cnt = do
  let path = uniqueName name cnt ++ "." ++ imgFormat fmt
  writeImg path cnt
  return $ Image nullAttr [] (path, "")
  where
    uniqueName :: String -> BS.ByteString -> String
    uniqueName name content
      | name == "" = showDigest $ sha1 content
      | otherwise = name

processBlocks :: Maybe Format -> Block -> IO Block
processBlocks (Just f@(Format format)) cb@(CodeBlock (id_, classes, keyValues) contents)
  | elem "plantuml" classes = do
    let fmt = imgFormat format
    cnt <- renderImage fmt contents
    img <- imgContent ("inline" `elem` classes) id_ format cnt
    return $ Para [img]
  | otherwise = return cb
processBlocks _ cb = return cb

imgFormat :: String -> String
imgFormat s ------ << FIXME use correct format srings
  | s `elem` ["pdf", "latex"] = "latex"
  | s `elem` ["html", "markdown"] = "svg"
  | otherwise = "png"

writeImg :: String -> BS.ByteString -> IO String
writeImg path content = do
  hFile <- openBinaryFile path WriteMode
  BS.hPut hFile content
  hClose hFile
  return path

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
-- DONE:10 Use CodeBlock's attributes
-- DONE:20 Embeded images
-- TODO:0 Read Metadata
-- TODO:10 Multiple formats (mermaid, ) - migrate tot pandoc-filter-multi
