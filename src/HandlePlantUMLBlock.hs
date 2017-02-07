import Text.Pandoc.JSON
import Data.ByteString.Lazy (hGetContents, hPut, ByteString)
import Data.ByteString.Lazy.UTF8 (fromString)
import Data.Digest.Pure.SHA (sha1, showDigest)
import System.IO
       (hClose, hPutStr, IOMode(..), openBinaryFile, Handle)
import System.Process

processBlocks :: Maybe Format -> Block -> IO Block
processBlocks format cb@(CodeBlock (id_, classes, _) contents) =
  if elem "plantuml" classes || elem "{plantuml}" classes -- stupid - only because of atom markdown-preview-enhanced!!
    then plantUMLToImg format contents
    else return cb
processBlocks _ cb = return cb

plantUMLToImg :: Maybe Format -> String -> IO Block
plantUMLToImg format content = do
  (result, path) <- renderImage format content
  writeImageFile result path
  return $ Para [Image nullAttr [] (path, "")]
  where
    writeImageFile :: ByteString -> String -> IO ()
    writeImageFile r p = do
      hFile <- openBinaryFile p WriteMode
      hPut hFile r
      hClose hFile

renderImage :: Maybe Format -> String -> IO (ByteString, String)
renderImage format content = do
  let path = uniqueName content ++ "." ++ imgFormat format
  (Just hIn, Just hOut, _, _) <-
    createProcess
      (proc "/bin/sh" ["plantuml", "-pipe", "-t" ++ imgFormat format]) -- shell won't work; I don't know why
      { std_in = CreatePipe
      , std_out = CreatePipe
      }
  hPutStr hIn content
  hClose hIn
  res <- hGetContents hOut
  hClose hOut
  return (res, path)
  where
    imgFormat :: Maybe Format -> String
    imgFormat (Just (Format s))
      | s `elem` ["pdf", "latex"] = "latex"
      | s `elem` ["html", "markdown"] = "svg"
      | otherwise = "png"
    imgFormat _ = "png"
    uniqueName :: String -> String
    uniqueName = showDigest . sha1 . fromString

main :: IO ()
main = toJSONFilter processBlocks
-- Roadmap
-- DONE:0 Image format by target format (eg: eps for Latex and svg for HTML)
-- TODO:10 Embeded images
-- TODO:20 Multiple formats (mermaid, ) - migrate tot pandoc-filter-multi
-- TODO:0 Read Metadata
