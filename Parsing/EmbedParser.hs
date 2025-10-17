module Common.Parsing.EmbedParser
    ( extractVideoId
    )
    where

import Text.Parsec
import Text.Parsec.String

-- Parser to extract the video ID
videoIdParser :: Parser String
videoIdParser = do
  -- Look for the data-video attribute
  _ <- manyTill anyChar
          $   (try $ string "data-video=\"")
          <|> (try $ string "data-video-id=\"")
          <|> (try $ string "href=\"https://youtu.be/")
  -- Capture the video ID
  videoId <- manyTill anyChar (try (char '"'))
  -- Return the captured ID
  return videoId

-- Function to apply the parser and extract the video ID
extractVideoId :: String -> Either ParseError String
extractVideoId input = parse videoIdParser "" input


main :: IO ()
main = do
    print $ extractVideoId working
    print $ extractVideoId nonWorking

    where
        working, nonWorking :: String
        working    = "<div class=\"video-container\" data-video=\"sODJC8nFMm4\"><a href=\"https://youtu.be/sODJC8nFMm4\" target=\"_blank\" class=\"file\"><img style=\"width:255px;height:190px;\" src=\"/vi/sODJC8nFMm4/0.jpg\" class=\"post-image\"/></a></div>"
        nonWorking = "<div class=\"video-container\" data-video-id=\"133Gqfx1Ydg\" data-iframe-width=\"768\" data-iframe-height=\"432\">\n\t\t\t<a href=\"https://youtu.be/133Gqfx1Ydg\" target=\"_blank\" class=\"file\">\n               \t<img style=\"width:255px;height:143px;object-fit:cover\" src=\"https://img.youtube.com/vi/133Gqfx1Ydg/0.jpg\" class=\"post-image\"/>\n\t\t\t</a>\n\t\t</div>"
