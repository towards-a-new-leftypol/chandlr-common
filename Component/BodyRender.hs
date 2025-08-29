{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Common.Component.BodyRender where

import Prelude hiding (head)
import Miso
    ( text
    , View
    )
import Miso.Html
    ( a_
    , br_
    , span_
    , strong_
    , u_
    , em_
    , s_
    , small_
    , pre_
    )
import Miso.Html.Property
    ( href_
    , target_
    , class_
    )
import Miso.String ( toMisoString, MisoString )
import System.FilePath ((</>))
import Text.Parsec (ParseError)
import Data.Maybe (fromJust)
import Data.List.NonEmpty (head)

import Common.Parsing.PostPartType (PostPart (..))
import Common.Parsing.QuoteLinkParser
import qualified Common.Component.Thread.Model as Model
import qualified Common.Network.SiteType as Site
import qualified Common.Network.BoardType as Board
import qualified Common.Network.ThreadType as Thread
import qualified Common.Network.PostType as Post

{-
 - This is the inverse of parsePostBody from BodyParser except
 - that the output is a miso View and not Text. It might be
 - worth trying to use quickcheck to verify the bijection between
 - Text and DOM by creating random PostParts and seeing if 
 - ((parsePostBody . render) parts == parts)
 -
 - (is there an easy way to render a miso View?, that's what's missing
 - a f :: View a -> Text)
 -}
render :: Model.Model -> [ PostPart ] -> [ View model a ]
render m = map (renderPostPart m)

renderPostPart :: Model.Model -> PostPart -> View model a
renderPostPart _ (SimpleText txt) = text txt
renderPostPart _ (PostedUrl u) =
    a_
        [ href_ u
        , target_ "_blank"
        ]
        [ text u ]

renderPostPart _ Skip = br_ []

renderPostPart m (Quote parse_result) = elems parse_result
    where
        elems :: Either ParseError ParsedURL -> View model a
        elems (Left err) =
            a_
                []
                [ text $ toMisoString $ show err ]

        elems (Right p) =
            case full_url p of
                Nothing ->
                    a_
                        [ href_ $ "/" <> site_name <> "/" <> linked_board <> "/" ]
                        [ text $ ">>>/" <> linked_board <> "/" ]

                Just u ->
                    if current_board /= linked_board
                    then
                        a_
                            [ href_ u ]
                            [ text $ ">>>/" <> linked_board <> "/" <> post_id ]
                    else
                        a_
                            [ href_ u ]
                            $
                            (text $ ">>" <> post_id)
                            :
                            if pid == op_id
                            then [ small_ [] [ " (OP)" ] ]
                            else []

            where
                linked_board = toMisoString $ boardName p

                pid = fromJust $ postId p

                post_id = toMisoString $ show pid

                current_board = toMisoString $ Board.pathpart $ head $ Site.boards (Model.site m)

                op_id = Post.board_post_id $ head $ Thread.posts $ head $ Board.threads $ head $ Site.boards (Model.site m)


        full_url :: ParsedURL -> Maybe MisoString
        full_url ParsedURL {..} = do
            tid <- threadId
            pid <- postId

            return $ "/" <> site_name <> "/" <> (toMisoString $ boardName </> show tid ++ "#" ++ show pid)

        site_name = toMisoString $ Site.name $ Model.site m

        -- cases of urls:
        -- url:
        -- /b/res/1.html#2
        -- if on different board:
        -- >>/b/2
        -- if on same board or same thread:
        -- >>2
        --
        -- url:
        -- /b/index.html
        -- if only board:
        -- >>>/b/

renderPostPart m (GreenText parts) =
    span_ [ class_ "quote" ] (render m parts)

renderPostPart m (OrangeText parts) =
    span_ [ class_ "orangeQuote" ] (render m parts)

renderPostPart m (RedText parts) =
    span_ [ class_ "heading" ] (render m parts)

renderPostPart m (Spoiler parts) =
    span_ [ class_ "spoiler" ] (render m parts)

renderPostPart m (Bold parts) =
    strong_ [] (render m parts)

renderPostPart m (Underlined parts) =
    u_ [] (render m parts)

renderPostPart m (Italics parts) =
    em_ [] (render m parts)

renderPostPart m (Strikethrough parts) =
    s_ [] (render m parts)

renderPostPart m (Code parts) =
    pre_ [ class_ "code" ] (render m parts)
