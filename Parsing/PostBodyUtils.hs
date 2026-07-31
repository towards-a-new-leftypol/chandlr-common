module Common.Parsing.PostBodyUtils where

import qualified Data.Map as Map

import Common.Parsing.PostPartType
import Common.Component.Thread.Model (PostWithBody)
import Common.Parsing.QuoteLinkParser (ParsedURL (..))

import Debug.Trace (trace)

collectBacklinks :: [ PostWithBody ] -> Backlinks
collectBacklinks = foldr insertElement Map.empty
    where
      insertElement :: PostWithBody -> Backlinks -> Backlinks
      insertElement (post, body) acc = foldr insertPost acc (quotedPosts body)
          where
              insertPost post_id = Map.insertWith (++) post_id [post]


quotedPosts :: [ PostPart ] -> [ Integer ]
quotedPosts [] = []
quotedPosts (Quote (Right (ParsedURL { postId = Just p })) : xxs)
                                       = p               : quotedPosts xxs
quotedPosts (Quote (Left e)     : xxs) =
                   trace ("!!! UrlParseError !!! " ++ e) $ quotedPosts xxs
quotedPosts ((GreenText     xs) : xxs) = quotedPosts xs ++ quotedPosts xxs
quotedPosts ((OrangeText    xs) : xxs) = quotedPosts xs ++ quotedPosts xxs
quotedPosts ((RedText       xs) : xxs) = quotedPosts xs ++ quotedPosts xxs
quotedPosts ((Spoiler       xs) : xxs) = quotedPosts xs ++ quotedPosts xxs
quotedPosts ((Bold          xs) : xxs) = quotedPosts xs ++ quotedPosts xxs
quotedPosts ((Underlined    xs) : xxs) = quotedPosts xs ++ quotedPosts xxs
quotedPosts ((Italics       xs) : xxs) = quotedPosts xs ++ quotedPosts xxs
quotedPosts ((Strikethrough xs) : xxs) = quotedPosts xs ++ quotedPosts xxs
quotedPosts ( _                 : xxs) = quotedPosts xxs
