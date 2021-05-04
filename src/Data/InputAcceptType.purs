module Ocelot.Data.InputAcceptType
  ( allAudios
  , allImages
  , allVideos
  , validate
  ) where

import Prelude

import DOM.HTML.Indexed.InputAcceptType as DOM.HTML.Indexed.InputAcceptType
import Data.Array as Data.Array
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.MediaType as Data.MediaType
import Data.String as Data.String
import Data.String.CodeUnits as Data.String.CodeUnits
import Data.String.Regex as Data.String.Regex
import Web.File.File as Web.File.File

allAudios :: DOM.HTML.Indexed.InputAcceptType.InputAcceptType
allAudios = DOM.HTML.Indexed.InputAcceptType.mediaType (Data.MediaType.MediaType "audio/*")

allImages :: DOM.HTML.Indexed.InputAcceptType.InputAcceptType
allImages = DOM.HTML.Indexed.InputAcceptType.mediaType (Data.MediaType.MediaType "image/*")

allVideos :: DOM.HTML.Indexed.InputAcceptType.InputAcceptType
allVideos = DOM.HTML.Indexed.InputAcceptType.mediaType (Data.MediaType.MediaType "video/*")

-- | Adapted from [attr-accept](https://github.com/react-dropzone/attr-accept)
validate ::
  DOM.HTML.Indexed.InputAcceptType.InputAcceptType ->
  Web.File.File.File ->
  Boolean
validate (DOM.HTML.Indexed.InputAcceptType.InputAcceptType xs) file =
  Data.Array.any (validateInputAcceptTypeAtom file) xs

validateInputAcceptTypeAtom ::
  Web.File.File.File ->
  DOM.HTML.Indexed.InputAcceptType.InputAcceptTypeAtom ->
  Boolean
validateInputAcceptTypeAtom file = case _ of
  DOM.HTML.Indexed.InputAcceptType.AcceptFileExtension extension ->
    validateFileExtension file extension
  DOM.HTML.Indexed.InputAcceptType.AcceptMediaType mediaType ->
    validateMediaType file mediaType

validateFileExtension ::
  Web.File.File.File ->
  String ->
  Boolean
validateFileExtension file accept =
  case Data.String.CodeUnits.stripSuffix (Data.String.Pattern (normalize accept)) (normalize (Web.File.File.name file)) of
    Nothing -> false
    Just _ -> true
  where
  normalize :: String -> String
  normalize =
    Data.String.toLower
      <<< Data.String.trim

validateMediaType ::
  Web.File.File.File ->
  Data.MediaType.MediaType ->
  Boolean
validateMediaType file (Data.MediaType.MediaType accept) =
  case Data.String.CodeUnits.stripSuffix (Data.String.Pattern "/*") validType of
    Nothing -> mimeType == validType
    Just baseValidType -> baseMimeType == baseValidType
  where
  mimeType :: String
  mimeType = normalize case Web.File.File.type_ file of
    Nothing -> ""
    Just (Data.MediaType.MediaType x) -> x

  baseMimeType :: String
  baseMimeType = case Data.String.Regex.regex "/.*$" mempty of
    Left _ -> mimeType
    Right regex -> Data.String.Regex.replace regex "" mimeType

  validType :: String
  validType = normalize accept

  normalize :: String -> String
  normalize =
    Data.String.toLower
      <<< Data.String.trim

