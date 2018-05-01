module Ocelot.Block.Icon
  ( back, back_
  , caratDown, caratDown_
  , caratLeft, caratLeft_
  , caratRight, caratRight_
  , caratUp, caratUp_
  , close, close_
  , collapse, collapse_
  , dataSources, dataSources_
  , delete, delete_
  , deleteCircle, deleteCircle_
  , download, download_
  , error, error_
  , expand, expand_
  , facebook, facebook_
  , info, info_
  , instagram, instagram_
  , loading, loading_
  , menu, menu_
  , navigate, navigate_
  , options, options_
  , refresh, refresh_
  , search, search_
  , settings, settings_
  , share, share_
  , success, success_
  , timeline, timeline_
  , tip, tip_
  , twitter, twitter_
  )
  where

import Prelude

import DOM.HTML.Indexed (HTMLspan)
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Halogen.HTML.Properties.ARIA as HA
import Ocelot.HTML.Properties ((<&>))

icon
  :: ∀ p i
   . String
  -> Array (HH.IProp HTMLspan i)
  -> HH.HTML p i
icon className iprops =
  HH.span
    [ HA.label className
    , HP.classes $ HH.ClassName <$>
      [ "inline-block"
      ]
    ]
    [ HH.span
        ( iprops <&>
          [ HA.hidden "true"
          , HP.classes $ HH.ClassName <$>
            [ className
            , "inline-block"
            , "transition-1/4-out"
            ]
          ]
        )
        []
    ]

back :: ∀ p i. Array (HH.IProp HTMLspan i) -> HH.HTML p i
back = icon "icon-back"

back_ :: ∀ p i. HH.HTML p i
back_ = back []

caratDown :: ∀ p i. Array (HH.IProp HTMLspan i) -> HH.HTML p i
caratDown = icon "icon-carat-down"

caratDown_ :: ∀ p i. HH.HTML p i
caratDown_ = caratDown []

caratLeft :: ∀ p i. Array (HH.IProp HTMLspan i) -> HH.HTML p i
caratLeft = icon "icon-carat-left"

caratLeft_ :: ∀ p i. HH.HTML p i
caratLeft_ = caratLeft []

caratRight :: ∀ p i. Array (HH.IProp HTMLspan i) -> HH.HTML p i
caratRight = icon "icon-carat-right"

caratRight_ :: ∀ p i. HH.HTML p i
caratRight_ = caratRight []

caratUp :: ∀ p i. Array (HH.IProp HTMLspan i) -> HH.HTML p i
caratUp = icon "icon-carat-up"

caratUp_ :: ∀ p i. HH.HTML p i
caratUp_ = caratUp []

close :: ∀ p i. Array (HH.IProp HTMLspan i) -> HH.HTML p i
close = icon "icon-close"

close_ :: ∀ p i. HH.HTML p i
close_ = close []

collapse :: ∀ p i. Array (HH.IProp HTMLspan i) -> HH.HTML p i
collapse = icon "icon-collapse"

collapse_ :: ∀ p i. HH.HTML p i
collapse_ = collapse []

dataSources :: ∀ p i. Array (HH.IProp HTMLspan i) -> HH.HTML p i
dataSources = icon "icon-data-sources"

dataSources_ :: ∀ p i. HH.HTML p i
dataSources_ = dataSources []

delete :: ∀ p i. Array (HH.IProp HTMLspan i) -> HH.HTML p i
delete = icon "icon-delete"

delete_ :: ∀ p i. HH.HTML p i
delete_ = delete []

deleteCircle :: ∀ p i. Array (HH.IProp HTMLspan i) -> HH.HTML p i
deleteCircle = icon "icon-delete-circle"

deleteCircle_ :: ∀ p i. HH.HTML p i
deleteCircle_ = deleteCircle []

download :: ∀ p i. Array (HH.IProp HTMLspan i) -> HH.HTML p i
download = icon "icon-download"

download_ :: ∀ p i. HH.HTML p i
download_ = download []

error :: ∀ p i. Array (HH.IProp HTMLspan i) -> HH.HTML p i
error = icon "icon-error"

error_ :: ∀ p i. HH.HTML p i
error_ = error []

expand :: ∀ p i. Array (HH.IProp HTMLspan i) -> HH.HTML p i
expand = icon "icon-expand"

expand_ :: ∀ p i. HH.HTML p i
expand_ = expand []

facebook :: ∀ p i. Array (HH.IProp HTMLspan i) -> HH.HTML p i
facebook = icon "icon-facebook"

facebook_ :: ∀ p i. HH.HTML p i
facebook_ = facebook []

info :: ∀ p i. Array (HH.IProp HTMLspan i) -> HH.HTML p i
info = icon "icon-info"

info_ :: ∀ p i. HH.HTML p i
info_ = info []

instagram :: ∀ p i. Array (HH.IProp HTMLspan i) -> HH.HTML p i
instagram = icon "icon-instagram"

instagram_ :: ∀ p i. HH.HTML p i
instagram_ = instagram []

loading :: ∀ p i. Array (HH.IProp HTMLspan i) -> HH.HTML p i
loading iprops =
  icon "icon-loading" $ [ HP.class_ $ HH.ClassName "spin" ] <&> iprops

loading_ :: ∀ p i. HH.HTML p i
loading_ = loading []

menu :: ∀ p i. Array (HH.IProp HTMLspan i) -> HH.HTML p i
menu = icon "icon-menu"

menu_ :: ∀ p i. HH.HTML p i
menu_ = menu []

navigate :: ∀ p i. Array (HH.IProp HTMLspan i) -> HH.HTML p i
navigate = icon "icon-navigate"

navigate_ :: ∀ p i. HH.HTML p i
navigate_ = navigate []

options :: ∀ p i. Array (HH.IProp HTMLspan i) -> HH.HTML p i
options = icon "icon-options"

options_ :: ∀ p i. HH.HTML p i
options_ = options []

refresh :: ∀ p i. Array (HH.IProp HTMLspan i) -> HH.HTML p i
refresh = icon "icon-refresh"

refresh_ :: ∀ p i. HH.HTML p i
refresh_ = refresh []

search :: ∀ p i. Array (HH.IProp HTMLspan i) -> HH.HTML p i
search = icon "icon-search"

search_ :: ∀ p i. HH.HTML p i
search_ = search []

settings :: ∀ p i. Array (HH.IProp HTMLspan i) -> HH.HTML p i
settings = icon "icon-settings"

settings_ :: ∀ p i. HH.HTML p i
settings_ = settings []

share :: ∀ p i. Array (HH.IProp HTMLspan i) -> HH.HTML p i
share = icon "icon-share"

share_ :: ∀ p i. HH.HTML p i
share_ = share []

success :: ∀ p i. Array (HH.IProp HTMLspan i) -> HH.HTML p i
success = icon "icon-success"

success_ :: ∀ p i. HH.HTML p i
success_ = success []

timeline :: ∀ p i. Array (HH.IProp HTMLspan i) -> HH.HTML p i
timeline = icon "icon-timeline"

timeline_ :: ∀ p i. HH.HTML p i
timeline_ = timeline []

tip :: ∀ p i. Array (HH.IProp HTMLspan i) -> HH.HTML p i
tip = icon "icon-tip"

tip_ :: ∀ p i. HH.HTML p i
tip_ = tip []

twitter :: ∀ p i. Array (HH.IProp HTMLspan i) -> HH.HTML p i
twitter = icon "icon-twitter"

twitter_ :: ∀ p i. HH.HTML p i
twitter_ = twitter []

