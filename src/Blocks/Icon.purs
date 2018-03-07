module Ocelot.Block.Icon 
  ( back, back_
  , caratDown, caratDown_
  , caratRight, caratRight_
  , caratUp, caratUp_
  , close, close_
  , collapse, collapse_
  , dataSources, dataSources_
  , download, download_
  , expand, expand_
  , info, info_
  , menu, menu_
  , options, options_
  , refresh, refresh_
  , search, search_ 
  , settings, settings_
  , share, share_
  , timeline, timeline_
  )
  where

import DOM.HTML.Indexed (HTMLspan)
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Halogen.HTML.Properties.ARIA as HA
import Ocelot.Core.Utils ((<&>))

icon 
  :: ∀ p i
   . String
  -> Array (HH.IProp HTMLspan i)
  -> HH.HTML p i
icon className iprops =
  HH.span
    [ HA.label className ]
    [ HH.span
        (iprops <&> [ HA.hidden "true", HP.class_ (HH.ClassName className) ])
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

download :: ∀ p i. Array (HH.IProp HTMLspan i) -> HH.HTML p i
download = icon "icon-download"

download_ :: ∀ p i. HH.HTML p i
download_ = download []

expand :: ∀ p i. Array (HH.IProp HTMLspan i) -> HH.HTML p i
expand = icon "icon-expand"

expand_ :: ∀ p i. HH.HTML p i
expand_ = expand []

info :: ∀ p i. Array (HH.IProp HTMLspan i) -> HH.HTML p i
info = icon "icon-info"

info_ :: ∀ p i. HH.HTML p i
info_ = info []

menu :: ∀ p i. Array (HH.IProp HTMLspan i) -> HH.HTML p i
menu = icon "icon-menu"

menu_ :: ∀ p i. HH.HTML p i
menu_ = menu []

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

timeline :: ∀ p i. Array (HH.IProp HTMLspan i) -> HH.HTML p i
timeline = icon "icon-timeline"

timeline_ :: ∀ p i. HH.HTML p i
timeline_ = timeline []

