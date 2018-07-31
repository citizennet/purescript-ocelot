module Ocelot.Block.Icon where

import Prelude hiding (add)

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
            ]
          ]
        )
        []
    ]

add :: ∀ p i. Array (HH.IProp HTMLspan i) -> HH.HTML p i
add = icon "icon-add"

add_ :: ∀ p i. HH.HTML p i
add_ = add []

added :: ∀ p i. Array (HH.IProp HTMLspan i) -> HH.HTML p i
added = icon "icon-added"

added_ :: ∀ p i. HH.HTML p i
added_ = added []

arrowDown :: ∀ p i. Array (HH.IProp HTMLspan i) -> HH.HTML p i
arrowDown = icon "icon-arrow-down"

arrowDown_ :: ∀ p i. HH.HTML p i
arrowDown_ = arrowDown []

arrowLeft :: ∀ p i. Array (HH.IProp HTMLspan i) -> HH.HTML p i
arrowLeft = icon "icon-arrow-left"

arrowLeft_ :: ∀ p i. HH.HTML p i
arrowLeft_ = arrowLeft []

arrowRight :: ∀ p i. Array (HH.IProp HTMLspan i) -> HH.HTML p i
arrowRight = icon "icon-arrow-right"

arrowRight_ :: ∀ p i. HH.HTML p i
arrowRight_ = arrowRight []

arrowUp :: ∀ p i. Array (HH.IProp HTMLspan i) -> HH.HTML p i
arrowUp = icon "icon-arrow-up"

arrowUp_ :: ∀ p i. HH.HTML p i
arrowUp_ = arrowUp []

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

chevronLeft :: ∀ p i. Array (HH.IProp HTMLspan i) -> HH.HTML p i
chevronLeft = icon "icon-chevron-left"

chevronLeft_ :: ∀ p i. HH.HTML p i
chevronLeft_ = chevronLeft []

chevronRight :: ∀ p i. Array (HH.IProp HTMLspan i) -> HH.HTML p i
chevronRight = icon "icon-chevron-right"

chevronRight_ :: ∀ p i. HH.HTML p i
chevronRight_ = chevronRight []

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

plus :: ∀ p i. Array (HH.IProp HTMLspan i) -> HH.HTML p i
plus = icon "icon-plus"

plus_ :: ∀ p i. HH.HTML p i
plus_ = plus []

refresh :: ∀ p i. Array (HH.IProp HTMLspan i) -> HH.HTML p i
refresh = icon "icon-refresh"

refresh_ :: ∀ p i. HH.HTML p i
refresh_ = refresh []

search :: ∀ p i. Array (HH.IProp HTMLspan i) -> HH.HTML p i
search = icon "icon-search"

search_ :: ∀ p i. HH.HTML p i
search_ = search []

selected :: ∀ p i. Array (HH.IProp HTMLspan i) -> HH.HTML p i
selected = icon "icon-selected"

selected_ :: ∀ p i. HH.HTML p i
selected_ = selected []

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

