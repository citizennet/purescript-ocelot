module Ocelot.Block.Icon where

import Prelude hiding (add)

import DOM.HTML.Indexed (HTMLspan)
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Halogen.HTML.Properties.ARIA as HA
import Ocelot.HTML.Properties ((<&>))

icon ::
  forall p i.
  String ->
  Array (HH.IProp HTMLspan i) ->
  HH.HTML p i
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

adSet :: forall p i. Array (HH.IProp HTMLspan i) -> HH.HTML p i
adSet = icon "icon-adset"

adSet_ :: forall p i. HH.HTML p i
adSet_ = adSet []

add :: forall p i. Array (HH.IProp HTMLspan i) -> HH.HTML p i
add = icon "icon-add"

add_ :: forall p i. HH.HTML p i
add_ = add []

added :: forall p i. Array (HH.IProp HTMLspan i) -> HH.HTML p i
added = icon "icon-added"

added_ :: forall p i. HH.HTML p i
added_ = added []

arrowDown :: forall p i. Array (HH.IProp HTMLspan i) -> HH.HTML p i
arrowDown = icon "icon-arrow-down"

arrowDown_ :: forall p i. HH.HTML p i
arrowDown_ = arrowDown []

arrowLeft :: forall p i. Array (HH.IProp HTMLspan i) -> HH.HTML p i
arrowLeft = icon "icon-arrow-left"

arrowLeft_ :: forall p i. HH.HTML p i
arrowLeft_ = arrowLeft []

arrowRight :: forall p i. Array (HH.IProp HTMLspan i) -> HH.HTML p i
arrowRight = icon "icon-arrow-right"

arrowRight_ :: forall p i. HH.HTML p i
arrowRight_ = arrowRight []

arrowUp :: forall p i. Array (HH.IProp HTMLspan i) -> HH.HTML p i
arrowUp = icon "icon-arrow-up"

arrowUp_ :: forall p i. HH.HTML p i
arrowUp_ = arrowUp []

back :: forall p i. Array (HH.IProp HTMLspan i) -> HH.HTML p i
back = icon "icon-back"

back_ :: forall p i. HH.HTML p i
back_ = back []

campaign :: forall p i. Array (HH.IProp HTMLspan i) -> HH.HTML p i
campaign = icon "icon-campaign"

campaign_ :: forall p i. HH.HTML p i
campaign_ = campaign []

caratDown :: forall p i. Array (HH.IProp HTMLspan i) -> HH.HTML p i
caratDown = icon "icon-carat-down"

caratDown_ :: forall p i. HH.HTML p i
caratDown_ = caratDown []

caratLeft :: forall p i. Array (HH.IProp HTMLspan i) -> HH.HTML p i
caratLeft = icon "icon-carat-left"

caratLeft_ :: forall p i. HH.HTML p i
caratLeft_ = caratLeft []

caratRight :: forall p i. Array (HH.IProp HTMLspan i) -> HH.HTML p i
caratRight = icon "icon-carat-right"

caratRight_ :: forall p i. HH.HTML p i
caratRight_ = caratRight []

caratUp :: forall p i. Array (HH.IProp HTMLspan i) -> HH.HTML p i
caratUp = icon "icon-carat-up"

caratUp_ :: forall p i. HH.HTML p i
caratUp_ = caratUp []

chevronLeft :: forall p i. Array (HH.IProp HTMLspan i) -> HH.HTML p i
chevronLeft = icon "icon-chevron-left"

chevronLeft_ :: forall p i. HH.HTML p i
chevronLeft_ = chevronLeft []

chevronRight :: forall p i. Array (HH.IProp HTMLspan i) -> HH.HTML p i
chevronRight = icon "icon-chevron-right"

chevronRight_ :: forall p i. HH.HTML p i
chevronRight_ = chevronRight []

close :: forall p i. Array (HH.IProp HTMLspan i) -> HH.HTML p i
close = icon "icon-close"

close_ :: forall p i. HH.HTML p i
close_ = close []

collapse :: forall p i. Array (HH.IProp HTMLspan i) -> HH.HTML p i
collapse = icon "icon-collapse"

collapse_ :: forall p i. HH.HTML p i
collapse_ = collapse []

dataSources :: forall p i. Array (HH.IProp HTMLspan i) -> HH.HTML p i
dataSources = icon "icon-data-sources"

dataSources_ :: forall p i. HH.HTML p i
dataSources_ = dataSources []

delete :: forall p i. Array (HH.IProp HTMLspan i) -> HH.HTML p i
delete = icon "icon-delete"

delete_ :: forall p i. HH.HTML p i
delete_ = delete []

deleteCircle :: forall p i. Array (HH.IProp HTMLspan i) -> HH.HTML p i
deleteCircle = icon "icon-delete-circle"

deleteCircle_ :: forall p i. HH.HTML p i
deleteCircle_ = deleteCircle []

demographics :: forall p i. Array (HH.IProp HTMLspan i) -> HH.HTML p i
demographics = icon "icon-demographics"

demographics_ :: forall p i. HH.HTML p i
demographics_ = demographics []

download :: forall p i. Array (HH.IProp HTMLspan i) -> HH.HTML p i
download = icon "icon-download"

download_ :: forall p i. HH.HTML p i
download_ = download []

error :: forall p i. Array (HH.IProp HTMLspan i) -> HH.HTML p i
error = icon "icon-error"

error_ :: forall p i. HH.HTML p i
error_ = error []

expand :: forall p i. Array (HH.IProp HTMLspan i) -> HH.HTML p i
expand = icon "icon-expand"

expand_ :: forall p i. HH.HTML p i
expand_ = expand []

facebook :: forall p i. Array (HH.IProp HTMLspan i) -> HH.HTML p i
facebook = icon "icon-facebook"

facebook_ :: forall p i. HH.HTML p i
facebook_ = facebook []

google :: forall p i. Array (HH.IProp HTMLspan i) -> HH.HTML p i
google = icon "icon-google"

google_ :: forall p i. HH.HTML p i
google_ = google []

info :: forall p i. Array (HH.IProp HTMLspan i) -> HH.HTML p i
info = icon "icon-info"

info_ :: forall p i. HH.HTML p i
info_ = info []

instagram :: forall p i. Array (HH.IProp HTMLspan i) -> HH.HTML p i
instagram = icon "icon-instagram"

instagram_ :: forall p i. HH.HTML p i
instagram_ = instagram []

linkedIn :: forall p i. Array (HH.IProp HTMLspan i) -> HH.HTML p i
linkedIn = icon "icon-linkedin"

linkedIn_ :: forall p i. HH.HTML p i
linkedIn_ = linkedIn []

menu :: forall p i. Array (HH.IProp HTMLspan i) -> HH.HTML p i
menu = icon "icon-menu"

menu_ :: forall p i. HH.HTML p i
menu_ = menu []

navigate :: forall p i. Array (HH.IProp HTMLspan i) -> HH.HTML p i
navigate = icon "icon-navigate"

navigate_ :: forall p i. HH.HTML p i
navigate_ = navigate []

options :: forall p i. Array (HH.IProp HTMLspan i) -> HH.HTML p i
options = icon "icon-options"

options_ :: forall p i. HH.HTML p i
options_ = options []

pinterest :: forall p i. Array (HH.IProp HTMLspan i) -> HH.HTML p i
pinterest = icon "icon-pinterest"

pinterest_ :: forall p i. HH.HTML p i
pinterest_ = pinterest []

plus :: forall p i. Array (HH.IProp HTMLspan i) -> HH.HTML p i
plus = icon "icon-plus"

plus_ :: forall p i. HH.HTML p i
plus_ = plus []

reddit :: forall p i. Array (HH.IProp HTMLspan i) -> HH.HTML p i
reddit = icon "icon-reddit"

reddit_ :: forall p i. HH.HTML p i
reddit_ = reddit []

refresh :: forall p i. Array (HH.IProp HTMLspan i) -> HH.HTML p i
refresh = icon "icon-refresh"

refresh_ :: forall p i. HH.HTML p i
refresh_ = refresh []

search :: forall p i. Array (HH.IProp HTMLspan i) -> HH.HTML p i
search = icon "icon-search"

search_ :: forall p i. HH.HTML p i
search_ = search []

selected :: forall p i. Array (HH.IProp HTMLspan i) -> HH.HTML p i
selected = icon "icon-selected"

selected_ :: forall p i. HH.HTML p i
selected_ = selected []

settings :: forall p i. Array (HH.IProp HTMLspan i) -> HH.HTML p i
settings = icon "icon-settings"

settings_ :: forall p i. HH.HTML p i
settings_ = settings []

share :: forall p i. Array (HH.IProp HTMLspan i) -> HH.HTML p i
share = icon "icon-share"

share_ :: forall p i. HH.HTML p i
share_ = share []

snapchat :: forall p i. Array (HH.IProp HTMLspan i) -> HH.HTML p i
snapchat = icon "icon-snapchat"

snapchat_ :: forall p i. HH.HTML p i
snapchat_ = snapchat []

success :: forall p i. Array (HH.IProp HTMLspan i) -> HH.HTML p i
success = icon "icon-success"

success_ :: forall p i. HH.HTML p i
success_ = success []

taboola :: forall p i. Array (HH.IProp HTMLspan i) -> HH.HTML p i
taboola = icon "icon-taboola"

taboola_ :: forall p i. HH.HTML p i
taboola_ = taboola []

tiktok :: forall p i. Array (HH.IProp HTMLspan i) -> HH.HTML p i
tiktok = icon "icon-tiktok"

tiktok_ :: forall p i. HH.HTML p i
tiktok_ = tiktok []

timeline :: forall p i. Array (HH.IProp HTMLspan i) -> HH.HTML p i
timeline = icon "icon-timeline"

timeline_ :: forall p i. HH.HTML p i
timeline_ = timeline []

tip :: forall p i. Array (HH.IProp HTMLspan i) -> HH.HTML p i
tip = icon "icon-tip"

tip_ :: forall p i. HH.HTML p i
tip_ = tip []

twitter :: forall p i. Array (HH.IProp HTMLspan i) -> HH.HTML p i
twitter = icon "icon-twitter"

twitter_ :: forall p i. HH.HTML p i
twitter_ = twitter []

youtube :: forall p i. Array (HH.IProp HTMLspan i) -> HH.HTML p i
youtube = icon "icon-youtube"

youtube_ :: forall p i. HH.HTML p i
youtube_ = youtube []

