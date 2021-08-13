module Ocelot.Block.Pager where

import Prelude

import DOM.HTML.Indexed (HTMLdiv)
import Data.Array ((..))
import Data.FoldableWithIndex (foldlWithIndex)
import Foreign.Object as Foreign.Object
import Halogen.HTML as HH
import Halogen.HTML.Core (ClassName(..))
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Ocelot.Block.Icon as Ocelot.Block.Icon
import Ocelot.HTML.Properties as Ocelot.HTML.Properties
import Web.UIEvent.MouseEvent (MouseEvent)

-----
-- old CN pager (require citizennet.css)

pager :: ∀ p i. Int -> Int -> (Int -> MouseEvent -> i) -> HH.HTML p i
pager skip last query =
  HH.div
    [ HP.classes
      [ ClassName "cn-pagination"
      , ClassName "clearfix"
      ]
    ]
    [ HH.ul
      [ HP.class_ (ClassName "pagination")
      ]
      makePagingButtons
    ]
  where
    makeList :: Array Int
    makeList | last < 2 = []
    makeList | last <= 6 = 1 .. last
    makeList | skip < 5 = 1 .. 6 <> [last]
    makeList | skip > (last - 4) = [1] <> (last - 5) .. last
    makeList = [1] <> (skip - 2) .. (skip + 2) <> [last]

    makePagingButtons :: Array (HH.HTML p i)
    makePagingButtons =
      foldlWithIndex makePagingButtons' [] makeList

    makePagingButtons' :: Int -> Array (HH.HTML p i) -> Int -> Array (HH.HTML p i)
    makePagingButtons' 0 btns btn =
      (if skip == 1 then [] else [backArrow]) <> [button btn]
    makePagingButtons' 1 btns btn | btn /= 2 =
      btns <> [ellipsis] <> [button btn]
    makePagingButtons' idx btns btn | btn == last && last < 7 && skip /= last =
      btns <> [button btn] <> [forwardArrow]
    makePagingButtons' idx btns btn | btn == last && last < 7 =
      btns <> [button btn]
    makePagingButtons' idx btns btn | btn == last && skip < (last - 3) =
      btns <> [ellipsis] <> [button btn] <> [forwardArrow]
    makePagingButtons' idx btns btn | btn == last =
      btns <> [button btn] <> (if skip == last then [] else [forwardArrow])
    makePagingButtons' _ btns btn =
      btns <> [button btn]

    arrow :: Int -> String -> HH.HTML p i
    arrow skip' label =
      HH.li_
        [ HH.a
          [ HE.onClick $ query skip'
          ]
          [ HH.text label
          ]
        ]

    backArrow :: HH.HTML p i
    backArrow = arrow (skip - 1) "←"

    forwardArrow :: HH.HTML p i
    forwardArrow = arrow (skip + 1) "→"

    button :: Int -> HH.HTML p i
    button btn =
      HH.li_
        [ HH.a
          [ HP.class_ $ ClassName (if btn == skip then "disabled" else "")
          , HE.onClick $ query btn
          ]
          [ HH.text $ show btn
          ]
        ]

    ellipsis :: HH.HTML p i
    ellipsis =
      HH.li_
        [ HH.span_
          [ HH.text "…"
          ]
        ]

-----
-- A block for handling paging in list components

pagerNew ::
  forall p i.
  Int ->
  Int ->
  Array (HP.IProp HTMLdiv i) ->
  (Int -> MouseEvent -> i) ->
  HH.HTML p i
pagerNew skip last iprops query = HH.div iprops (makePagingButtonsNew skip last query)

makePagingButtonsNew ::
  forall p i.
  Int ->
  Int ->
  (Int -> MouseEvent -> i) ->
  Array (HH.HTML p i)
makePagingButtonsNew skip last query = foldlWithIndex makePagingButtons' [] makeList
  where
  makeList :: Array Int
  makeList | last < 2 = []
  makeList | last <= 6 = 1 .. last
  makeList | skip < 5 = 1 .. 6 <> [last]
  makeList | skip > (last - 4) = [1] <> (last - 5) .. last
  makeList = [1] <> (skip - 2) .. (skip + 2) <> [last]

  makePagingButtons' :: Int -> Array (HH.HTML p i) -> Int -> Array (HH.HTML p i)
  makePagingButtons' 0 btns btn =
    (if skip == 1 then [] else [backArrow]) <> [button btn]
  makePagingButtons' 1 btns btn | btn /= 2 =
    btns <> [ellipsis] <> [button btn]
  makePagingButtons' idx btns btn | btn == last && last < 7 && skip /= last =
    btns <> [button btn] <> [forwardArrow]
  makePagingButtons' idx btns btn | btn == last && last < 7 =
    btns <> [button btn]
  makePagingButtons' idx btns btn | btn == last && skip < (last - 3) =
    btns <> [ellipsis] <> [button btn] <> [forwardArrow]
  makePagingButtons' idx btns btn | btn == last =
    btns <> [button btn] <> (if skip == last then [] else [forwardArrow])
  makePagingButtons' _ btns btn =
    btns <> [button btn]

  arrow :: Int -> HH.HTML p i -> HH.HTML p i
  arrow skip' label =
    HH.button
    [ HE.onMouseDown $ query skip'
    , Ocelot.HTML.Properties.css "mx-4"
    , Ocelot.HTML.Properties.style <<< Foreign.Object.fromHomogeneous
      $ { "line-height": "2.5rem"
        }
    ]
    [ label ]

  backArrow :: HH.HTML p i
  backArrow = arrow (skip - 1) (Ocelot.Block.Icon.chevronLeft [ Ocelot.HTML.Properties.css "hover:text-grey"])

  forwardArrow :: HH.HTML p i
  forwardArrow = arrow (skip + 1) (Ocelot.Block.Icon.chevronRight [ Ocelot.HTML.Properties.css "hover:text-grey"])

  button :: Int -> HH.HTML p i
  button btn =
    HH.button
    (if btn == skip then disabled else active)
    [ HH.span_
      [ HH.text $ show btn ]
    ]
    where
    disabled =
      [ Ocelot.HTML.Properties.css "border border-black disabled focus:outline-none inline-block text-center"
      , Ocelot.HTML.Properties.style <<< Foreign.Object.fromHomogeneous
        $ { "border-radius": "50%"
          , "width": "2.5rem"
          , "line-height": "2.5rem"
          }
      ]

    active =
      [ Ocelot.HTML.Properties.css "inline-block hover:bg-grey hover:text-white text-center"
      , Ocelot.HTML.Properties.style <<< Foreign.Object.fromHomogeneous
        $ { "border-radius": "50%"
          , "width": "2.5rem"
          , "line-height": "2.5rem"
          }
      , HE.onMouseDown $ query btn
      ]

  ellipsis :: HH.HTML p i
  ellipsis =
    HH.span
    [ Ocelot.HTML.Properties.css "mx-2"]
    [ HH.text "…" ]
