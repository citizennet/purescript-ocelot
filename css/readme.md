# CSS

This folder contains our modified version of Tailwind.css.

- Use `npm run watch` when working on the CSS for automatic builds
- Use `npm run build-prod` when ready to save a new version


## Webpack

This project uses Webpack with PostCSS, and ExtractText plugins to generate a CSS file from the Tailwind configuration and minify the result.

## Icons

Adding a new  icon is a manual process that involves a number of steps:

### 1. Prepare SVG's

The SVG's need to meet certain requirements so that the resulting glyphs will look right.

While a developer can do all of the following preparations – provided access to the necessary tools – it's perfectly reasonable to ask the Design team to deliver the assets already prepared to these requirements.

1. Remove all strokes. In fonts, strokes are ignored. If the SVG relies on strokes, those will need to be converted to paths.
    - In Illustrator you can do this conversion by selecting a glyph with a stroke, then using `Object > Expand...`.
1. Remove all colors. Fonts can't embed colors, but instead inherit the colors specified by the browser. Like strokes, fills will be ignored. Instead, fonts define fill by contour direction. However, it may be useful as a visual cue to those working with the SVG's to leave black fills.
1. Make sure all vectors are merged into one compound path.
    - In Illustrator you can do this by selecting all paths then using `Object > Compound Path > Make`.
1. Make sure that either the horizontal or vertical edges of the SVG artwork are flush against the SVG border. If this isn't done, the extra padding around the icon will make it appear shrunken.
    - You can make all edges flush against the border in Illustrator by using `Object > Artborads > Fit to Artwork Bounds`. However, this will usually conflict with the next requirement of having the width and height match, so do this step first.
1. Make sure that the SVG's width and height match. The font icons will all be given the same height, which means wide logos will end up looking larger and tall logos will end up looking shrunken.
    - In Illustrator you can do this by going to the Artboards pane (`Window > Artboards` if it's not currently shown), pressing the `Artboard Options` icon next to the current Artboard, then adjusting the smaller of the Height and Width dimensions to match the larger.

For instructions using the open-source vector editor, Inkscape, see [here][Inkscape instructions].

### 2. Convert to font

The next step is to convert the prepared SVG's to a font. At the time of this writing, [Fontello][] is a free online tool that's well suited for this task. 

1. Drag and drop your SVG's into the [Fontello][] web app.
    - N.B. You'll want to upload all SVG fonts, not just the new ones, because the generated font will need to contain all icons. You'll notice the current `css/src/icons.css` contains two different `@font-face` definitions, because the original SVG's for the first set of icons were lost. We've introduced the `css/svg` directory so that we don't lose the source SVG's going forward.
1. Select all the custom icons.
1. Select the "Customize Codes" tab.
1. For any pre-existing icons, make sure the code matches the `content` property for that icon in `css/src/icons.css`.
1. Press "Download webfont" to download the generated files.
1. Unpack the downloaded zip and find the `css/fontello-embedded.css` file.
1. Replace the `src` for the `@font-face` in `css/src/icons.css` with the `src` for `@font-face` in the generated CSS.
1. Add any new icon declarations to `css/src/icons.css`. You can get the correct `content` code from either the bottom of the generated CSS, or from the "Customize Codes" tab in [Fontello].

### 3. Add icon blocks

If you only updated existing icons, there's nothing to do here, but if there are any new icons, you'll need to add them to `Ocelot.Block.Icon`.

### 4. Update UI Guide

Again, if you only updated existing icons, there shouldn't be anything to update in UI Guide, but if you've added new icons, you'll want to update `UIGuide.Component.Icons` to showcase them.

### Troubleshooting

If there's an issue with the icons not displaying as expected in some or all browsers, it may be helpful to review the comments for [PR#130][].

[Fontello]: http://fontello.com/
[Inkscape instructions]: https://github.com/fontello/fontello/wiki/How-to-use-custom-images#preparing-images-in-inkscape
[PR#130]: /citizennet/purescript-ocelot/pull/130
