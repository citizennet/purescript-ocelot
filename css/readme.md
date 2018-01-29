# CSS

This folder contains our modified version of Tailwind.css.

- Use `npm run watch` when working on the CSS for automatic builds
- Use `npm run build-prod` when ready to save a new version


## Webpack

This project uses Webpack with PostCSS, PurgeCSS, and ExtractText plugins to generate a CSS file from the Tailwind configuration, remove any unused code based on our PureScript files, and minify the result.

In an initial test, this build process successfully outputs our CSS at just **4.66kb** as compared to the **256kb** output by Tailwind alone.
