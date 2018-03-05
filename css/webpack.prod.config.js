const merge = require('webpack-merge')
const dev = require('./webpack.dev.config.js')
const path = require('path')
const glob = require('glob-all')
const PurgecssPlugin = require('purgecss-webpack-plugin')
const OptimizeCssAssetsPlugin = require('optimize-css-assets-webpack-plugin')
const postCssConfig = require('./postcss.config')

// Custom PurgeCSS extractor for Tailwind that allows special characters in
// class names.
// https://github.com/FullHuman/purgecss#extractor
class TailwindExtractor {
  static extract(content) {
    return content.match(/[A-z0-9-:\/]+/g) || [];
  }
}

module.exports = env => {
  const dev_ = dev(env)
  const merged = merge(dev_, {
    output: {
      filename: dev_.output.filename.replace(/\.css$/, '.min.css')
    },
    plugins: [
      // Remove class names not used in any source files
      new PurgecssPlugin({
        // Locations of any files to scan
        paths: glob.sync([
          path.join(__dirname, "../dist/**/*.js")
        ]),
        whitelist: ['ocelot-localized'],
        extractors: [
          {
            extractor: TailwindExtractor,
            extensions: [ "js" ]
          }
        ]
      }),

      // Minify the CSS after processing
      new OptimizeCssAssetsPlugin()
    ]
  })
  console.log('merged', merged)
  return merged
}
