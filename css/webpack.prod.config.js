const merge = require('webpack-merge')
const dev = require('./webpack.dev.config.js')
const path = require('path')
const glob = require('glob-all')
const PurgecssPlugin = require('purgecss-webpack-plugin')
const OptimizeCssAssetsPlugin = require('optimize-css-assets-webpack-plugin')


// Custom PurgeCSS extractor for Tailwind that allows special characters in
// class names.
// https://github.com/FullHuman/purgecss#extractor
class TailwindExtractor {
  static extract(content) {
    return content.match(/[A-z0-9-:\/]+/g) || [];
  }
}

module.exports = merge(dev, {
  plugins: [
		// Remove class names not used in any source files
		new	PurgecssPlugin({
			// Locations of any files to scan
			paths: glob.sync([
				path.join(__dirname, "../docs/**/*.js")
			]),
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
