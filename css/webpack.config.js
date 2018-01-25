let path = require('path');
let glob = require('glob-all');
let ExtractTextPlugin = require('extract-text-webpack-plugin');
let PurgecssPlugin = require('purgecss-webpack-plugin');
let OptimizeCssAssetsPlugin = require('optimize-css-assets-webpack-plugin');


// Custom PurgeCSS extractor for Tailwind that allows special characters in
// class names.
// https://github.com/FullHuman/purgecss#extractor
class TailwindExtractor {
  static extract(content) {
    return content.match(/[A-z0-9-:\/]+/g) || [];
  }
}

module.exports = {
  entry: './index.js',
  output: {
    path: path.resolve(__dirname, './dist'),
    filename: 'cn-tailwind.css'
  },
  module: {
    rules: [
      {
        test: /\.css$/,
        use: ExtractTextPlugin.extract({
          fallback: 'style-loader',
          use: [
            { loader: 'css-loader', options: { importLoaders: 1 } },
						// Use for Tailwind processing
            'postcss-loader'
          ]
        })
      }
    ]
  },
  plugins: [
		// Bundle CSS to separate file, not JS
    new ExtractTextPlugin('cn-tailwind.css'),

		// Remove class names not used in any source files
		new	PurgecssPlugin({
			// Locations of any files to scan
			paths: glob.sync([
				path.join(__dirname, "../src/**/*.purs"),
				path.join(__dirname, "../test/**/*.purs"),
				path.join(__dirname, "../docs/**/*.js"),
			]),
			extractors: [
				{
					extractor: TailwindExtractor,
					extensions: [ "js", "purs" ]
				}
			]
		}),

		// Minify the CSS after processing
		new OptimizeCssAssetsPlugin()

	]
}
