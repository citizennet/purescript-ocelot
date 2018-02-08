const path = require('path')
const ExtractTextPlugin = require('extract-text-webpack-plugin')

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
	]
}
