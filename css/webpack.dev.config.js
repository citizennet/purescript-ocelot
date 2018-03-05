const path = require('path')
const ExtractTextPlugin = require('extract-text-webpack-plugin')
const postCssConfig = require('./postcss.config')

module.exports = env => {
  const filename = env === 'localized' ?
    'cn-tailwind-localized.css' :
    'cn-tailwind.css'

  console.log('env', env)
  console.log('filename', filename)

  return {
    entry: './index.js',
    output: {
      path: path.resolve(__dirname, '../dist'),
      filename
    },
    module: {
      rules: [
        {
          test: /\.css$/,
          use: ExtractTextPlugin.extract({
            fallback: 'style-loader',
            use: [
              { loader: 'css-loader', options: { importLoaders: 1 } },
              'postcss-loader'
              // {
                // loader: 'postcss-loader',
                // options: {
                  // ident: 'postcss',
                  // plugins: postCssConfig(env)
                // }
              // }
            ]
          })
        }
      ]
    },
    plugins: [
      new ExtractTextPlugin(filename)
    ]
  }
}
