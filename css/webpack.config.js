const merge = require('webpack-merge')
const path = require('path')
const glob = require('glob-all')
const ExtractTextPlugin = require('extract-text-webpack-plugin')
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

module.exports = (env='') => {
  const prod = env.includes('prod')
  const scoped = env.includes('scoped')
  const filename = `cn-tailwind${scoped?'.scoped':''}${prod?'.min':''}.css`

  console.log(prod, scoped, filename)

  const config =
    { entry: './index.js'
    , output:
      { path: path.resolve(__dirname, '../dist')
      , filename
      }
    , module:
      { rules:
        [ { test: /\.css$/
          , use: ExtractTextPlugin.extract(
            { fallback: 'style-loader'
            , use:
              [ { loader: 'css-loader'
                , options: { importLoaders: 1 }
                }
              , { loader: 'postcss-loader'
                , options:
                  { ident: 'postcss'
                  , plugins: postCssConfig(scoped)
                  }
                }
              ]
            })
          }
        ]
      }
    , plugins: [ new ExtractTextPlugin(filename) ]
    }

  if (prod) {
    const merged = merge
      ( config
      , { plugins:
          [ // Remove class names not used in any source files
            new PurgecssPlugin
            ( { // Locations of any files to scan
                paths: glob.sync([
                  path.join(__dirname, "../dist/**/*.js")
                ])
              , whitelist: [ 'ocelot-scoped' ]
              , extractors:
                [ { extractor: TailwindExtractor
                  , extensions: [ "js" ]
                  }
                ]
              }
            )
          , // Minify the CSS after processing
            new OptimizeCssAssetsPlugin()
          ]
        }
      )
    console.log('config', merged)
    return merged
  }

  console.log('config', config)
  return config
}
