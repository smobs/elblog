const validate = require("webpack-validator");
const path = require('path');

const config = 
      { debug: true
        , devtool: 'source-map'
        , devServer: { contentBase: '.'
                       , port: 4008
                       , stats: 'errors-only'
                     }
        , entry: './static/entry'
        , output: {path: path.join(__dirname, "static/dist")
                   , pathinfo: true
                   , filename: "bundle.js"
                  }
        , module: { loaders: [ { test: /\.purs$/
                           , loader: 'purs-loader'
                           , query: { src: [ 'bower_components/purescript-*/src/**/*.purs', 'static/purescript/src/**/*.purs' ]
                                    , bundle: false
                                    , psc: 'node_modules/purescript/bin/psc.js'
                                    , pscBundle: 'node_modules/purescript/bin/psc-bundle.js'
                                    , pscArgs: { sourceMaps: true }
                                    , pscIde: true
                                    }
                           }
                         , { test: /\.js$/
                           , loader: 'source-map-loader'
                           , exclude: /node_modules|bower_components/
                           }
                         ]
              }
        , resolve: { modulesDirectories: ['node_modules', 'bower_components'],
                     extensions: ['', '.js','.purs']
        }
};

module.exports = validate(config);
    
