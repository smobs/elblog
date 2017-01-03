const validate = require("webpack-validator");
const path = require('path');
const webpack = require('webpack');
const config = 
      { debug: true
        , devtool: 'source-map'
        , devServer: { contentBase: '.'
                       , port: 4008
                       , stats: 'errors-only'
                     }
        , entry: './static/pong'
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
                               },
                               // That will tell Webpack that EaselJS refers to `window` with `this` and exports `window.createjs`.
                               { test: /bower_components\/EaselJS\/.*\.js$/, loader: 'imports?this=>window!exports?window.createjs' }
                               
                         , { test: /\.js$/
                           , loader: 'source-map-loader'
                           , exclude: /node_modules|bower_components/
                           }
                         ]
                  }
        , plugins: [
          // This will tell Webpack how to find the main file of bower modules
          new webpack.ResolverPlugin(
            new webpack.ResolverPlugin.DirectoryDescriptionFilePlugin('.bower.json', ['main'])
          )
        ]
        , resolve: { modulesDirectories: ['node_modules', 'bower_components'],
                     extensions: ['', '.js','.purs']
        }
};

module.exports = validate(config);
    
