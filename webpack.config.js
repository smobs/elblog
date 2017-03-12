const path = require('path');
const webpack = require('webpack');
const ExtractTextPlugin = require('extract-text-webpack-plugin');

const ignorePaths = /node_modules|bower_components/
const config = 
      { devtool: 'source-map'
      , devServer: { contentBase: '.'
                     , port: 4008
                     , stats: 'errors-only'
                   }
      , entry: './static/entry'
      , output: {path: path.join(__dirname, "static/dist")
                 , pathinfo: true
                 , filename: "bundle.js"
                }
      , module: { 
          rules: [ { test: /\.purs$/
                   , loader: 'purs-loader'
                   , options: { src: [ 'bower_components/purescript-*/src/**/*.purs', 'static/purescript/src/**/*.purs', 'static/purescript/generated/**/*.purs' ]
                            , bundle: false
                            , psc: 'node_modules/purescript/bin/psc.js'
                            , pscBundle: 'node_modules/purescript/bin/psc-bundle.js'
                            , pscArgs: { sourceMaps: true }
                            , pscIde: true
                            , watch: true
                            }
                       }
                       , { test: /\.js$/
                         , loader: 'babel-loader'
                         , exclude: ignorePaths
                         , options: {presets: ['es2015']}
                        }
                       , { test: /\.css$/
                         , use: ExtractTextPlugin.extract({
                              use: [ { loader: "css-loader"
                                     , options: { sourceMap: true }}
                                   ]})}
                       , { test: /\.jpg$/
                         , use: [{ loader: "url-loader"
                                 , options: { name: "[name].[ext]"
                                            , publicPath: "static/resources/"
                                            , outputPath: "resources/"
                                            , limit: 10000}}]}
                       ]
                }
      ,  plugins: [ new ExtractTextPlugin({filename: './style.css'})]
                
};

module.exports = config;
    
