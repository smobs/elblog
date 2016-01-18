
var gulp = require('gulp');
var purescript = require('gulp-purescript');
var webpack = require('webpack-stream');
var sources = [
  "static/purescript/src/**/*.purs",
  "bower_components/purescript-*/src/**/*.purs"
];

var foreigns = [
  "static/purescript/src/**/*.js",
  "bower_components/purescript-*/src/**/*.js"
];

var pscBundle = "static/dist/psc-bundle.js";

var cssSources = "static/css/**"

gulp.task('make', function () {
  return purescript.psc({src: sources, ffi: foreigns});
});

gulp.task('psc-bundle', ['make'], function (){
  return purescript.pscBundle({
    src: "output/**/*.js",
    output: pscBundle,
    module: "Main",
    main: "Main"
  });
});

gulp.task('bundle', ['psc-bundle'], function (){
  return gulp.src(pscBundle)
    .pipe(webpack({
      resolve: {moduleDirectories: ["node_modules"]},
      output: {filename: "bundle.js"}
    }))
    .pipe(gulp.dest("static/dist"));
});

gulp.task('css', function(){
  return gulp.src(cssSources)
    .pipe(gulp.dest("static/dist/css"));
});

gulp.task('default', ['css','bundle'])

gulp.task('heroku:prod', ['default']);
