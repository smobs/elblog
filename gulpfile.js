
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

gulp.task('make', function () {
  return purescript.psc({src: sources, ffi: foreigns});
});

gulp.task('psc-bundle', ['make'], function (){
  return purescript.pscBundle({src: "output/**/*.js", output: pscBundle});
});

gulp.task('bundle', ['psc-bundle'], function (){
  return gulp.src(pscBundle)
    .pipe(webpack({
      resolve: {moduleDirectories: ["node_modules"]},
      output: {filename: "bundle.js"}
    }))
    .pipe(gulp.dest("static/dist"));
});

gulp.task('default', ['bundle'])

gulp.task('heroku:prod', ['default']);
