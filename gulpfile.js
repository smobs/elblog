
var gulp = require('gulp');
var elm = require('gulp-elm');
var purescript = require('gulp-purescript');

var sources = [
  "static/purescript/**/*.purs",
  "bower_components/purescript-*/**/*.purs"
];

var foreigns = [
  "static/purescript/**/*.js",
  "bower_components/purescript-*/**/*.js"
];

gulp.task('make', function () {
  return purescript.psc({src: sources, ffi: foreigns});
});

gulp.task('bundle', ['make'], function (){
  return purescript.pscBundle({src: "output/**/*.js", output: "static/dist/Main.js"});
})

gulp.task('default', ['bundle'])

gulp.task('heroku:prod', ['default']);
