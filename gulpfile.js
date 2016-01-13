
var gulp = require('gulp');
var purescript = require('gulp-purescript');

var sources = [
  "static/purescript/src/**/*.purs",
  "bower_components/purescript-*/src/**/*.purs"
];

var foreigns = [
  "static/purescript/src/**/*.js",
  "bower_components/purescript-*/src/**/*.js"
];

gulp.task('make', function () {
  return purescript.psc({src: sources, ffi: foreigns});
});

gulp.task('bundle', ['make'], function (){
  return purescript.pscBundle({src: "output/**/*.js", output: "static/dist/bundle.js"});
})

gulp.task('default', ['bundle'])

gulp.task('heroku:prod', ['default']);
