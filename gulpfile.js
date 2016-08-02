
var bower = require('gulp-bower');
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

var cssSources = "static/css/**";

var resources = "static/resources/**";

gulp.task('bower', function() {
  return bower();
});

gulp.task('make', ['bower'], function () {
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

gulp.task("dotpsci", function () {
  return purescript.psci({ src: sources, ffi: foreigns })
    .pipe(gulp.dest("."));
});

gulp.task('bundle', ['psc-bundle'], function (){
  return gulp.src(pscBundle)
    .pipe(webpack({
      resolve: {moduleDirectories: ["node_modules"]},
      output: {filename: "index.js"}
    }))
    .pipe(gulp.dest("static/dist"));
});

gulp.task('css', function(){
  return gulp.src(cssSources)
    .pipe(gulp.dest("static/dist/css"));
});

gulp.task('resources', function () {
  return gulp.src(resources)
    .pipe(gulp.dest("static/dist/resources"));
});

gulp.task('default', ['resources','css','bundle', 'dotpsci'])

gulp.task('heroku:prod', ['default']);
