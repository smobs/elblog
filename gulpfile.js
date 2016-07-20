
var bower = require('gulp-bower');
var gulp = require('gulp');
var webpack = require('webpack-stream');

var cssSources = "static/css/**";

var resources = "static/resources/**";

gulp.task('bower', function() {
  return bower();
});

gulp.task('webpack', ['bower'], function () {
  return gulp.src('static/entry.js')
    .pipe(webpack(require('./webpack.config.js')))
    .pipe(gulp.dest('static/dist/'))
});

gulp.task('css', function(){
  return gulp.src(cssSources)
    .pipe(gulp.dest("static/dist/css"));
});

gulp.task('resources', function () {
  return gulp.src(resources)
    .pipe(gulp.dest("static/dist/resources"));
});

gulp.task('default', ['resources','css','webpack'])

gulp.task('heroku:prod', ['default']);
