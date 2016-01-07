
var gulp = require('gulp');
var elm = require('gulp-elm');

gulp.task('elm-init', elm.init)

gulp.task('elm', ['elm-init'], function () {
  return gulp.src('elm/Main.elm')
    .pipe(elm())
    .pipe(gulp.dest('dist/'));
});

gulp.task('default', ['elm'])

gulp.task('heroku:prod', ['default']);
