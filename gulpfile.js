
var gulp = require('gulp');
var elm = require('gulp-elm');

gulp.task('elm', function () {
  return gulp.src('elm/Main.elm')
    .pipe(elm())
    .pipe(gulp.dest('dist/'));
});

gulp.task('default', ['elm'])

gulp.task('heroku:prod', ['default']);
