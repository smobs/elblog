const Pong = require("./purescript/src/Pong.purs")
window.onload = function () {
  const canvas = document.createElement('canvas');
  canvas.id = 'Foo';
  canvas.width = Pong.canvasSize.w;
  canvas.height = Pong.canvasSize.h;

  console.log('hello');
  document.body.appendChild(canvas);

  const state = Pong.initial;
  console.log('rendering...');
  Pong.renderPong('Foo')(state)();
  console.log('done');
};

