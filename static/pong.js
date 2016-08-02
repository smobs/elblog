const Pong = require("./purescript/src/Pong.purs")
window.onload = function () {
  const canvas = document.createElement('canvas');
  const cId = 'Foo';
  canvas.id = cId;
  canvas.width = Pong.canvasSize.w;
  canvas.height = Pong.canvasSize.h;

  console.log('hello');
  document.body.appendChild(canvas);

  var state = Pong.initial;
  var render = function() {
    console.log('rendering...');
    Pong.renderPong(cId)(state)();
    console.log('done');
  };
  render();
  const addButton = function(text, command){
    const pongButton = document.createElement('button');
    pongButton.textContent = text;
    pongButton.onclick = function () {
      state = Pong.sendCommand(command)(state);
      render();
    };
    document.body.appendChild(pongButton);
  };

  const OneDown = Pong.MovePlayer.create({player: Pong.One.value, move: Pong.Down.value});
  addButton("P1 Down", OneDown);
  const OneUp = Pong.MovePlayer.create({player: Pong.One.value, move: Pong.Up.value});
  addButton("P1 Up", OneUp);
  const TwoDown = Pong.MovePlayer.create({player: Pong.Two.value, move: Pong.Down.value});
  addButton("P2 Down", TwoDown);
  const TwoUp = Pong.MovePlayer.create({player: Pong.Two.value, move: Pong.Up.value});
  addButton("P2 Up", TwoUp);
};

