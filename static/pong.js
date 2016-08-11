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
    console.log(state.score.one + ':' state.score.two)
    console.log('done');
  };
  render();
  const addButton = function(text, command){
    const pongButton = document.createElement('button');
    pongButton.textContent = text;
    pongButton.onclick = function () {
      state = Pong.sendCommand(command)(state);
    };
    document.body.appendChild(pongButton);
  };
  
  const OneDown = Pong.MovePlayer.create({player: Pong.One.value, move: Pong.Direction.create(Pong.Down.value)});
  addButton("P1 Down", OneDown);
  const OneStop = Pong.MovePlayer.create({player: Pong.One.value, move: Pong.Stop.value});
  addButton("P1 Stop", OneStop);

  const OneUp = Pong.MovePlayer.create({player: Pong.One.value, move: Pong.Direction.create(Pong.Up.value)});
  addButton("P1 Up", OneUp);
  const TwoDown = Pong.MovePlayer.create({player: Pong.Two.value, move: Pong.Direction.create(Pong.Down.value)});
  addButton("P2 Down", TwoDown);
  const TwoStop = Pong.MovePlayer.create({player: Pong.Two.value, move: Pong.Stop.value});
  addButton("P2 Stop", TwoStop);
  const TwoUp = Pong.MovePlayer.create({player: Pong.Two.value, move: Pong.Direction.create(Pong.Up.value)});
  addButton("P2 Up", TwoUp);

  const StepGame = Pong.Step.value;
  addButton("Step", StepGame);

  window.setInterval(function () {
      state = Pong.sendCommand(StepGame)(state);
      render();
    }, 100);
  
};

