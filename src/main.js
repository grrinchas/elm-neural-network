import Elm from './app/Main.elm';
import './main.scss';
import {copyCanvas, initCanvas, render, getImageData} from "./Native/Canvas";

const mountNode = document.getElementById('app');
const app = Elm.Main.embed(mountNode);


app.ports.initCanvas.subscribe(function (canvas) {
    initCanvas(app, canvas);
});

app.ports.render.subscribe(function (stage) {
    render(stage, app)
});

app.ports.getImageData.subscribe(function (input) {
    getImageData(input, app);
});

app.ports.copyCanvas.subscribe(function (input) {
    copyCanvas(input, app);
});

if (module.hot) {
    module.hot.accept();
}
