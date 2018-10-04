import './main.css';
import { Elm } from './Main.elm';
import registerServiceWorker from './registerServiceWorker';
import { start } from './codemirror.js';

Elm.Main.init({
	node: document.getElementById('root')
});

start();

registerServiceWorker();
