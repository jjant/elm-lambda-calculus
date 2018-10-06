import './main.css';
import { Elm } from './Main.elm';
import registerServiceWorker from './registerServiceWorker';
import { start } from './codemirror.js';

const app = Elm.Main.init({
	node: document.getElementById('root')
});

start();

app.ports.codeToJs.subscribe(b => {
	const toNumber = churchNumber => churchNumber(a => a + 1)(0);

	try {
		const fn = eval(b);

		app.ports.fromJs.send(
			typeof fn === 'function' ? '<function>' : String(toNumber(fn))
		);
	} catch (_) {
		app.ports.fromJs.send('');
	}
});

registerServiceWorker();
