import './styles/main.css';
import '@github/time-elements';
import { Elm } from './src/Main.elm';

Elm.Main.init({
  node: document.getElementById('app')
});