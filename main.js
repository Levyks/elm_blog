import './styles/main.css';
import '@github/time-elements';
import { Elm } from './src/Main.elm';

const baseUrl = import.meta.env.BASE_URL;

document.getElementById('base').setAttribute('href', baseUrl || '/')

Elm.Main.init({
  node: document.getElementById('app'),
  flags: baseUrl || ""
});