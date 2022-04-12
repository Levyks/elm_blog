import './styles/main.css';
import '@github/time-elements';
import { Elm } from './src/Main.elm';

const baseUrl = import.meta.env.BASE_URL;

document.getElementById('base').setAttribute('href', baseUrl || '/')

const app = Elm.Main.init({
  node: document.getElementById('app'),
  flags: baseUrl || ""
});

/**
 * I shouldn't be using JS to do this, but it was the easiest way
 * And I wanted to get a feel for how ports work, so:
 * 
 * TODO: handle this in Elm itself
 */
app.ports.updateQueryParam.subscribe(([key, value]) => {
  const url = new URL(window.location.href);
  if(value) {
    url.searchParams.set(key, value);
  } else {
    url.searchParams.delete(key);
  }
  history.pushState({}, '', url.toString());
})