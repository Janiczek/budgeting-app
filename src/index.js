import './style.css';
import {Elm} from './Main.elm';
const localStorageKey = 'elm-app';

const savedModel = localStorage.getItem(localStorageKey);
const idSeed = +new Date();

const app = Elm.Main.init({
  node: document.getElementById('app'),
  flags: { savedModel, idSeed },
});

app.ports.saveToLocalStorage.subscribe(string => {
  localStorage.setItem(localStorageKey,string);
});
