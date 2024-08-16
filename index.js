import { Elm } from './src/Main.elm'

const storedTheme = localStorage.getItem('THEME');

var app = Elm.Main.init({
  node: document.querySelector('main'), 
  flags: storedTheme
});


app.ports.saveProperty.subscribe(function(tuple) {
    const key = tuple[0];
    const value = tuple[1];
    localStorage.setItem(key,value);
});

