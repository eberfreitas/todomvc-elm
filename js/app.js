import { Elm } from "../src/Main.elm";

const model = localStorage.getItem("todos-elm") || "";

const app = Elm.Main.init({
	node: document.getElementById("app"),
	flags: model
});

app.ports.sendModel.subscribe((model) => {
	localStorage.setItem("todos-elm", JSON.stringify(model));
});
