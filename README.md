# anyhtmlwidget

Bringing core concepts from [anywidget](https://github.com/manzt/anywidget) to R.

- Define widget as a JavaScript EcmaScript Module (ESM) string in R
- Access state from JS using the same AnyWidget `model` API.
- Bidirectional communication (R <-> JS)

Note: this is currently an experiment

## Installation

```R
devtools::install_github("keller-mark/anyhtmlwidget")
```

## Usage

```R
library(anyhtmlwidget)

esm <- "
function render({ el, model }) {
  let count = () => model.get('count');
  let btn = document.createElement('button');
  btn.innerHTML = `count button ${count()}`;
  btn.addEventListener('click', () => {
    model.set('count', count() + 1);
    model.save_changes();
  });
  model.on('change:count', () => {
        btn.innerHTML = `count is ${count()}`;
  });
  el.appendChild(btn);
}
export default { render };
"

widget <- AnyHtmlWidget$new(.esm = esm, .mode = "static", count = 1)
widget
```

Setting a value will cause a re-render:

```R
widget$count <- 2
```

Access the latest values:

```R
widget$count
# [1] 2
```

## Modes

- `static`
- `gadget`
- `dynamic`
- `shiny`


