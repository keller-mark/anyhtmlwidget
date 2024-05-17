# anyhtmlwidget

Bringing core concepts from [anywidget](https://github.com/manzt/anywidget) to R.

- Define widget as a JavaScript EcmaScript Module (ESM) string in R (no need for specialized JS files in `inst/`)
- Access state using a subset of the AnyWidget `model` API.
- Automatic re-renders upon value changes

Note: this is currently an experiment

## Installation

```R
devtools::install_github("keller-mark/anyhtmlwidget")
```

## Usage

```R
library(anyhtmlwidget)

esm <- "
function render({ el, model, width, height }) {
  el.style.border = '4px solid red';
  let btn = document.createElement('button');
  btn.innerHTML = `count is ${model.get('count')}`;
  el.appendChild(btn);
}
export default { render };
"

values <- list(
  count = 1
)

widget <- AnyHtmlWidget$new(esm = esm, values = values)
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
