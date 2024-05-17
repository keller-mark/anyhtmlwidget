# anyhtmlwidget

Bringing core concepts from [anywidget](https://github.com/manzt/anywidget) to R.

- Define widget as a JavaScript EcmaScript Module (ESM) string in R
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

## Bidirectional communication

At the moment, this project is using R [htmlwidgets](https://www.htmlwidgets.org/) under the hood.

When htmlwidgets are used in RStudio (rendered in the Viewer pane), they are simply printed as [static HTML](https://github.com/ramnathv/htmlwidgets/blob/373eedef8298c53c13f40bc442230d947884af59/R/htmlwidgets.R#L26).
This means there is no built-in mechanism for bidirectional communication or sending updates back to the R process.
In contrast, Python Jupyter widgets (and by extension, AnyWidgets) keep an active websocket connection back to the Python kernel to send updates.

When R htmlwidgets are running within Shiny apps, they have access to `Shiny.setInputValue()` which can be used to send data back to the Shiny process (i.e., updating values or triggering re-renders).
One option to enable bidirectional communication in both scenarios is to use Shiny for the RStudio Viewer usage as well.
This is the approach used by projects such as [Unravel](https://github.com/nischalshrestha/Unravel/blob/35f697761942847fe17a9c6de72d48e8998e9ec1/R/unravel.R#L14) which rely on both running in the Viewer pane and sending values back to the R process upon user interactions.


