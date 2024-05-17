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

widget <- anyhtmlwidget::AnyHtmlWidget$new(esm = esm, values = values)
widget

widget$count <- 2

widget$count <- 3

widget$count
