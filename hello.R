esm <- "
function render({ el, model, width, height }) {
  el.style.border = '4px solid red';
  let btn = document.createElement('button');
  btn.innerHTML = `count is ${model.get('count')}`;

  el.appendChild(btn);
}
function resize({ el, width, height }) {
  el.style.width = width;
  el.style.height = height;

  console.log('from resize');
}
export default { render, resize };
"

values <- list(
  count = 1
)

widget <- anyhtmlwidget::AnyHtmlWidget$new(esm = esm, values = values)
widget

widget$count <- 2

widget$count <- 3
