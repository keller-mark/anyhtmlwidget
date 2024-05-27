library(anyhtmlwidget)

esm <- "
function render({ el, model }) {
  el.style.border = '4px solid red';
  let count = () => model.get('count');
  let btn = document.createElement('button');
  btn.innerHTML = `count is ${count()}`;
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

widget <- anyhtmlwidget::AnyHtmlWidget$new(.esm = esm, .mode = "dynamic", count = 1)
widget
