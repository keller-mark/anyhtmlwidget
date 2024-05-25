library(anyhtmlwidget)

esm <- "
function render({ el, model, width, height }) {
  console.log(window);
  console.log(model);
  let count = () => model.get('count');
  el.style.border = '4px solid red';
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

widget <- anyhtmlwidget::AnyHtmlWidget$new(esm = esm, mode = "gadget", count = 1)
widget


