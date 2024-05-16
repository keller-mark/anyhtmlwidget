esm <- "
function render({ el, width, height }) {
  el.style.border = '4px solid red';
  let btn = document.createElement('button');
  btn.innerHTML = `count button`;

  el.appendChild(btn);
}
function resize({ el, width, height }) {
  el.style.width = width;
  el.style.height = height;

  console.log('from resize');
}
export default { render, resize };
"

widget <- anyhtmlwidget::anyhtmlwidget(esm, width=400, height=300)
