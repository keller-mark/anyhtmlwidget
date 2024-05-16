library(shiny)
library(anyhtmlwidget)

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


ui <- fluidPage(
  "anyhtmlwidget",
  anyhtmlwidget_output(output_id = "my_widget"),
)

server <- function(input, output, session) {
  output$my_widget <- render_anyhtmlwidget(expr = {
    anyhtmlwidget(esm = esm)
  })
}

shinyApp(ui, server)
