library(shiny)
library(anyhtmlwidget)

esm <- "
function render({ el, model, width, height }) {
  console.log(model);
  let count = () => model.get('value');
  el.style.border = '4px solid red';
  let btn = document.createElement('button');
  btn.innerHTML = `count button ${count()}`;
  btn.addEventListener('click', () => {
    model.set('value', count() + 1);
    model.save_changes();
  });

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
  verbatimTextOutput("values")
)

server <- function(input, output, session) {
  rv <- reactiveValues(current=list(value = 1))

  observeEvent(input$anyhtmlwidget_on_save_changes, {
    # We can access any values from the coordination space here.
    # In this example, we access the ID of the currently-hovered cell.
    rv$current <- input$anyhtmlwidget_on_save_changes
  })
  output$values <- renderPrint({ rv$current })

  output$my_widget <- render_anyhtmlwidget(expr = {
    my_anyhtmlwidget(esm = esm, values = rv$current)
  })
}

shinyApp(ui, server)
