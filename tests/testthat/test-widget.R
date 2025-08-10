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

test_that("counter widget can be instantiated", {
  w <- AnyHtmlWidget$new(
    .esm = esm,
    .mode = "static",
    .height='400px',
    count = 1
  )

  expect_equal(w$count, 1)

  # Check that getters work
  expect_equal(w$.get_value("count"), 1)
  expect_equal(w$.get_esm(), esm)
  expect_equal(w$.get_values(), list(
    count = 1
  ))
  expect_equal(w$.get_width(), "100%")
  expect_equal(w$.get_height(), '400px')
  expect_equal(w$.get_mode(), "static")
  expect_equal(w$.get_host(), "0.0.0.0")
  expect_true(is.numeric(w$.get_port()))

  # Check that setters work
  w$.set_value("count", 3, emit_change = FALSE)
  expect_equal(w$.get_value("count"), 3)

  # Check that onChange handler works.
  # Create an empty list to track calls to the handler.
  change_list <<- list()
  handle_change <- function(key, new_val) {
    # Append to the list of { key, val }
    # pairs of tracked changes
    change_list <<- append(change_list,
      list(list(key = key, val = new_val))
    )
  }
  w$.on_change(handle_change)

  w$.set_value("count", 5, emit_change = FALSE)
  expect_equal(w$.get_value("count"), 5)
  expect_equal(length(change_list), 0)

  w$.set_value("count", 6, emit_change = TRUE)
  expect_equal(w$.get_value("count"), 6)
  expect_equal(length(change_list), 1)
  expect_equal(change_list[[1]], list(key = "count", val = 6))

  w$.set_value("count", 7, emit_change = TRUE)
  expect_equal(w$.get_value("count"), 7)
  expect_equal(length(change_list), 2)
  expect_equal(change_list[[1]], list(key = "count", val = 6))
  expect_equal(change_list[[2]], list(key = "count", val = 7))
})

test_that("invalid mode parameter value results in error", {
  expect_error(AnyHtmlWidget$new(
    .esm = esm,
    .mode = "INVALID",
    .height='400px',
    count = 1
  ), "Invalid widget mode.")
})

test_that("render return value reflects mode", {
  static_w <- AnyHtmlWidget$new(
    .esm = esm,
    .mode = "static",
    .height='400px',
    count = 1
  )

  render_val <- static_w$render(return_widget = TRUE)
  expect_equal(class(render_val), c("anyhtmlwidget", "htmlwidget"))
  render_val2 <- static_w$.get_htmlwidget()
  expect_equal(class(render_val2), c("anyhtmlwidget", "htmlwidget"))

  dynamic_w <- AnyHtmlWidget$new(
    .esm = esm,
    .mode = "dynamic",
    .height='400px',
    count = 1
  )
  render_val <- dynamic_w$render(return_widget = TRUE)
  expect_equal(class(render_val), c("anyhtmlwidget", "htmlwidget"))
  render_val2 <- static_w$.get_htmlwidget()
  expect_equal(class(render_val2), c("anyhtmlwidget", "htmlwidget"))
  
  shiny_w <- AnyHtmlWidget$new(
    .esm = esm,
    .mode = "shiny",
    .height='400px',
    count = 1
  )
  expect_error(shiny_w$render(return_widget = TRUE), "render is meant for use with static, gadget, and dynamic modes")
  expect_error(shiny_w$.get_htmlwidget(), ".get_htmlwidget is meant for use with static and dynamic modes")

  gadget_w <- AnyHtmlWidget$new(
    .esm = esm,
    .mode = "gadget",
    .height='400px',
    count = 1
  )
  render_val <- gadget_w$render(return_widget = TRUE)
  expect_equal(class(render_val), c("list"))
  expect_equal(names(render_val), c("ui", "server"))
  expect_error(gadget_w$.get_htmlwidget(), ".get_htmlwidget is meant for use with static and dynamic modes")

})