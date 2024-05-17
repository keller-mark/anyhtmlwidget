#' Vitessce Widget
#'
#' This function creates a new Vitessce htmlwidget.
#' A Vitessce widget is defined by a config which specifies dataset(s),
#' a layout of views, and linked view connections. A config object can be
#' passed to the widget using the \code{config} parameter.
#'
#' We do not recommend calling this function directly. Instead, we
#' recommend calling the \code{widget()} method on the \code{VitessceConfig}
#' instance.
#'
#' @param config A view config as a `VitessceConfig` object.
#' @param theme The theme of the widget, either "dark" or "light". Optional. By default, "dark".
#' @param width The width of the widget as a number or CSS string. Optional.
#' @param height The height of the widget as a number or CSS string. Optional.
#' @param port The port for the local web server (which serves local dataset objects to the widget).
#' Optional. By default, uses open port between 8000 and 9000.
#' @param base_url The base URL for the web server. Optional.
#' By default, creates a localhost URL which includes the port.
#' @param serve Should local data be served by running a local web server with R plumber? By default, TRUE.
#' @param element_id An element ID. Optional.
#' @return The htmlwidget.
#'
#' @export
#'
#' @examples
#' ESM <- "function render() {}
#' anyhtmlwidget()
my_anyhtmlwidget <- function(esm, values = NULL, width = NULL, height = NULL, element_id = NULL) {

  # forward widget options to javascript
  params = list(
    esm = esm,
    values = values
  )

  # create widget
  htmlwidgets::createWidget(
    name = 'anyhtmlwidget',
    params,
    width = width,
    height = height,
    package = 'anyhtmlwidget',
    elementId = element_id,
    sizingPolicy = htmlwidgets::sizingPolicy(
      viewer.padding = 0,
      browser.padding = 0,
      browser.fill = TRUE
    )
  )
}

#' Shiny bindings for anyhtmlwidget
#'
#' Output and render functions for using anyhtmlwidget within Shiny
#' applications and interactive Rmd documents.
#'
#' @param output_id output variable to read from
#' @param width,height Must be a valid CSS unit (like \code{'100\%'},
#'   \code{'400px'}, \code{'auto'}) or a number, which will be coerced to a
#'   string and have \code{'px'} appended.
#' @param expr An expression that generates a vitessce
#' @param env The environment in which to evaluate \code{expr}.
#' @param quoted Is \code{expr} a quoted expression (with \code{quote()})? This
#'   is useful if you want to save an expression in a variable.
#' @return The Shiny UI element.
#'
#' @rdname anyhtmlwidget-shiny
#' @export
anyhtmlwidget_output <- function(output_id, width = '100%', height = '400px'){
  htmlwidgets::shinyWidgetOutput(output_id, 'anyhtmlwidget', width, height, package = 'anyhtmlwidget')
}

#' @name anyhtmlwidget-shiny
#' @return The Shiny server output.
#' @export
render_anyhtmlwidget <- function(expr, env = parent.frame(), quoted = FALSE) {
  if (!quoted) { expr <- substitute(expr) } # force quoted
  htmlwidgets::shinyRenderWidget(expr, anyhtmlwidget_output, env, quoted = TRUE)
}

#' @export
AnyHtmlWidget <- R6::R6Class("AnyHtmlWidget",
  lock_objects = FALSE,
  private = list(
    esm = NULL,
    values = NULL
  ),
  active = list(

  ),
  public = list(
    initialize = function(esm = NA, values = NA) {
      private$esm <- esm
      private$values <- values

      active_env <- self$`.__enclos_env__`$`.__active__`

      # TODO: check that values is not NA
      for(key in names(values)) {
        active_binding <- function(val) {
          if(missing(val)) {
            return(private$values[[key]])
          } else {
            private$values[[key]] <- val
            self$print()
          }
        }
        active_env[[key]] <- active_binding
        makeActiveBinding(key, active_env[[key]], self)
      }
      self$`.__enclos_env__`$`.__active__` <- active_env

    },
    print = function() {
      print(self$render())
    },
    render = function() {
      w <- my_anyhtmlwidget(
        esm = private$esm,
        values = private$values,
        width = 400,
        height = 600
      )
      return(w)
    }
  )
)
