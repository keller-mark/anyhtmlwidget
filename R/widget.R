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
the_anyhtmlwidget <- function(esm, values = NULL, ns_id = NULL, width = NULL, height = NULL, element_id = NULL) {

  # forward widget options to javascript
  params = list(
    esm = esm,
    values = values,
    ns_id = ns_id
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
    values = NULL,
    mode = NULL,
    change_handler = NULL
  ),
  active = list(

  ),
  public = list(
    initialize = function(esm = NA, mode = NA, ...) {
      private$esm <- esm
      private$values <- list(...)

      if(is.na(mode)) {
        mode <- "static"
      }
      if(!mode %in% c("static", "gadget", "shiny", "dynamic")) {
        stop("Invalid widget mode.")
      }
      private$mode <- mode

      active_env <- self$`.__enclos_env__`$`.__active__`

      # TODO: check that values is not NA
      for(key in names(private$values)) {
        active_binding <- function(val) {
          if(missing(val)) {
            return(self$get_value(key))
          } else {
            self$set_value(key, val)
            if(private$mode == "static") {
              self$print()
            }
          }
        }
        active_env[[key]] <- active_binding
        makeActiveBinding(key, active_env[[key]], self)
      }
      self$`.__enclos_env__`$`.__active__` <- active_env
    },
    set_value = function(key, val, emit_change = TRUE) {
      private$values[[key]] <- val
      if(emit_change && !is.null(private$change_handler)) {
        # Should this only call the callback if the current value is different than the new value?
        private$change_handler(key, val)
      }
    },
    on_change = function(callback) {
      private$change_handler <- callback
    },
    get_value = function(key) {
      return(private$values[[key]])
    },
    get_esm = function() {
      return(private$esm)
    },
    get_values = function() {
      return(private$values)
    },
    set_values = function(new_values) {
      private$values <- new_values
    },
    set_mode = function(mode) {
      if(!mode %in% c("static", "gadget", "shiny", "dynamic")) {
        stop("Invalid widget mode.")
      }
      private$mode <- mode
    },
    print = function() {
      if(private$mode == "shiny") {
        # If Shiny mode, we just want to use the original R6 print behavior.
        # Reference: https://github.com/r-lib/R6/blob/507867875fdeaffbe7f7038291256b798f6bb042/R/print.R#L35C5-L35C36
        print(cat(format(self), sep = "\n"))
      } else {
        # Otherwise, we want to render the widget.
        self$render()
      }
    },
    render = function() {
      if(private$mode == "static") {
        invoke_static(self)
      } else if(private$mode == "gadget") {
        invoke_gadget(self)
      }
    }
  )
)

invoke_static <- function(w) {
  w <- the_anyhtmlwidget(
    esm = w$get_esm(),
    values = w$get_values(),
    width = 400,
    height = 600
  )
  print(w)
}

invoke_gadget <- function(w) {
  require(shiny)

  ui <- fluidPage(
    anyhtmlwidget_output(output_id = "my_widget"),
    verbatimTextOutput("values"),
    shiny::actionButton("go", label = "Go")
  )

  server <- function(input, output, session) {
    increment <- reactiveVal(0)

    observeEvent(input$anyhtmlwidget_on_save_changes, {
      # update values on w here
      for(key in names(input$anyhtmlwidget_on_save_changes)) {
        w$set_value(key, input$anyhtmlwidget_on_save_changes[[key]], emit_change = FALSE)
      }
      increment(increment() + 1)
    })
    output$values <- renderPrint({
      increment()
      w$get_values()
    })

    observeEvent(input$go, {
      w$count <- 999
      increment(increment() + 1)
    })

    w$on_change(function(key, new_val) {
      session$sendCustomMessage("anyhtmlwidget_on_change", list(key = key, value = new_val))
    })

    output$my_widget <- render_anyhtmlwidget(expr = {
      the_anyhtmlwidget(esm = w$get_esm(), values = w$get_values())
    })
  }

  runGadget(ui, server)
}

# Shiny module UI
widgetUI <- function(id) {
  ns <- NS(id)
  anyhtmlwidget_output(output_id = ns("widget"))
}

# Shiny module server
widgetServer <- function(id, w) {
  ns <- NS(id)
  moduleServer(
    id,
    function(input, output, session) {
      initial_values <- w$get_values()
      rv <- do.call(reactiveValues, initial_values)

      observeEvent(input$anyhtmlwidget_on_save_changes, {
        # update values on w here
        for(key in names(input$anyhtmlwidget_on_save_changes)) {
          rv[[key]] <- input$anyhtmlwidget_on_save_changes[[key]]
          w$set_value(key, input$anyhtmlwidget_on_save_changes[[key]], emit_change = FALSE)
        }
      })

      for(key in names(initial_values)) {
        observeEvent(rv[[key]], {
          session$sendCustomMessage(ns("anyhtmlwidget_on_change"), list(key = key, value = rv[[key]]))
        })
      }

      output$widget <- render_anyhtmlwidget(expr = {
        the_anyhtmlwidget(esm = w$get_esm(), values = initial_values, ns_id = id)
      })

      return(rv)
    }
  )
}
