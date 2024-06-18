#' The internal function that creates the htmlwidget.
#' @param esm The ES Module as a string.
#' @param values The values that will be used for the initial Model state.
#' @param ns_id Namespace ID, only used when in the Shiny module mode. Optional.
#' @param width The width of the widget as a number or CSS string. Optional.
#' @param height The height of the widget as a number or CSS string. Optional.
#' @param port The port of the WebSocket server, when in dynamic mode. Optional.
#' @param host The host of the WebSocket server, when in dynamic mode. Optional.
#' @param element_id An element ID. Optional.
#' @return The result of htmlwidgets::createWidget.
#'
#' @keywords internal
the_anyhtmlwidget <- function(esm, values = NULL, ns_id = NULL, width = NULL, height = NULL, port = NULL, host = NULL, element_id = NULL) {
  params = list(
    esm = esm,
    values = values,
    ns_id = ns_id,
    port = port,
    host = host
  )

  htmlwidgets::createWidget(
    'anyhtmlwidget',
    params,
    width = width,
    height = height,
    package = 'anyhtmlwidget',
    elementId = element_id,
    sizingPolicy = htmlwidgets::sizingPolicy(
      viewer.padding = 0,
      browser.padding = 0,
      browser.fill = TRUE,
      viewer.fill = TRUE
    )
  )
}

#' Internal Shiny UI binding for anyhtmlwidget.
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
#' @keywords internal
anyhtmlwidget_output <- function(output_id, width = '100%', height = '400px'){
  htmlwidgets::shinyWidgetOutput(output_id, 'anyhtmlwidget', width, height, package = 'anyhtmlwidget')
}

#' Internal Shiny server binding for anyhtmlwidget.
#'
#' @keywords internal
render_anyhtmlwidget <- function(expr, env = parent.frame(), quoted = FALSE) {
  if (!quoted) { expr <- substitute(expr) } # force quoted
  htmlwidgets::shinyRenderWidget(expr, anyhtmlwidget_output, env, quoted = TRUE)
}

#' AnyHtmlWidget
#' @title AnyHtmlWidget Class
#' @docType class
#' @description
#' Class representing a widget.
#'
#' @rdname AnyHtmlWidget
#' @export
AnyHtmlWidget <- R6::R6Class("AnyHtmlWidget",
  lock_objects = FALSE,
  private = list(
    # TODO: prefix with dot
    # since R6 requires all items in
    # public, private, and active to have unique names
    .esm = NULL,
    .values = NULL,
    .mode = NULL,
    .change_handler = NULL,
    .server = NULL,
    .server_host = NULL,
    .server_port = NULL,
    .width = NULL,
    .height = NULL
  ),
  active = list(),
  public = list(
    #' @description
    #' Create a new widget instance.
    #' @param .esm The EcmaScript module as a string.
    #' @param .mode The widget mode.
    #' @param .width The widget width. Optional.
    #' @param .height The widget height. Optional.
    #' @param .commands TODO
    #' @param ... All other named arguments will be used to create active bindings on the instance.
    initialize = function(.esm, .mode, .width = NA, .height = NA, .commands = NA, ...) {
      private$.esm <- .esm
      private$.values <- list(...)

      if(is.na(.width)) {
        .width <- "100%"
      }
      if(is.na(.height)) {
        .height <- "100%"
      }

      private$.width <- .width
      private$.height <- .height

      private$.server_host <- "0.0.0.0"
      private$.server_port <- httpuv::randomPort(min = 8000, max = 9000, n = 1000)

      if(!.mode %in% c("static", "gadget", "shiny", "dynamic")) {
        stop("Invalid widget mode.")
      }
      private$.mode <- .mode

      active_env <- self$`.__enclos_env__`$`.__active__`

      # TODO: check that values is not NA
      for(key in names(private$.values)) {
        active_binding <- function(val) {
          if(missing(val)) {
            return(self$.get_value(key))
          } else {
            self$.set_value(key, val)
            if(private$.mode == "static") {
              self$print()
            }
          }
        }
        active_env[[key]] <- active_binding
        makeActiveBinding(key, active_env[[key]], self)
      }
      self$`.__enclos_env__`$`.__active__` <- active_env
    },
    #' @description
    #' Set a value.
    #' @param key The key of the value to set.
    #' @param val The new value.
    #' @param emit_change Should the .on_change handler be called?
    .set_value = function(key, val, emit_change = TRUE) {
      private$.values[[key]] <- val
      if(emit_change && !is.null(private$.change_handler)) {
        # Should this only call the callback if the current value is different than the new value?
        private$.change_handler(key, val)
      }
    },
    #' @description
    #' Register a change handler to call if emit_change is TRUE in .set_value.
    #' @param callback A callback function to register.
    .on_change = function(callback) {
      private$.change_handler <- callback
    },
    #' @description
    #' Get a particular value.
    #' @param key The key of the value to get.
    #' @returns The value.
   .get_value = function(key) {
      return(private$.values[[key]])
    },
    #' @description
    #' Get the ESM string.
    #' @returns The ESM string.
    .get_esm = function() {
      return(private$.esm)
    },
    #' @description
    #' Get all widget values
    #' @returns List of values.
    .get_values = function() {
      return(private$.values)
    },
    #' @description
    #' Get the widget width.
    #' @returns The width.
    .get_width = function() {
      return(private$.width)
    },
    #' @description
    #' Get the widget height.
    #' @returns The height.
    .get_height = function() {
      return(private$.height)
    },
    #' @description
    #' Set all values. TODO: is this ever used?
    #' @param new_values A list of new values.
    .set_values = function(new_values) {
      private$.values <- new_values
    },
    #' @description
    #' Set the widget mode.
    #' @param mode The new widget mode.
    .set_mode = function(mode) {
      if(!mode %in% c("static", "gadget", "shiny", "dynamic")) {
        stop("Invalid widget mode.")
      }
      private$.mode <- mode
    },
    #' @description
    #' Start the server, if not running.
    .start_server = function() {
      if(is.null(private$.server)) {
        private$.server <- start_server(self, host = private$.server_host, port = private$.server_port)
      }
    },
    #' @description
    #' Stop the server, if running.
    .stop_server = function() {
      if(!is.null(private$.server)) {
        private$.server$stop()
      }
    },
    #' @description
    #' Get the server hostname.
    #' @return The hostname as a string.
    .get_host = function() {
      return(private$.server_host)
    },
    #' @description
    #' Get the server port.
    #' @returns The port number.
    .get_port = function() {
      return(private$.server_port)
    },
    #' @description
    #' Custom print function for the R6 class.
    #' If mode is "shiny", falls back to original R6 print behavior.
    #' Otherwise, renders the widget.
    print = function() {
      if(private$.mode == "shiny") {
        # If Shiny mode, we just want to use the original R6 print behavior.
        # Reference: https://github.com/r-lib/R6/blob/507867875fdeaffbe7f7038291256b798f6bb042/R/print.R#L35C5-L35C36
        print(cat(format(self), sep = "\n"))
      } else {
        # Otherwise, we want to render the widget.
        self$render()
      }
    },
    #' @description
    #' Render the widget.
    render = function() {
      if(private$.mode == "static") {
        invoke_static(self)
      } else if(private$.mode == "gadget") {
        invoke_gadget(self)
      } else if(private$.mode == "dynamic") {
        invoke_dynamic(self)
      } else {
        stop("render is meant for use with static, gadget, and dynamic modes")
      }
    }
  )
)

#' @keywords internal
invoke_static <- function(w) {
  w <- the_anyhtmlwidget(
    esm = w$.get_esm(),
    values = w$.get_values(),
    width = w$.get_width(),
    height = w$.get_height()
  )
  print(w)
}

#' @keywords internal
invoke_dynamic <- function(w) {
  w$.start_server()
  w <- the_anyhtmlwidget(
    esm = w$.get_esm(),
    values = w$.get_values(),
    width = w$.get_width(),
    height = w$.get_height(),
    port = w$.get_port(),
    host = w$.get_host()
  )
  print(w)
}

#' @keywords internal
invoke_gadget <- function(w) {
  ui <- shiny::tagList(
    anyhtmlwidget_output(output_id = "my_widget", width = '100%', height = '100%')
  )

  server <- function(input, output, session) {
    increment <- shiny::reactiveVal(0)

    shiny::observeEvent(input$anyhtmlwidget_on_save_changes, {
      # update values on w here
      for(key in names(input$anyhtmlwidget_on_save_changes)) {
        w$.set_value(key, input$anyhtmlwidget_on_save_changes[[key]], emit_change = FALSE)
      }
      increment(increment() + 1)
    })

    # output$values <- renderPrint({
    #   increment()
    #   w$.get_values()
    # })
    #
    # observeEvent(input$go, {
    #   w$count <- 999
    #   increment(increment() + 1)
    # })

    w$.on_change(function(key, new_val) {
      session$sendCustomMessage("anyhtmlwidget_on_change", list(key = key, value = new_val))
    })

    output$my_widget <- render_anyhtmlwidget(expr = {
      the_anyhtmlwidget(esm = w$.get_esm(), values = w$.get_values(), width = w$.get_width(), height = w$.get_height())
    })
  }

  shiny::runGadget(ui, server)
}

#' Shiny module UI for anyhtmlwidgets.
#'
#' @param id The output ID.
#' @param width The widget width. Optional.
#' @param height The widget height. Optional.
#'
#' @export
widgetUI <- function(id, width = '100%', height = '400px') {
  ns <- shiny::NS(id)
  anyhtmlwidget_output(output_id = ns("widget"), width = width, height = height)
}

#' Shiny module server for anyhtmlwidgets.
#'
#' @param id The matching output ID used in the Shiny UI.
#' @param w The widget instance.
#' @returns reactiveValues corresponding to the widget's active bindings.
#'
#' @export
widgetServer <- function(id, w) {
  ns <- shiny::NS(id)
  shiny::moduleServer(
    id,
    function(input, output, session) {
      initial_values <- w$.get_values()
      rv <- do.call(shiny::reactiveValues, initial_values)

      shiny::observeEvent(input$anyhtmlwidget_on_save_changes, {
        # update values on w here
        for(key in names(input$anyhtmlwidget_on_save_changes)) {
          rv[[key]] <- input$anyhtmlwidget_on_save_changes[[key]]
          w$.set_value(key, input$anyhtmlwidget_on_save_changes[[key]], emit_change = FALSE)
        }
      })

      for(key in names(initial_values)) {
        shiny::observeEvent(rv[[key]], {
          session$sendCustomMessage(ns("anyhtmlwidget_on_change"), list(key = key, value = rv[[key]]))
        })
      }

      output$widget <- render_anyhtmlwidget(expr = {
        the_anyhtmlwidget(esm = w$.get_esm(), values = initial_values, width = w$.get_width(), height = w$.get_height(), ns_id = id)
      })

      return(rv)
    }
  )
}
