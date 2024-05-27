start_server <- function(w, host = "0.0.0.0", port = 8080) {
  app <- list(
    onWSOpen = function(ws) {
      # The ws object is a WebSocket object
      cat("Server connection opened.\n")

      w$on_change(function(key, new_val) {
        msg_list <- list(
          type = jsonlite::unbox("on_change"),
          payload = list(
            key = jsonlite::unbox(key),
            value = jsonlite::unbox(new_val)
          )
        )
        ws$send(jsonlite::toJSON(msg_list, auto_unbox = FALSE))
      })

      ws$onMessage(function(binary, message) {
        msg_list <- jsonlite::fromJSON(message, simplifyVector = FALSE, auto_unbox = FALSE)
        msg_type <- msg_list$type
        msg_val <- msg_list$payload

        if(msg_type == "on_save_changes") {
          for(key in names(msg_val)) {
            w$set_value(key, msg_val[[key]], emit_change = FALSE)
          }
        }

        ret_val <- list(success = TRUE)
        ws$send(jsonlite::toJSON(ret_val, auto_unbox = FALSE))
      })
      ws$onClose(function() {
        cat("Server connection closed.\n")
      })
    }
  )

  s <- httpuv::startServer(host = host, port = port, app = app)
  return(s)
}

