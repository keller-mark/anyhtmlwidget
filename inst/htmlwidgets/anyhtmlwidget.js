class AnyModel {
  constructor(state, ns_id, ws) {
    this.ns_id = ns_id;
    this.state = state;
    this.target = new EventTarget();
    this.ws = ws;
    this.unsavedKeys = new Set();
  }
  get(name) {
    return this.state[name];
  }
  set(key, value) {
    this.state[key] = value;
    this.unsavedKeys.add(key);
    this.target.dispatchEvent(
			new CustomEvent(`change:${key}`, { detail: value }),
		);
  }
  on(name, callback) {
    this.target.addEventListener(name, callback);
  }
  off(name) {
    // Not yet implemented
  }
  save_changes() {
    const unsavedState = Object.fromEntries(
      Array.from(this.unsavedKeys.values())
        .map(key => ([key, this.state[key]]))
    );
    this.unsavedKeys = new Set();
    if(window && window.Shiny && window.Shiny.setInputValue) {
      const eventPrefix = this.ns_id ? `${this.ns_id}-` : '';
      Shiny.setInputValue(`${eventPrefix}anyhtmlwidget_on_save_changes`, unsavedState);
    } else if(this.ws) {
      this.ws.send(JSON.stringify({
        type: "on_save_changes",
        payload: unsavedState,
      }));
    }
  }
}

function emptyElement(el) {
	while (el.firstChild) {
		el.removeChild(el.firstChild);
	}
}

HTMLWidgets.widget({
  name: 'anyhtmlwidget',
  type: 'output',
  factory: function(el, width, height) {

    let widget;
    let model;
    let cleanup;
    let ws;

    return {
      renderValue: async function(x) {
        if(cleanup && typeof cleanup === "function") {
          cleanup();
          cleanup = undefined;
          if(ws) {
            ws.close();
            ws = undefined;
          }
        }
      	// The default can either be an object like { render, initialize }
      	// or a function that returns this object.
      	if(!widget) {
      	  const esm = x.esm;
          const url = URL.createObjectURL(new Blob([esm], { type: "text/javascript" }));
      	  const mod = await import(/* webpackIgnore: true */ url);
      	  URL.revokeObjectURL(url);

      	  widget = typeof mod.default === "function"
        		? await mod.default()
        		: mod.default;

        	// TODO: initialize here
      	}

      	if(x.port && x.host && !window.Shiny) {
      	  ws = new WebSocket(`ws://${x.host}:${x.port}`);
      	}

      	model = new AnyModel(x.values, x.ns_id, ws);

      	if(window && window.Shiny && window.Shiny.addCustomMessageHandler) {
      	  const eventPrefix = x.ns_id ? `${x.ns_id}-` : '';
      	  Shiny.addCustomMessageHandler(`${eventPrefix}anyhtmlwidget_on_change`, ({ key, value}) => {
      	    model.set(key, value);
      	  });
      	} else if(x.port && x.host) {
      	  ws.onmessage = (event) => {
      	    const { type, payload } = JSON.parse(event.data);
      	    if(type === "on_change") {
      	      const { key, value } = payload;
      	      model.set(key, value);
      	    }
      	  };
      	}

      	try {
      	  emptyElement(el);
          // Register cleanup function.
      	  cleanup = await widget.render({ model, el, width, height });

      	} catch(e) {
      	  // TODO: re-throw error
      	}
      },
      resize: async function(width, height) {
        // TODO: emit resize event on window (and let user handle)?
        if(widget?.resize) {
          await widget.resize({ model, el, width, height });
        }
      }
    };
  }
});
