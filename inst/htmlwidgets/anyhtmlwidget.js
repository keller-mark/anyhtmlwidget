function valuesToModel(values) {
  return {
    get: (name) => {
      return values[name];
    },
    // TODO: more anywidget model methods.
  };
}

HTMLWidgets.widget({
  name: 'anyhtmlwidget',
  type: 'output',
  factory: function(el, width, height) {
    // TODO: define shared variables for this instance

    let widget;
    let model;

    return {
      renderValue: async function(x) {
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

      	model = valuesToModel(x.values);

      	try {
      	  // TODO: pass width/height?
      	  // TODO: pass anywidget/ipywidgets model-like object?
      	  const cleanup = await widget.render({ model, el, width, height });

      	} catch(e) {
      	  // TODO: re-throw error
      	}

      	// TODO: register cleanup function somehow, then call later
      },
      resize: async function(width, height) {
        if(widget?.resize) {
          await widget.resize({ model, el, width, height });
        }
      }
    };
  }
});
