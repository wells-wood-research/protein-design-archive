// This returns the flags passed into your Elm application
export const flags = async ({ env }) => {
  return {}
}

// This function is called once your Elm app is running
export const onReady = ({ app, env }) => {
  if (app.ports) {
    // This port requests plotting from Vega
    if (app.ports.vegaPlotCmd) {
      app.ports.vegaPlotCmd.subscribe(({ plotId, spec }) => {
        // using request animation frame to make sure that elm has updated the
        // page before trying to embed the plots
        window.requestAnimationFrame(() => {
          vegaEmbed("#" + plotId, spec, {
            actions: true,
          }).catch(console.warn);
        });
      }
      );
    } else {
      console.log("No port called `vegaPlotCmd` is exposed by the Elm app.")
    }

  } else {
    console.log("No Elm ports found.")
  }
}
