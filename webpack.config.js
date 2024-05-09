const path = require("path");

module.exports = {
  mode: "development",
  optimization: {
    usedExports: true,
  },
  entry: {
    main: "./static/main.js",
    nglComponent: "./static/ngl-web-component",
  },
  output: {
    filename: "[name].bundle.js",
    path: path.resolve(__dirname, "dist"),
  },
};