module.exports = {
    entry: [
        ${1:'./app/main.js'}
    ],
    output: {
        path: __dirname + '/dist',
        filename: "app.bundle.js"
    },
    module: {
        rules: [
            {
              test: /\.js$/,
              exclude: /node_modules/,
              use: {
                  loader: "babel-loader"
              }
            }
        ]
  },
    // plugins: []
};
