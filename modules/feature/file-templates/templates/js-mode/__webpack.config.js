module.exports = {
    entry: [
        ${1:'./app/main.js'}
    ],
    output: {
        path: __dirname + '/dist',
        filename: "app.bundle.js"
    },
    module: {
        loaders: [
            { test: /\.js$/, include: __dirname + '/app', loader: 'babel-loader' }$0
        ]
    }
    // plugins: []
};