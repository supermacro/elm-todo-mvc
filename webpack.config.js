const path = require("path");
const HtmlWebpackPlugin = require('html-webpack-plugin');

const SOURCE_DIR = path.join(__dirname, 'src')

module.exports = {
  entry: {
    app: [
      './src/index.js'
    ]
  },

  output: {
    path: path.resolve(__dirname + '/dist'),
    filename: '[name].js',
  },

  module: {
    rules: [
      {
        test: /\.(css|scss)$/,
        use: [
          'style-loader',
          'css-loader',
        ]
      },
      {
        test:    /\.elm$/,
        exclude: [/elm-stuff/, /node_modules/],
        loader:  'elm-webpack-loader?verbose=true&warn=true',
      },
    ],

    noParse: /\.elm$/,
  },

  plugins: [
    new HtmlWebpackPlugin({
      template: path.join(SOURCE_DIR, 'index.html'),
      minify: {
        collapseWhitespace: true,
        removeComments: true
      }
      // favicon: path.resolve('./static/favicon.png')
    })
  ],

  devServer: {
    inline: true,
    stats: { colors: true },
  },
};
