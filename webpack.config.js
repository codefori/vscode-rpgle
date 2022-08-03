//@ts-check

'use strict';

const path = require(`path`);
const webpack = require(`webpack`);

/**@type {webpack.Configuration}*/
module.exports = (env) => { 
  const target = env.target || `desktop`;

  const options = {
    outname: `extension.js`,
    target: `node`,
    mainFields: [],
    fallback: {}
  }
  
  switch (target) {
  case `web`:
    options.target = `webworker`;
    options.outname = `web.js`;
    options.mainFields = [`browser`, `module`, `main`];
    options.fallback[`path`] = require.resolve(`path-browserify`);
    break;
  
  case `desktop`:
  default:
    options.target = `node`;
    options.outname = `extension.js`;
    break;
  }
  
  console.log(`Target: ${target}`);
  
  return {
    target: options.target, // vscode extensions run in a Node.js-context 📖 -> https://webpack.js.org/configuration/node/

    entry: {
      extension: `./src/extension.js`,
    }, // the entry point of this extension, 📖 -> https://webpack.js.org/configuration/entry-context/
    output: {
    // the bundle is stored in the 'dist' folder (check package.json), 📖 -> https://webpack.js.org/configuration/output/
      path: path.resolve(__dirname, `dist`),
      filename: options.outname,
      libraryTarget: `commonjs`,
      devtoolModuleFilenameTemplate: `../[resource-path]`,
    },
    devtool: `nosources-source-map`,
    externals: {
      vscode: `commonjs vscode` // the vscode-module is created on-the-fly and must be excluded. Add other modules that cannot be webpack'ed, 📖 -> https://webpack.js.org/configuration/externals/
    },
    resolve: {
      fallback: options.fallback,
      mainFields: options.mainFields,
      // support reading TypeScript and JavaScript files, 📖 -> https://github.com/TypeStrong/ts-loader
      extensions: [`.ts`, `.js`, `.svg`],
    },
    module: {
      rules: [
        {
          test: /\.js$/,
          include: path.resolve(__dirname, `node_modules/@bendera/vscode-webview-elements/dist`),
          type: `asset/source`
        }
      ]
    },
    plugins: [
      new webpack.DefinePlugin({
        'process.env.TARGET': JSON.stringify(target),
      })
    ],
  }
};