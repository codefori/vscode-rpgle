/*---------------------------------------------------------------------------------------------
 *  Copyright (c) Microsoft Corporation. All rights reserved.
 *  Licensed under the MIT License. See License.txt in the project root for license information.
 *--------------------------------------------------------------------------------------------*/

// @ts-nocheck

'use strict';

const withDefaults = require(`../shared.webpack.config`);
const path = require(`path`);
const webpack = require(`webpack`);

module.exports = withDefaults({
  context: path.join(__dirname),
  entry: {
    extension: `./src/server.ts`,
  },
  output: {
    filename: `server.js`,
    path: path.join(__dirname, `..`, `out`)
  },
  plugins: [
    new webpack.DefinePlugin({
      'process.env.LANGUAGE_TOOLS_ENABLED': process.env.LANGUAGE_TOOLS_ENABLED || `true`,
      'process.env.LINTER_ENABLED': process.env.LINTER_ENABLED || `true`,
      'process.env.FORMATTER_ENABLED': process.env.FORMATTER_ENABLED || `true`,
    }),
  ],
});