/*---------------------------------------------------------------------------------------------
 *  Copyright (c) Microsoft Corporation. All rights reserved.
 *  Licensed under the MIT License. See License.txt in the project root for license information.
 *--------------------------------------------------------------------------------------------*/

// @ts-nocheck

'use strict';

const withDefaults = require(`../../shared.webpack.config`);
const path = require(`path`);
const webpack = require(`webpack`);

module.exports = withDefaults({
  context: path.join(__dirname),
  entry: {
    extension: `./index.ts`,
  },
  output: {
    filename: `index.js`,
    path: path.join(__dirname, `dist`)
  },
  // Other stuff
  plugins: [
    new webpack.BannerPlugin({banner: `#! /usr/bin/env node`, raw: true})
  ]
});