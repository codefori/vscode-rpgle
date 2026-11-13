// @ts-check
import { defineConfig, globalIgnores } from "eslint/config";
import globals from "globals";
import tsParser from "@typescript-eslint/parser";
import tsPlugin from "@typescript-eslint/eslint-plugin";

export default defineConfig([globalIgnores(["**/dist"]), {
  files: ["**/*.ts", "**/*.tsx"],
  languageOptions: {
    parser: tsParser,
    globals: {
      ...Object.fromEntries(Object.entries(globals.browser).map(([key]) => [key, "off"])),
      ...globals.commonjs,
      ...globals.node,
      ...globals.mocha,
      Thenable: "readonly",
      NodeJS: "readonly",
    },

    ecmaVersion: 2018,
    sourceType: "module",

    parserOptions: {
      ecmaFeatures: {
        jsx: true,
      },
    },
  },

  plugins: {
    // @ts-ignore
    "@typescript-eslint": tsPlugin,
  },

  rules: {
    "no-const-assign": "warn",
    "no-this-before-super": "warn",
    "no-undef": "warn",
    "no-unreachable": "warn",
    "no-var": "error",
    "constructor-super": "warn",
    "valid-typeof": "warn",
    indent: ["error", "tab", { SwitchCase: 1 }],
  },
}]);
