// The flat configuration ESLint 9 and newer look for.  The .eslintrc.json
// beside it says the same thing for ESLint 8, which reads that one instead,
// so the resource lints the same however old the installed ESLint is.
//
// `strict' only has anything to say about a script; under the module goal
// the source is strict already and the rule stays quiet.

module.exports = [
  {
    languageOptions: {
      sourceType: "script"
    },
    rules: {
      strict: "warn",
      "no-unused-vars": "warn"
    }
  }
];
