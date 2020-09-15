"use strict";
exports.__esModule = true;
var extract_require_1 = require("./extract-require");
var path = require("path");
var fs = require("fs");
// usage: `npm start "input-file-path.js" "output-file-path.json"`
if (process.argv.length !== 4) {
    throw new Error('invalid number of parameters');
}
var targetPackage = process.argv[2];
var outputParam = process.argv[3];
var packagePath = path.resolve(targetPackage);
var packageName = path.parse(packagePath).name;
var result = extract_require_1.resolveRequirePath(packagePath, packageName);
var outputPath = path.resolve(outputParam);
fs.writeFileSync(outputPath, JSON.stringify(result, null, 2));
