"use strict";
exports.__esModule = true;
var path = require("path");
var extract_require_1 = require("./extract-require");
describe('resolveRequirePath', function () {
    it('resolves a module in a sibling folder', function () {
        var absoluteUrl = path.resolve('./test-folder/test-a');
        var result = extract_require_1.resolveRequirePath(absoluteUrl, 'test-a');
        var expectedResult = [
            path.resolve('./test-folder/test-a/index.js'),
            path.resolve('./test-folder/test-a/node_modules/test-package-b/index.js'),
            path.resolve('./test-folder/test-a/node_modules/test-package-a/index.js'),
        ];
        expect(result).toEqual(expectedResult);
    });
});
