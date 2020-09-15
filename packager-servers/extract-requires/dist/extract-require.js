"use strict";
exports.__esModule = true;
exports.resolveRequirePath = void 0;
var R = require("ramda");
var module_1 = require("module");
function monkeyPatchRequire(extensionsToMutate, fileExtension) {
    console.log('mutating extensions', fileExtension);
    var realRequire = extensionsToMutate[fileExtension];
    extensionsToMutate[fileExtension] = function () {
        var args = [];
        for (var _i = 0; _i < arguments.length; _i++) {
            args[_i] = arguments[_i];
        }
        try {
            var realResult = realRequire.apply(void 0, args);
            return realResult;
        }
        catch (e) {
            return {};
        }
    };
}
function monkeyPatchExtensions(extensionsToMutate) {
    Object.keys(extensionsToMutate).forEach(function (fileExtension) {
        return monkeyPatchRequire(extensionsToMutate, fileExtension);
    });
}
function resolveRequirePath(packageEntryPath, toRequire) {
    try {
        var localRequire = module_1.createRequire(packageEntryPath);
        monkeyPatchExtensions(localRequire.extensions);
        var before = Object.keys(localRequire.cache);
        localRequire(packageEntryPath);
        var after = Object.keys(localRequire.cache);
        var newImports = R.difference(after, before);
        return newImports;
    }
    catch (e) {
        return [];
    }
}
exports.resolveRequirePath = resolveRequirePath;
