'use strict';

var SourceMapGenerator = require('source-map').SourceMapGenerator;
var trackNodes = {
    Atrule: true,
    Selector: true,
    Declaration: true
};

module.exports = function generateSourceMap(generator, ast) {
    function updateGeneratedPos(buffer) {
        for (; bufferPos < buffer.length; bufferPos++) {
            if (buffer.charCodeAt(bufferPos) === 10) { // \n
                generated.line++;
                generated.column = 0;
            } else {
                generated.column++;
            }
        }
    }

    var map = new SourceMapGenerator();
    var bufferPos = 0;
    var generated = {
        line: 1,
        column: 0
    };
    var original = {
        line: 0,
        column: 0
    };
    var sourceMappingActive = false;
    var activatedGenerated = {
        line: 1,
        column: 0
    };
    var activatedMapping = {
        generated: activatedGenerated
    };

    var css = generator(ast, function(node, buffer) {
        if (!node.loc ||
            !node.loc.start ||
            !trackNodes.hasOwnProperty(node.type)) {
            return;
        }

        var line = node.loc.start.line;
        var column = node.loc.start.column - 1;

        if (original.line !== line ||
            original.column !== column) {
            original.line = line;
            original.column = column;

            updateGeneratedPos(buffer);

            if (sourceMappingActive) {
                sourceMappingActive = false;
                if (generated.line !== activatedGenerated.line ||
                    generated.column !== activatedGenerated.column) {
                    map.addMapping(activatedMapping);
                }
            }

            sourceMappingActive = true;
            map.addMapping({
                source: node.loc.source,
                original: original,
                generated: generated
            });
        }

    }, function(node, buffer) {
        if (sourceMappingActive && trackNodes.hasOwnProperty(node.type)) {
            updateGeneratedPos(buffer);
            activatedGenerated.line = generated.line;
            activatedGenerated.column = generated.column;
        }
    });

    if (sourceMappingActive) {
        map.addMapping(activatedMapping);
    }

    return {
        css: css,
        map: map
    };
};
