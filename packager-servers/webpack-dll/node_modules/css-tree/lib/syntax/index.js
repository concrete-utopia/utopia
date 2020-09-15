var hasOwnProperty = Object.prototype.hasOwnProperty;
var List = require('../utils/list');
var createParser = require('../parser');
var createGenerator = require('../generator').createGenerator;
var createMarkupGenerator = require('../generator').createMarkupGenerator;
var sourceMapGenerator = require('../generator').sourceMap;
var createConvertors = require('../utils/convert');
var createWalker = require('../walker');
var names = require('../utils/names');
var mix = require('./mix');

function assign(dest, src) {
    for (var key in src) {
        dest[key] = src[key];
    }

    return dest;
}

function createParseContext(name) {
    return function() {
        return this[name]();
    };
}

function isValidNumber(value) {
    // Number.isInteger(value) && value >= 0
    return (
        typeof value === 'number' &&
        isFinite(value) &&
        Math.floor(value) === value &&
        value >= 0
    );
}

function isValidLocation(loc) {
    return (
        Boolean(loc) &&
        isValidNumber(loc.offset) &&
        isValidNumber(loc.line) &&
        isValidNumber(loc.column)
    );
}

function createNodeStructureChecker(type, fields) {
    return function checkNode(node, warn) {
        if (!node || node.constructor !== Object) {
            return warn('Type of node should be an object');
        }

        for (var key in node) {
            if (key === 'type') {
                if (node.type !== type) {
                    warn('Wrong node type `' + node.type + '` but expected `' + type + '`');
                }
            } else if (key === 'loc') {
                if (node.loc === null) {
                    continue;
                } else if (node.loc && node.loc.constructor === Object) {
                    if (typeof node.loc.source === 'string' &&
                        isValidLocation(node.loc.start) &&
                        isValidLocation(node.loc.end)) {
                        continue;
                    }
                }
                warn('Wrong value for `' + type + '.' + key + '` field');
            } else if (fields.hasOwnProperty(key)) {
                for (var i = 0, valid = false; !valid && i < fields[key].length; i++) {
                    var fieldType = fields[key][i];

                    switch (fieldType) {
                        case String:
                            valid = typeof node[key] === 'string';
                            break;

                        case Boolean:
                            valid = typeof node[key] === 'boolean';
                            break;

                        case null:
                            valid = node[key] === null;
                            break;

                        default:
                            if (typeof fieldType === 'string') {
                                valid = node[key] && node[key].type === fieldType;
                            } else if (Array.isArray(fieldType)) {
                                valid = node[key] instanceof List;
                            }
                    }
                }
                if (!valid) {
                    warn('Wrong value for `' + type + '.' + key + '` field');
                }
            } else {
                warn('Unknown field `' + key + '` for ' + type);
            }
        }

        for (var key in fields) {
            if (hasOwnProperty.call(node, key) === false) {
                warn('Field `' + type + '.' + key + '` is missed');
            }
        }
    };
}

function processStructure(name, nodeType) {
    var structure = nodeType.structure;
    var fields = {
        type: String,
        loc: true
    };
    var walkers = [];
    var docs = {
        type: '"' + name + '"'
    };

    for (var key in structure) {
        var walker = {
            name: key,
            type: false,
            nullable: false
        };
        var docsTypes = [];
        var fieldTypes = fields[key] = Array.isArray(structure[key])
            ? structure[key].slice()
            : [structure[key]];

        for (var i = 0; i < fieldTypes.length; i++) {
            var fieldType = fieldTypes[i];
            if (fieldType === String || fieldType === Boolean) {
                docsTypes.push(fieldType.name);
            } else if (fieldType === null) {
                walker.nullable = true;
                docsTypes.push('null');
            } else if (typeof fieldType === 'string') {
                walker.type = 'node';
                docsTypes.push('<' + fieldType + '>');
            } else if (Array.isArray(fieldType)) {
                walker.type = 'list';
                docsTypes.push('List'); // TODO: use type enum
            } else {
                throw new Error('Wrong value in `' + name + '` structure definition');
            }
        }

        docs[key] = docsTypes.join(' | ');

        if (walker.type) {
            walkers.push(walker);
        }
    }

    return {
        docs: docs,
        check: createNodeStructureChecker(name, fields),
        walk: walkers.length ? {
            context: nodeType.walkContext,
            fields: walkers
        } : null
    };
}

function createSyntax(config) {
    var parser = { context: {}, scope: {}, atrule: {}, pseudo: {} };
    var walker = { type: {} };
    var generator = {};
    var lexer = { structure: {} };

    if (config.parseContext) {
        for (var name in config.parseContext) {
            switch (typeof config.parseContext[name]) {
                case 'function':
                    parser.context[name] = config.parseContext[name];
                    break;

                case 'string':
                    parser.context[name] = createParseContext(config.parseContext[name]);
                    break;
            }
        }
    }

    if (config.scope) {
        for (var name in config.scope) {
            parser.scope[name] = config.scope[name];
        }
    }

    if (config.atrule) {
        for (var name in config.atrule) {
            var atrule = config.atrule[name];

            if (atrule.parse) {
                parser.atrule[name] = atrule.parse;
            }
        }
    }

    if (config.pseudo) {
        for (var name in config.pseudo) {
            var pseudo = config.pseudo[name];

            if (pseudo.parse) {
                parser.pseudo[name] = pseudo.parse;
            }
        }
    }

    if (config.node) {
        for (var name in config.node) {
            var nodeType = config.node[name];

            parser[name] = nodeType.parse;
            generator[name] = nodeType.generate;

            if (nodeType.structure) {
                var structure = processStructure(name, nodeType);
                lexer.structure[name] = {
                    docs: structure.docs,
                    check: structure.check
                };
                if (structure.walk) {
                    walker.type[name] = structure.walk;
                }
            } else {
                throw new Error('Missed `structure` field in `' + name + '` node type definition');
            }
        }
    }

    var parse = createParser(parser);
    var Lexer = require('../lexer/Lexer');
    var walker = createWalker(walker.type);
    var convertors = createConvertors(walker);
    var markupGenerator = createMarkupGenerator(generator);

    var syntax = {
        List: require('../utils/list'),
        Tokenizer: require('../tokenizer'),
        Lexer: require('../lexer/Lexer'),

        property: names.property,
        keyword: names.keyword,

        lexer: null,
        syntax: require('../lexer'),
        createLexer: function(config) {
            return new Lexer(config, syntax, lexer.structure);
        },

        parse: parse,

        walk: walker.all,
        walkUp: walker.allUp,
        walkRules: walker.rules,
        walkRulesRight: walker.rulesRight,
        walkDeclarations: walker.declarations,

        translate: createGenerator(generator),
        translateWithSourceMap: function(node) {
            return sourceMapGenerator(markupGenerator, node);
        },
        translateMarkup: markupGenerator,

        clone: require('../utils/clone'),
        fromPlainObject: convertors.fromPlainObject,
        toPlainObject: convertors.toPlainObject,

        createSyntax: function(config) {
            return createSyntax(mix({}, config));
        },
        fork: function(extension) {
            var base = mix({}, config); // copy of config
            return createSyntax(
                typeof extension === 'function'
                    ? extension(base, assign)
                    : mix(base, extension)
            );
        }
    };

    syntax.lexer = new Lexer({
        generic: true,
        types: config.types,
        properties: config.properties
    }, syntax, lexer.structure);

    return syntax;
};

exports.create = function(config) {
    return createSyntax(mix({}, config));
};
