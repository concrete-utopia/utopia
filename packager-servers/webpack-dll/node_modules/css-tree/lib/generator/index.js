'use strict';

var hasOwnProperty = Object.prototype.hasOwnProperty;

function each(list) {
    var cursor = list.head;
    var result = [];

    while (cursor !== null) {
        result.push(this.generate(cursor.data, cursor, list));
        cursor = cursor.next;
    }

    return result;
}

function eachComma(list) {
    var cursor = list.head;
    var result = [];

    while (cursor !== null) {
        if (cursor.prev) {
            result.push(',', this.generate(cursor.data));
        } else {
            result.push(this.generate(cursor.data));
        }

        cursor = cursor.next;
    }

    return result;
}

function createGenerator(types) {
    var context = {
        generate: function(node, item, list) {
            if (hasOwnProperty.call(types, node.type)) {
                var ret = types[node.type].call(this, node, item, list);
                return typeof ret === 'string' ? ret : ret.join('');
            } else {
                throw new Error('Unknown node type: ' + node.type);
            }
        },
        each: each,
        eachComma: eachComma
    };

    return function(node) {
        return context.generate(node);
    };
}

function createMarkupGenerator(types) {
    var context = {
        generate: function(node, item, list) {
            if (hasOwnProperty.call(types, node.type)) {
                return {
                    node: node,
                    value: types[node.type].call(this, node, item, list)
                };
            } else {
                throw new Error('Unknown node type: ' + node.type);
            }
        },
        each: each,
        eachComma: eachComma
    };

    return function(node, before, after) {
        function walk(node, buffer) {
            var value = node.value;

            before(node.node, buffer, value);

            if (typeof value === 'string') {
                buffer += value;
            } else {
                for (var i = 0; i < value.length; i++) {
                    if (typeof value[i] === 'string') {
                        buffer += value[i];
                    } else {
                        buffer = walk(value[i], buffer);
                    }
                }
            }

            after(node.node, buffer, value);

            return buffer;
        }

        if (typeof before !== 'function') {
            before = function() {};
        }
        if (typeof after !== 'function') {
            after = function() {};
        }

        return walk(context.generate(node), '');
    };
}

module.exports = {
    createGenerator: createGenerator,
    createMarkupGenerator: createMarkupGenerator,
    sourceMap: require('./sourceMap')
};
