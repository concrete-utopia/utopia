'use strict';

function walkRules(node, item, list) {
    switch (node.type) {
        case 'StyleSheet':
            var oldStylesheet = this.stylesheet;
            this.stylesheet = node;

            node.children.each(walkRules, this);

            this.stylesheet = oldStylesheet;
            break;

        case 'Atrule':
            if (node.block !== null) {
                walkRules.call(this, node.block);
            }

            this.fn(node, item, list);
            break;

        case 'Rule':
            this.fn(node, item, list);

            var oldRule = this.rule;
            this.rule = node;

            walkRules.call(this, node.block);

            this.rule = oldRule;
            break;

        case 'Block':
            var oldBlock = this.block;
            this.block = node;

            node.children.each(walkRules, this);

            this.block = oldBlock;
            break;
    }
}

function walkRulesRight(node, item, list) {
    switch (node.type) {
        case 'StyleSheet':
            var oldStylesheet = this.stylesheet;
            this.stylesheet = node;

            node.children.eachRight(walkRulesRight, this);

            this.stylesheet = oldStylesheet;
            break;

        case 'Atrule':
            if (node.block !== null) {
                walkRulesRight.call(this, node.block);
            }

            this.fn(node, item, list);
            break;

        case 'Rule':
            var oldRule = this.rule;
            this.rule = node;

            walkRulesRight.call(this, node.block);

            this.rule = oldRule;

            this.fn(node, item, list);
            break;

        case 'Block':
            var oldBlock = this.block;
            this.block = node;

            node.children.eachRight(walkRulesRight, this);

            this.block = oldBlock;
            break;
    }
}

function walkDeclarations(node) {
    switch (node.type) {
        case 'StyleSheet':
            var oldStylesheet = this.stylesheet;
            this.stylesheet = node;

            node.children.each(walkDeclarations, this);

            this.stylesheet = oldStylesheet;
            break;

        case 'Atrule':
            if (node.block !== null) {
                walkDeclarations.call(this, node.block);
            }
            break;

        case 'Rule':
            var oldRule = this.rule;
            this.rule = node;

            if (node.block !== null) {
                walkDeclarations.call(this, node.block);
            }

            this.rule = oldRule;
            break;

        case 'Block':
            node.children.each(function(node, item, list) {
                if (node.type === 'Declaration') {
                    this.fn(node, item, list);
                } else {
                    walkDeclarations.call(this, node);
                }
            }, this);
            break;
    }
}

function createContext(root, fn) {
    var context = {
        fn: fn,
        root: root,
        stylesheet: null,
        atruleExpression: null,
        rule: null,
        selector: null,
        block: null,
        declaration: null,
        function: null
    };

    return context;
}

module.exports = function createWalker(types) {
    var walkers = {};

    for (var name in types) {
        var config = types[name];
        walkers[name] = Function('node', 'context', 'walk',
            (config.context ? 'var old = context.' + config.context + ';\ncontext.' + config.context + ' = node;\n' : '') +
            config.fields.map(function(field) {
                var line = field.type === 'list'
                    ? 'node.' + field.name + '.each(walk);'
                    : 'walk(node.' + field.name + ');';

                if (field.nullable) {
                    line = 'if (node.' + field.name + ') {\n    ' + line + '}';
                }

                return line;
            }).join('\n') +
            (config.context ? '\ncontext.' + config.context + ' = old;' : '')
        );
    }

    return {
        all: function(root, fn) {
            function walk(node, item, list) {
                fn.call(context, node, item, list);
                if (walkers.hasOwnProperty(node.type)) {
                    walkers[node.type](node, context, walk);
                }
            }

            var context = createContext(root, fn);

            walk(root);
        },
        allUp: function(root, fn) {
            function walk(node, item, list) {
                if (walkers.hasOwnProperty(node.type)) {
                    walkers[node.type](node, context, walk);
                }
                fn.call(context, node, item, list);
            }

            var context = createContext(root, fn);

            walk(root);
        },
        rules: function(root, fn) {
            walkRules.call(createContext(root, fn), root);
        },
        rulesRight: function(root, fn) {
            walkRulesRight.call(createContext(root, fn), root);
        },
        declarations: function(root, fn) {
            walkDeclarations.call(createContext(root, fn), root);
        }
    };
};
