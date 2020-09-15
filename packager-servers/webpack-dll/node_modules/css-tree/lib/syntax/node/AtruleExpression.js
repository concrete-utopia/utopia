var List = require('../../utils/list');

module.exports = {
    name: 'AtruleExpression',
    structure: {
        children: [[]]
    },
    parse: function(name) {
        var children = null;

        if (name !== null) {
            name = name.toLowerCase();
        }

        // custom consumer
        if (this.atrule.hasOwnProperty(name)) {
            if (typeof this.atrule[name].expression === 'function') {
                children = this.atrule[name].expression.call(this);

                if (children instanceof List === false) {
                    return children;
                }
            }
        } else {
            // default consumer
            this.scanner.skipSC();
            children = this.readSequence(this.scope.AtruleExpression);
        }

        if (children === null || children.isEmpty()) {
            return null;
        }

        return {
            type: 'AtruleExpression',
            loc: this.getLocationFromList(children),
            children: children
        };
    },
    generate: function(node) {
        return this.each(node.children);
    },
    walkContext: 'atruleExpression'
};
