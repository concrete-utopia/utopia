module.exports = {
    parse: {
        expression: function() {
            return this.MediaQueryList();
        },
        block: function() {
            return this.Block(this.Rule);
        }
    }
};
