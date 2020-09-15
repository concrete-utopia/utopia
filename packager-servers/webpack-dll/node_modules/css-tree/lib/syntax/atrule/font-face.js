module.exports = {
    parse: {
        expression: null,
        block: function() {
            return this.Block(this.Declaration);
        }
    }
};
