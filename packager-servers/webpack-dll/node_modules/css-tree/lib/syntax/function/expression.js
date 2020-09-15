var List = require('../../utils/list');
var BALANCED = true;

// legacy IE function
// expression '(' raw ')'
module.exports = function() {
    return new List().appendData(
        this.Raw(BALANCED, 0, 0)
    );
};
