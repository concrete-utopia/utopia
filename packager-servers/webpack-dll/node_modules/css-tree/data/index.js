var mdnProperties = require('./mdn-data-properties.json');
var mdnSyntaxes = require('./mdn-data-syntaxes.json');
var patch = require('./patch.json');
var data = {
    properties: {},
    types: {}
};

function normalizeSyntax(syntax) {
    return syntax
        .replace(/&lt;/g, '<')
        .replace(/&gt;/g, '>')
        .replace(/&nbsp;/g, ' ')
        .replace(/&amp;/g, '&');
}

// apply patch
for (var key in patch.properties) {
    if (key in mdnProperties) {
        if (patch.properties[key]) {
            mdnProperties[key].syntax = patch.properties[key].syntax;
        } else {
            delete mdnProperties[key];
        }
    } else {
        mdnProperties[key] = patch.properties[key];
    }
}

for (var key in patch.syntaxes) {
    if (patch.syntaxes[key].syntax) {
        mdnSyntaxes[key] = patch.syntaxes[key].syntax;
    } else {
        delete mdnSyntaxes[key];
    }
}

// normalize source mdnProperties syntaxes, since it uses html token
for (var key in mdnProperties) {
    data.properties[key] = normalizeSyntax(mdnProperties[key].syntax);
}

for (var key in mdnSyntaxes) {
    data.types[key] = normalizeSyntax(mdnSyntaxes[key]);
}

module.exports = data;
