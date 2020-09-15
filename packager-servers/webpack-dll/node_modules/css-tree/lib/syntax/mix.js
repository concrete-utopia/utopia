var shape = {
    generic: true,
    types: {},
    properties: {},
    parseContext: {},
    scope: {},
    atrule: ['parse'],
    pseudo: ['parse'],
    node: ['name', 'structure', 'parse', 'generate', 'walkContext']
};

function mix(dest, src, shape) {
    for (var key in shape) {
        if (shape[key] === true) {
            if (key in src) {
                dest[key] = src[key];
            }
        } else if (shape[key]) {
            if (shape[key].constructor === Object) {
                var res = {};
                for (var name in dest[key]) {
                    res[name] = dest[key][name];
                }
                for (var name in src[key]) {
                    res[name] = src[key][name];
                }
                dest[key] = res;
            } else if (Array.isArray(shape[key])) {
                var res = {};
                var innerShape = shape[key].reduce(function(s, k) {
                    s[k] = true;
                    return s;
                }, {});
                for (var name in dest[key]) {
                    res[name] = {};
                    if (dest[key] && dest[key][name]) {
                        mix(res[name], dest[key][name], innerShape);
                    }
                }
                for (var name in src[key]) {
                    if (!res[name]) {
                        res[name] = {};
                    }
                    if (src[key] && src[key][name]) {
                        mix(res[name], src[key][name], innerShape);
                    }
                }
                dest[key] = res;
            }
        }
    }
    return dest;
}

module.exports = function(dest, src) {
    return mix(dest, src, shape);
};
