/*
Copyright (c) 2015, Yahoo! Inc. All rights reserved.
Copyrights licensed under the New BSD License.
See the accompanying LICENSE file for terms.

Authors: Nera Liu <neraliu@yahoo-inc.com>
         Adonis Fung <adon@yahoo-inc.com>
         Albert Yu <albertyu@yahoo-inc.com>
*/


require("mocha");

expect = require('expect.js');
xssFilters = require('../src/xss-filters');
testutils = require('./utils.js');

require('./unit/private-xss-filters.js');
require('./unit/xss-filters.js');