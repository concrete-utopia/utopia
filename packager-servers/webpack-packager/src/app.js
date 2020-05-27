var express = require('express');
var compression = require('compression');
var app = express();
var verifyAvailability = require('./middleware/verifyAvailability');
var extractPackages = require('./middleware/extractPackages');
var extractAndBundle = require('./middleware/extractAndBundle');

app.use(compression());
app.get('/*', verifyAvailability, extractPackages, extractAndBundle);

module.exports = app;
