var utils = require('../utils');

function extractPackages (req, res, next) {
  req.params.packages = req.params['0'];
  if (!utils.isValidPackages(req.params.packages)) {
    res.sendStatus(404);
  } else {
    next();
  }
}

module.exports = extractPackages;
