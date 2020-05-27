function verifyAvailability(req, res, next) {
  if (verifyAvailability.isAvailable) {
    next();
  } else {
    res.sendStatus(503);
  }
}

verifyAvailability.isAvailable = true;

module.exports = verifyAvailability;
