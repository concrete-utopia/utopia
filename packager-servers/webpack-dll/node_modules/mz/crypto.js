
require('thenify-all').withCallback(
  require('crypto'),
  exports, [
    'pbkdf2',
    'randomBytes',
    'pseudoRandomBytes',
  ]
)
