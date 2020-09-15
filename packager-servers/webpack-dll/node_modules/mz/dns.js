
require('thenify-all').withCallback(
  require('dns'),
  exports, [
    'lookup',
    'resolve',
    'resolve4',
    'resolve6',
    'resolveMx',
    'resolveTxt',
    'resolveSrv',
    'resolveNs',
    'resolveCname',
    'reverse',
  ]
)
