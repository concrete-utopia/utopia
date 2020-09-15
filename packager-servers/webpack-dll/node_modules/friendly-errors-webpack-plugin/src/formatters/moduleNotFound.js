'use strict';
const concat = require('../utils').concat;

function moduleNotFound (count) {
  if (count === 1) {
    return 'This module was not found:';
  }

  return 'These modules were not found:';
}

function forgetToInstall (missingDependencies) {
  const moduleNames = missingDependencies.map(missingDependency => missingDependency.module);

  if (missingDependencies.length === 1) {
    return `To install it, you can run: npm install --save ${moduleNames.join(' ')}`;
  }

  return `To install them, you can run: npm install --save ${moduleNames.join(' ')}`;
}

function isRelative (module) {
  return module.startsWith('./');
}

function groupModules (errors) {
  const missingModule = new Map();

  errors.forEach((error) => {
    if (!missingModule.has(error.module)) {
      missingModule.set(error.module, [])
    }
    missingModule.get(error.module).push(error);
  });

  return Array.from(missingModule.keys()).map(module => ({
    module: module,
    relative: isRelative(module),
    errors: missingModule.get(module),
  }));
}

function formatFileList (files) {
  const length = files.length;
  if (!length) return '';
  return ` in ${files[0]}${files[1] ? `, ${files[1]}` : ''}${length > 2 ? ` and ${length - 2} other${length === 3 ? '' : 's'}` : ''}`;
}

function formatGroup (group) {
  const files = group.errors.map(e => e.file).filter(Boolean);
  return `* ${group.module}${formatFileList(files)}`;
}

function formatErrors (errors) {
  if (errors.length === 0) {
    return [];
  }

  const groups = groupModules(errors);
  const missingDependencies = groups.filter(group => !group.relative);

  return concat(
    moduleNotFound(errors.length),
    '',
    groups.map(formatGroup),
    missingDependencies.length === 0 ? undefined : [
      '',
      forgetToInstall(missingDependencies)
    ]
  );
}

function format (errors) {
  return formatErrors(errors.filter((e) => (
    e.type === 'module-not-found'
  )));
}

module.exports = format;
