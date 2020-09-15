'use strict';

Object.defineProperty(exports, "__esModule", {
  value: true
});
exports.default = deepForceUpdate;
function traverseRenderedChildren(internalInstance, callback, argument) {
  callback(internalInstance, argument);

  if (internalInstance._renderedComponent) {
    traverseRenderedChildren(internalInstance._renderedComponent, callback, argument);
  } else {
    for (var key in internalInstance._renderedChildren) {
      if (internalInstance._renderedChildren.hasOwnProperty(key)) {
        traverseRenderedChildren(internalInstance._renderedChildren[key], callback, argument);
      }
    }
  }
}

function setPendingForceUpdate(internalInstance, shouldUpdate) {
  if (internalInstance._pendingForceUpdate === false && shouldUpdate(internalInstance)) {
    internalInstance._pendingForceUpdate = true;
  }
}

function forceUpdateIfPending(internalInstance, onUpdate) {
  if (internalInstance._pendingForceUpdate === true) {
    var publicInstance = internalInstance._instance;
    var updater = publicInstance.updater;


    if (typeof publicInstance.forceUpdate === 'function') {
      publicInstance.forceUpdate();
    } else if (updater && typeof updater.enqueueForceUpdate === 'function') {
      updater.enqueueForceUpdate(publicInstance);
    }
    onUpdate(internalInstance);
  }
}

function deepForceUpdateStack(instance, shouldUpdate, onUpdate) {
  var internalInstance = instance._reactInternalInstance;
  traverseRenderedChildren(internalInstance, setPendingForceUpdate, shouldUpdate);
  traverseRenderedChildren(internalInstance, forceUpdateIfPending, onUpdate);
}

function deepForceUpdate(instance) {
  // TODO: this is temporarily disabled because it's not in 2.x release line.
  // See https://github.com/gaearon/react-deep-force-update/issues/8
  var shouldUpdate = function shouldUpdate() {
    return true;
  };
  var onUpdate = function onUpdate() {};

  var root = instance._reactInternalFiber || instance._reactInternalInstance;
  if (typeof root.tag !== 'number') {
    // Traverse stack-based React tree.
    return deepForceUpdateStack(instance, shouldUpdate, onUpdate);
  }

  var node = root;
  while (true) {
    if (node.stateNode !== null && typeof node.type === 'function' && shouldUpdate(node)) {
      var publicInstance = node.stateNode;
      var updater = publicInstance.updater;

      if (typeof publicInstance.forceUpdate === 'function') {
        publicInstance.forceUpdate();
      } else if (updater && typeof updater.enqueueForceUpdate === 'function') {
        updater.enqueueForceUpdate(publicInstance);
      }
      onUpdate(node);
    }
    if (node.child) {
      node.child.return = node;
      node = node.child;
      continue;
    }
    if (node === root) {
      return undefined;
    }
    while (!node.sibling) {
      if (!node.return || node.return === root) {
        return undefined;
      }
      node = node.return;
    }
    node.sibling.return = node.return;
    node = node.sibling;
  }
}
module.exports = exports['default'];