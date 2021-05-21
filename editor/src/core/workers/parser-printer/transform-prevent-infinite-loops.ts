// I can't find the _actual_ original for this, but this version is from
// https://github.com/facebook/react/blob/d906de7f602df810c38aa622c83023228b047db6/scripts/babel/transform-prevent-infinite-loops.js

/**
 * Copyright (c) 2013-present, Facebook, Inc.
 * Copyright (c) 2017, Amjad Masad
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

'use strict'

// Based on https://repl.it/site/blog/infinite-loops.

export const InfiniteLoopMaxIterations = 10001
export const InfiniteLoopError = `Potential infinite loop: exceeded ${InfiniteLoopMaxIterations} iterations. If the editor is now unresponsive, you need to reload the page. Your code is safe, and you can fix the error after reloading.`

export default ({ types: t, template }: { types: any; template: any }) => {
  const buildGuard = template(`
    if (ITERATOR++ > MAX_ITERATIONS) {
      throw new RangeError(
        '${InfiniteLoopError}'
      );
    }
  `)

  return {
    visitor: {
      'WhileStatement|ForStatement|DoWhileStatement': (path: any) => {
        // An iterator that is incremented with each iteration
        const iterator = path.scope.parent.generateUidIdentifier('loopIt')
        const iteratorInit = t.numericLiteral(0)
        path.scope.parent.push({
          id: iterator,
          init: iteratorInit,
        })
        // If statement and throw error if it matches our criteria
        const guard = buildGuard({
          ITERATOR: iterator,
          MAX_ITERATIONS: t.numericLiteral(InfiniteLoopMaxIterations),
        })
        // No block statment e.g. `while (1) 1;`
        if (!(path.get('body').isBlockStatement() as boolean)) {
          const statement = path.get('body').node
          path.get('body').replaceWith(t.blockStatement([guard, statement]))
        } else {
          path.get('body').unshiftContainer('body', guard)
        }
      },
    },
  }
}
