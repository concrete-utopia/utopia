import { ElementPath } from '../shared/project-file-types'
import * as EP from '../shared/element-path'
import {
  ChildOrAttribute,
  childOrBlockIsChild,
  JSXConditionalExpression,
} from '../shared/element-template'
import { ElementPathTree } from '../shared/element-path-tree'
import { getUtopiaID } from './element-template-utils'
import { assertNever } from '../shared/utils'

export type ThenOrElse = 'then' | 'else'

export function thenOrElsePathPart(thenOrElse: ThenOrElse): string {
  switch (thenOrElse) {
    case 'then':
      return 'then-case'
    case 'else':
      return 'else-case'
    default:
      assertNever(thenOrElse)
  }
}

export function getThenOrElsePath(elementPath: ElementPath, thenOrElse: ThenOrElse): ElementPath {
  return EP.appendToPath(elementPath, thenOrElsePathPart(thenOrElse))
}

// Get the path for the clause (then or else) of a conditional.
export function getConditionalClausePath(
  conditionalPath: ElementPath,
  conditionalClause: ChildOrAttribute,
  thenOrElse: ThenOrElse,
): ElementPath {
  if (childOrBlockIsChild(conditionalClause)) {
    return EP.appendToPath(conditionalPath, getUtopiaID(conditionalClause))
  } else {
    return getThenOrElsePath(conditionalPath, thenOrElse)
  }
}

// Ensure that the children of a conditional are the whenTrue clause followed
// by the whenFalse clause.
export function reorderConditionalChildPathTrees(
  conditional: JSXConditionalExpression,
  conditionalPath: ElementPath,
  childPaths: Array<ElementPathTree>,
): Array<ElementPathTree> {
  if (childPaths.length > 2) {
    throw new Error(`Too many child paths.`)
  } else {
    let result: Array<ElementPathTree> = []

    // The whenTrue clause should be first.
    const thenPath = getConditionalClausePath(conditionalPath, conditional.whenTrue, 'then')
    const thenPathTree = childPaths.find((childPath) => EP.pathsEqual(childPath.path, thenPath))
    if (thenPathTree != null) {
      result.push(thenPathTree)
    }

    // The whenFalse clause should be second.
    const elsePath = getConditionalClausePath(conditionalPath, conditional.whenFalse, 'else')
    const elsePathTree = childPaths.find((childPath) => EP.pathsEqual(childPath.path, elsePath))
    if (elsePathTree != null) {
      result.push(elsePathTree)
    }

    return result
  }
}
