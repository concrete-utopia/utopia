import {
  findMaybeConditionalExpression,
  getConditionalActiveCase,
  maybeBranchConditionalCase,
} from '../../../../../core/model/conditionals'
import { MetadataUtils } from '../../../../../core/model/element-metadata-utils'
import { Either, foldEither, left, right } from '../../../../../core/shared/either'
import * as EP from '../../../../../core/shared/element-path'
import {
  ElementInstanceMetadataMap,
  JSXElementChild,
  elementReferencesElsewhere,
  elementReferencesElsewherePaths,
  emptyComments,
  isJSXElement,
  jsExpressionValue,
} from '../../../../../core/shared/element-template'
import { setJSXValuesAtPaths } from '../../../../../core/shared/jsx-attributes'
import { ElementPath } from '../../../../../core/shared/project-file-types'
import * as PP from '../../../../../core/shared/property-path'
import { ProjectContentTreeRoot } from '../../../../assets'
import { EditorState, ElementProps } from '../../../../editor/store/editor-state'
import {
  InsertionPath,
  childInsertionPath,
  conditionalClauseInsertionPath,
} from '../../../../editor/store/insertion-path'
import { CSSCursor } from '../../../canvas-types'
import { setCursorCommand } from '../../../commands/set-cursor-command'
import {
  InteractionCanvasState,
  StrategyApplicationResult,
  strategyApplicationResult,
} from '../../canvas-strategy-types'
import * as ObjectPath from 'object-path'

export function isAllowedToReparent(
  projectContents: ProjectContentTreeRoot,
  startingMetadata: ElementInstanceMetadataMap,
  target: ElementPath,
): boolean {
  if (MetadataUtils.isElementGenerated(target)) {
    return false
  } else {
    const metadata = MetadataUtils.findElementByElementPath(startingMetadata, target)
    if (metadata == null) {
      const parentPath = EP.parentPath(target)
      const conditional = findMaybeConditionalExpression(parentPath, startingMetadata)
      if (conditional != null) {
        return maybeBranchConditionalCase(parentPath, conditional, target) != null
      }
      return false
    } else {
      return foldEither(
        (_) => true,
        (elementFromMetadata) => {
          return (
            !elementReferencesElsewhere(elementFromMetadata) &&
            MetadataUtils.targetHonoursPropsPosition(projectContents, metadata)
          )
        },
        metadata.element,
      )
    }
  }
}

export function canCopyElement(
  editor: EditorState,
  target: ElementPath,
): Either<string, ElementPath> {
  const metadata = MetadataUtils.findElementByElementPath(editor.jsxMetadata, target)
  if (MetadataUtils.isElementGenerated(target)) {
    return left('Cannot copy generated element')
  }

  if (metadata == null) {
    const parentPath = EP.parentPath(target)
    const conditional = findMaybeConditionalExpression(parentPath, editor.jsxMetadata)
    if (conditional != null) {
      const branchCase = maybeBranchConditionalCase(parentPath, conditional, target)
      if (branchCase == null) {
        return left('Cannot copy empty branch')
      }
      return right(target)
    }
    return left('Cannot find element metadata')
  }

  return foldEither(
    (_) => right(target),
    () => {
      if (!MetadataUtils.targetHonoursPropsPosition(editor.projectContents, metadata)) {
        return left('target does not honour positioning props')
      }
      return right(target)
    },
    metadata.element,
  )
}

export function replacePropsWithRuntimeValues(
  elementProps: ElementProps,
  element: JSXElementChild,
): JSXElementChild {
  if (!isJSXElement(element)) {
    return element
  }

  // gather property paths that are defined elsewhere
  const paths = elementReferencesElsewherePaths(element, PP.create())

  // try and get the values from allElementProps, replace everything else with undefined
  const valuesAndPaths = paths.map((propertyPath) => ({
    path: propertyPath,
    value: jsExpressionValue(
      ObjectPath.get(elementProps, PP.getElements(propertyPath)),
      emptyComments,
    ),
  }))

  return foldEither(
    () => {
      return element
    },
    (updatedProps) => {
      return {
        ...element,
        props: updatedProps,
      }
    },
    setJSXValuesAtPaths(element.props, valuesAndPaths),
  )
}

export function ifAllowedToReparent(
  canvasState: InteractionCanvasState,
  startingMetadata: ElementInstanceMetadataMap,
  targets: Array<ElementPath>,
  ifAllowed: () => StrategyApplicationResult,
): StrategyApplicationResult {
  const allowed = targets.every((target) => {
    return isAllowedToReparent(canvasState.projectContents, startingMetadata, target)
  })
  if (allowed) {
    return ifAllowed()
  } else {
    return strategyApplicationResult([setCursorCommand(CSSCursor.NotPermitted)], {}, 'failure')
  }
}

export function getInsertionPathForReparentTarget(
  newParent: ElementPath,
  metadata: ElementInstanceMetadataMap,
): InsertionPath {
  const conditional = findMaybeConditionalExpression(newParent, metadata)
  if (conditional == null) {
    return childInsertionPath(newParent)
  }
  const clause = getConditionalActiveCase(newParent, conditional, metadata)
  if (clause == null) {
    return childInsertionPath(newParent)
  }
  return conditionalClauseInsertionPath(newParent, clause, 'replace')
}
