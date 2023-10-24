import type { ProjectContentTreeRoot } from '../../../assets'
import type { AllElementProps } from '../../../editor/store/editor-state'
import { withUnderlyingTarget } from '../../../editor/store/editor-state'
import type { ElementPath, Imports, NodeModules } from '../../../../core/shared/project-file-types'
import type { CanvasCommand } from '../../commands/commands'
import { reparentElement } from '../../commands/reparent-element-command'
import type {
  ElementInstanceMetadataMap,
  JSXElementChild,
} from '../../../../core/shared/element-template'
import { isJSXConditionalExpression, isJSXElement } from '../../../../core/shared/element-template'
import * as EP from '../../../../core/shared/element-path'
import { getRequiredImportsForElement } from '../../../editor/import-utils'
import { forceNotNull } from '../../../../core/shared/optional-utils'
import { addImportsToFile } from '../../commands/add-imports-to-file-command'
import type { BuiltInDependencies } from '../../../../core/es-modules/package-manager/built-in-dependencies-list'
import { CSSCursor } from '../../canvas-types'
import { addToReparentedToPaths } from '../../commands/add-to-reparented-to-paths-command'
import { getStoryboardElementPath } from '../../../../core/model/scene-utils'
import { generateUidWithExistingComponents } from '../../../../core/model/element-template-utils'
import { addElement } from '../../commands/add-element-command'
import type { CustomStrategyState, InteractionCanvasState } from '../canvas-strategy-types'
import { duplicateElement } from '../../commands/duplicate-element-command'
import { wildcardPatch } from '../../commands/wildcard-patch-command'
import { hideInNavigatorCommand } from '../../commands/hide-in-navigator-command'
import { MetadataUtils } from '../../../../core/model/element-metadata-utils'
import type { InsertionPath } from '../../../editor/store/insertion-path'
import {
  childInsertionPath,
  getElementPathFromInsertionPath,
  getInsertionPath,
} from '../../../editor/store/insertion-path'
import { getUtopiaID } from '../../../../core/shared/uid-utils'
import type { IndexPosition } from '../../../../utils/utils'
import { fastForEach } from '../../../../core/shared/utils'
import { addElements } from '../../commands/add-elements-command'
import type { ElementPathTrees } from '../../../../core/shared/element-path-tree'
import { getRequiredGroupTrueUps } from '../../commands/queue-true-up-command'
import type { Either } from '../../../../core/shared/either'
import { left, right } from '../../../../core/shared/either'
import { maybeBranchConditionalCase } from '../../../../core/model/conditionals'
import type { NonEmptyArray } from '../../../../core/shared/array-utils'
import {
  mapDropNulls,
  isNonEmptyArray,
  lastOfNonEmptyArray,
} from '../../../../core/shared/array-utils'
import type { CanvasRectangle } from '../../../../core/shared/math-utils'
import {
  boundingRectangleArray,
  isInfinityRectangle,
  nullIfInfinity,
} from '../../../../core/shared/math-utils'
import { isElementRenderedBySameComponent } from './reparent-helpers/reparent-helpers'
import type { ParsedCopyData } from '../../../../utils/clipboard'

interface GetReparentOutcomeResult {
  commands: Array<CanvasCommand>
  newPath: ElementPath
}

export interface PathToReparent {
  type: 'PATH_TO_REPARENT'
  target: ElementPath
}

export function pathToReparent(target: ElementPath): PathToReparent {
  return {
    type: 'PATH_TO_REPARENT',
    target: target,
  }
}

export interface ElementToReparent {
  type: 'ELEMENT_TO_REPARENT'
  element: JSXElementChild
  imports: Imports
}

export function elementToReparent(element: JSXElementChild, imports: Imports): ElementToReparent {
  return {
    type: 'ELEMENT_TO_REPARENT',
    element: element,
    imports: imports,
  }
}

export type ToReparent = PathToReparent | ElementToReparent

export function getReparentOutcome(
  metadata: ElementInstanceMetadataMap,
  pathTrees: ElementPathTrees,
  allElementProps: AllElementProps,
  builtInDependencies: BuiltInDependencies,
  projectContents: ProjectContentTreeRoot,
  nodeModules: NodeModules,
  toReparent: ToReparent,
  targetParent: InsertionPath,
  whenToRun: 'always' | 'on-complete',
  indexPosition: IndexPosition | null,
): GetReparentOutcomeResult | null {
  const newParentElementPath = getElementPathFromInsertionPath(targetParent)

  const newTargetFilePath = withUnderlyingTarget(
    newParentElementPath,
    projectContents,
    null,
    (success, element, underlyingTarget, underlyingFilePath) => {
      return underlyingFilePath
    },
  )

  if (newTargetFilePath == null) {
    return null
  }

  let commands: Array<CanvasCommand> = []
  let newPath: ElementPath

  switch (toReparent.type) {
    case 'PATH_TO_REPARENT':
      const importsToAdd = getRequiredImportsForElement(
        toReparent.target,
        projectContents,
        nodeModules,
        newTargetFilePath,
        builtInDependencies,
      )
      commands.push(addImportsToFile(whenToRun, newTargetFilePath, importsToAdd))
      commands.push(reparentElement(whenToRun, toReparent.target, targetParent, indexPosition))
      commands.push(
        ...getRequiredGroupTrueUps(
          projectContents,
          metadata,
          pathTrees,
          allElementProps,
          toReparent.target,
        ),
      )
      newPath = EP.appendToPath(newParentElementPath, EP.toUid(toReparent.target))
      break
    case 'ELEMENT_TO_REPARENT':
      newPath = EP.appendToPath(newParentElementPath, getUtopiaID(toReparent.element))
      commands.push(addImportsToFile(whenToRun, newTargetFilePath, toReparent.imports))
      commands.push(
        addElement(whenToRun, targetParent, toReparent.element, {
          indexPosition: indexPosition ?? undefined,
        }),
      )
      break
    default:
      const _exhaustiveCheck: never = toReparent
      throw new Error(`Unhandled to reparent value ${JSON.stringify(toReparent)}`)
  }

  if (whenToRun === 'always') {
    commands.push(addToReparentedToPaths('mid-interaction', [newPath]))
  }

  return {
    commands: commands,
    newPath: newPath,
  }
}

export function getReparentOutcomeMultiselect(
  builtInDependencies: BuiltInDependencies,
  projectContents: ProjectContentTreeRoot,
  nodeModules: NodeModules,
  openFile: string | null | undefined,
  toReparentMultiple: Array<ToReparent>,
  targetParent: InsertionPath | null,
  whenToRun: 'always' | 'on-complete',
  indexPosition: IndexPosition | null,
): Array<CanvasCommand> | null {
  // Cater for something being reparented to the canvas.
  let newParent: InsertionPath
  if (targetParent == null) {
    const storyboardElementPath = getStoryboardElementPath(projectContents, openFile)
    if (storyboardElementPath == null) {
      console.warn(`Unable to find storyboard path.`)
      return null
    } else {
      newParent = childInsertionPath(storyboardElementPath)
    }
  } else {
    newParent = targetParent
  }

  const newParentElementPath = getElementPathFromInsertionPath(newParent)

  // Lookup the filename that will be added to.
  const newTargetFilePath = forceNotNull(
    `Unable to determine target path for ${
      newParent == null ? null : EP.toString(newParentElementPath)
    }`,
    withUnderlyingTarget(
      newParentElementPath,
      projectContents,
      null,
      (_success, _element, _underlyingTarget, underlyingFilePath) => {
        return underlyingFilePath
      },
    ),
  )

  let commands: Array<CanvasCommand> = []
  let elementsToInsert: Array<JSXElementChild> = []
  fastForEach(toReparentMultiple, (toReparent) => {
    switch (toReparent.type) {
      case 'PATH_TO_REPARENT':
        const importsToAdd = getRequiredImportsForElement(
          toReparent.target,
          projectContents,
          nodeModules,
          newTargetFilePath,
          builtInDependencies,
        )
        commands.push(addImportsToFile(whenToRun, newTargetFilePath, importsToAdd))
        commands.push(reparentElement(whenToRun, toReparent.target, newParent, indexPosition))
        break
      case 'ELEMENT_TO_REPARENT':
        commands.push(addImportsToFile(whenToRun, newTargetFilePath, toReparent.imports))
        elementsToInsert.push(toReparent.element)
        break
      default:
        const _exhaustiveCheck: never = toReparent
        throw new Error(`Unhandled to reparent value ${JSON.stringify(toReparent)}`)
    }
  })

  if (elementsToInsert.length > 0) {
    commands.push(
      addElements(whenToRun, newParent, elementsToInsert, {
        indexPosition: indexPosition ?? undefined,
      }),
    )
  }

  return commands
}

export function cursorForMissingReparentedItems(
  reparentedToPaths: Array<ElementPath>,
  spyMetadata: ElementInstanceMetadataMap,
): CSSCursor | null {
  for (const reparentedToPath of reparentedToPaths) {
    if (!(EP.toString(reparentedToPath) in spyMetadata)) {
      return CSSCursor.NotPermitted
    }
  }

  return null
}

export function placeholderCloneCommands(
  canvasState: InteractionCanvasState,
  customStrategyState: CustomStrategyState,
  filteredSelectedElements: Array<ElementPath>,
  newParent: ElementPath,
): { commands: Array<CanvasCommand>; duplicatedElementNewUids: { [elementPath: string]: string } } {
  let duplicatedElementNewUids: { [elementPath: string]: string } = {
    ...customStrategyState.duplicatedElementNewUids,
  }
  const commands = filteredSelectedElements.flatMap((elementPath) => {
    const element = MetadataUtils.findElementByElementPath(
      canvasState.startingMetadata,
      elementPath,
    )
    // we want to keep a placeholder element where the original dragged element was to avoid the new parent shifting around on the screen
    // change this if you don't need keep the starting layout
    const hasCommonAncestor =
      EP.getCommonParent([newParent, EP.parentPath(elementPath)]) != null &&
      !MetadataUtils.isPositionAbsolute(element)
    if (hasCommonAncestor) {
      const selectedElementString = EP.toString(elementPath)
      const newUid =
        customStrategyState.duplicatedElementNewUids[selectedElementString] ??
        generateUidWithExistingComponents(canvasState.projectContents)
      duplicatedElementNewUids[selectedElementString] = newUid

      const newPath = EP.appendToPath(EP.parentPath(elementPath), newUid)

      return [
        duplicateElement('mid-interaction', elementPath, newUid),
        wildcardPatch('mid-interaction', {
          hiddenInstances: { $push: [newPath] },
        }),
        hideInNavigatorCommand([newPath]),
      ]
    } else {
      return []
    }
  })
  return { commands: commands, duplicatedElementNewUids: duplicatedElementNewUids }
}

function rectangleSizesEqual(a: CanvasRectangle, b: CanvasRectangle): boolean {
  return a.height === b.height && a.width === b.width
}

export type ReparentTargetForPaste =
  | {
      type: 'sibling'
      siblingPath: ElementPath
      siblingBounds: CanvasRectangle
      parentPath: InsertionPath
    }
  | { type: 'parent'; parentPath: InsertionPath }

type PasteParentNotFoundError =
  | 'Cannot find a suitable parent'
  | 'Cannot insert component instance into component definition'

function checkComponentNotInsertedIntoOwnDefinition(
  selectedViews: NonEmptyArray<ElementPath>,
  metadata: ElementInstanceMetadataMap,
  elementsToInsert: JSXElementChild[],
): boolean {
  const parentTarget = EP.getCommonParentOfNonemptyPathArray(selectedViews, true)

  const jsxElements = elementsToInsert.filter(isJSXElement)

  return jsxElements.some((element) =>
    isElementRenderedBySameComponent(metadata, parentTarget, element),
  )
}

function insertIntoSlot(
  selectedViews: NonEmptyArray<ElementPath>,
  metadata: ElementInstanceMetadataMap,
  projectContents: ProjectContentTreeRoot,
  elementPathTrees: ElementPathTrees,
  numberOfElementsToInsert: number,
): ReparentTargetForPaste | null {
  const targetPath = selectedViews[0]
  const parentPath = EP.parentPath(targetPath)
  const parentElement = withUnderlyingTarget(parentPath, projectContents, null, (_, element) => {
    return element
  })

  if (parentElement == null || !isJSXConditionalExpression(parentElement)) {
    return null
  }

  const wrapperFragmentUID = generateUidWithExistingComponents(projectContents)
  const conditionalCase = maybeBranchConditionalCase(parentPath, parentElement, targetPath)
  if (conditionalCase == null) {
    return null
  }

  const parentInsertionPath = getInsertionPath(
    targetPath,
    projectContents,
    metadata,
    elementPathTrees,
    wrapperFragmentUID,
    numberOfElementsToInsert,
  )

  if (parentInsertionPath == null) {
    return null
  }

  return { type: 'parent', parentPath: parentInsertionPath }
}

function pasteNextToSameSizedElement(
  copyData: ParsedCopyData,
  selectedViews: NonEmptyArray<ElementPath>,
  metadata: ElementInstanceMetadataMap,
): ReparentTargetForPaste | null {
  const selectedViewsAABB = boundingRectangleArray(
    selectedViews.map((path) =>
      nullIfInfinity(MetadataUtils.getFrameInCanvasCoords(path, metadata)),
    ),
  )
  const pastedElementsAABB = boundingRectangleArray(
    copyData.elementPaste.map((element) =>
      nullIfInfinity(
        MetadataUtils.getFrameInCanvasCoords(
          element.originalElementPath,
          copyData.originalContextMetadata,
        ),
      ),
    ),
  )

  if (
    selectedViewsAABB == null ||
    pastedElementsAABB == null ||
    !rectangleSizesEqual(selectedViewsAABB, pastedElementsAABB)
  ) {
    return null
  }

  const parentPath = EP.getCommonParentOfNonemptyPathArray(selectedViews)

  const pastedElementNames = mapDropNulls(
    (element) => MetadataUtils.getJSXElementName(element.element),
    copyData.elementPaste,
  )

  const targetElementSupportsInsertedElement = MetadataUtils.canInsertElementsToTargetText(
    parentPath,
    metadata,
    pastedElementNames,
  )

  if (!targetElementSupportsInsertedElement) {
    return null
  }

  const parentInstance = MetadataUtils.findElementByElementPath(metadata, parentPath)

  const isSelectedViewParentAutolayouted = MetadataUtils.isFlexLayoutedContainer(parentInstance)
  const selectedViewsAbsolute = selectedViews.every((path) =>
    MetadataUtils.isPositionAbsolute(MetadataUtils.findElementByElementPath(metadata, path)),
  )
  const pastedElementsAbsolute = copyData.elementPaste.every((element) =>
    MetadataUtils.isPositionAbsolute(
      MetadataUtils.findElementByElementPath(
        copyData.originalContextMetadata,
        element.originalElementPath,
      ),
    ),
  )

  const pastingAbsoluteToAbsolute = selectedViewsAbsolute && pastedElementsAbsolute

  if (
    (isSelectedViewParentAutolayouted || pastingAbsoluteToAbsolute) &&
    targetElementSupportsInsertedElement
  ) {
    return {
      type: 'sibling',
      siblingPath: lastOfNonEmptyArray(selectedViews),
      siblingBounds: selectedViewsAABB,
      parentPath: childInsertionPath(parentPath),
    }
  }
  return null
}

function pasteIntoParentOrGrandparent(
  elementsToInsert: JSXElementChild[],
  projectContents: ProjectContentTreeRoot,
  selectedViews: NonEmptyArray<ElementPath>,
  metadata: ElementInstanceMetadataMap,
  elementPathTree: ElementPathTrees,
): ReparentTargetForPaste | null {
  const pastedElementNames = mapDropNulls(
    (element) => (element.type === 'JSX_ELEMENT' ? element.name : null),
    elementsToInsert,
  )

  const parentTarget = EP.getCommonParentOfNonemptyPathArray(selectedViews, true)

  // paste into parent
  const targetElementSupportsInsertedElement = MetadataUtils.canInsertElementsToTargetText(
    parentTarget,
    metadata,
    pastedElementNames,
  )
  if (
    MetadataUtils.targetSupportsChildren(
      projectContents,
      metadata,
      parentTarget,
      elementPathTree,
    ) &&
    targetElementSupportsInsertedElement
  ) {
    return { type: 'parent', parentPath: childInsertionPath(parentTarget) }
  }

  // paste into parent of parent
  const parentOfSelected = EP.parentPath(parentTarget)
  if (
    MetadataUtils.targetSupportsChildren(
      projectContents,
      metadata,
      parentOfSelected,
      elementPathTree,
    )
  ) {
    return { type: 'parent', parentPath: childInsertionPath(parentOfSelected) }
  }
  return null
}

export function getTargetParentForOneShotInsertion(
  storyboardPath: ElementPath,
  projectContents: ProjectContentTreeRoot,
  selectedViews: Array<ElementPath>,
  metadata: ElementInstanceMetadataMap,
  elementsToInsert: JSXElementChild[],
  elementPathTree: ElementPathTrees,
): Either<PasteParentNotFoundError, ReparentTargetForPaste> {
  if (!isNonEmptyArray(selectedViews)) {
    return right({ type: 'parent', parentPath: childInsertionPath(storyboardPath) })
  }

  if (checkComponentNotInsertedIntoOwnDefinition(selectedViews, metadata, elementsToInsert)) {
    return left('Cannot insert component instance into component definition')
  }

  const insertIntoSlotResult = insertIntoSlot(
    selectedViews,
    metadata,
    projectContents,
    elementPathTree,
    elementsToInsert.length,
  )
  if (insertIntoSlotResult != null) {
    return right(insertIntoSlotResult)
  }

  const pasteIntoParentOrGrandparentResult = pasteIntoParentOrGrandparent(
    elementsToInsert,
    projectContents,
    selectedViews,
    metadata,
    elementPathTree,
  )
  if (pasteIntoParentOrGrandparentResult != null) {
    return right(pasteIntoParentOrGrandparentResult)
  }
  return left('Cannot find a suitable parent')
}

export function getTargetParentForPaste(
  storyboardPath: ElementPath,
  projectContents: ProjectContentTreeRoot,
  selectedViews: Array<ElementPath>,
  metadata: ElementInstanceMetadataMap,
  copyData: ParsedCopyData,
  elementPathTree: ElementPathTrees,
): Either<PasteParentNotFoundError, ReparentTargetForPaste> {
  if (!isNonEmptyArray(selectedViews)) {
    return right({ type: 'parent', parentPath: childInsertionPath(storyboardPath) })
  }
  const pastedJSXElements = mapDropNulls(
    (p) =>
      MetadataUtils.getJSXElementFromMetadata(
        copyData.originalContextMetadata,
        p.originalElementPath,
      ),
    copyData.elementPaste,
  )
  if (checkComponentNotInsertedIntoOwnDefinition(selectedViews, metadata, pastedJSXElements)) {
    return left('Cannot insert component instance into component definition')
  }

  const insertIntoSlotResult = insertIntoSlot(
    selectedViews,
    metadata,
    projectContents,
    elementPathTree,
    copyData.elementPaste.length,
  )
  if (insertIntoSlotResult != null) {
    return right(insertIntoSlotResult)
  }

  const pasteNextToSameSizedElementResult = pasteNextToSameSizedElement(
    copyData,
    selectedViews,
    metadata,
  )
  if (pasteNextToSameSizedElementResult != null) {
    return right(pasteNextToSameSizedElementResult)
  }

  const pasteIntoParentOrGrandparentResult = pasteIntoParentOrGrandparent(
    copyData.elementPaste.map((e) => e.element),
    projectContents,
    selectedViews,
    metadata,
    elementPathTree,
  )
  if (pasteIntoParentOrGrandparentResult != null) {
    return right(pasteIntoParentOrGrandparentResult)
  }
  return left('Cannot find a suitable parent')
}
