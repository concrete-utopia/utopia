import type { ProjectContentTreeRoot } from '../../../assets'
import type { AllElementProps } from '../../../editor/store/editor-state'
import {
  getJSXElementFromProjectContents,
  withUnderlyingTarget,
} from '../../../editor/store/editor-state'
import {
  type ElementPath,
  type Imports,
  type NodeModules,
} from '../../../../core/shared/project-file-types'
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
import { assertNever, fastForEach } from '../../../../core/shared/utils'
import { addElements } from '../../commands/add-elements-command'
import type { ElementPathTrees } from '../../../../core/shared/element-path-tree'
import { getRequiredGroupTrueUps } from '../../commands/queue-true-up-command'
import type { Either } from '../../../../core/shared/either'
import { flatMapEither, left, right } from '../../../../core/shared/either'
import { maybeBranchConditionalCase } from '../../../../core/model/conditionals'
import type { NonEmptyArray } from '../../../../core/shared/array-utils'
import {
  mapDropNulls,
  isNonEmptyArray,
  lastOfNonEmptyArray,
} from '../../../../core/shared/array-utils'
import type { CanvasRectangle } from '../../../../core/shared/math-utils'
import { boundingRectangleArray, nullIfInfinity } from '../../../../core/shared/math-utils'
import { isElementRenderedBySameComponent } from './reparent-helpers/reparent-helpers'
import type { ParsedCopyData } from '../../../../utils/clipboard'
import { getParseSuccessForFilePath } from '../../canvas-utils'
import { renameDuplicateImports } from '../../../../core/shared/import-shared-utils'
import { set } from '../../../../core/shared/optics/optic-utilities'
import { fromField, fromTypeGuard } from '../../../../core/shared/optics/optic-creators'
import type { PropertyControlsInfo } from '../../../custom-code/code-file'

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

function adjustElementDuplicateName(
  element: ElementToReparent,
  targetFile: string,
  projectContents: ProjectContentTreeRoot,
): ElementToReparent {
  let elementToReturn = element
  const fileContents = getParseSuccessForFilePath(targetFile, projectContents)
  if (fileContents == null) {
    return elementToReturn
  }
  const { imports, duplicateNameMapping } = renameDuplicateImports(
    fileContents.imports,
    element.imports,
    targetFile,
  )
  // handle element name
  if (isJSXElement(element.element)) {
    const elementName = MetadataUtils.getJSXElementName(element.element)?.baseVariable
    if (elementName != null && duplicateNameMapping.has(elementName)) {
      elementToReturn = {
        ...element,
        element: {
          ...element.element,
          name: {
            ...element.element.name,
            baseVariable: duplicateNameMapping.get(elementName)!,
          },
        },
      }
    }
  }
  elementToReturn = {
    ...elementToReturn,
    imports: imports,
  }
  return elementToReturn
}

function getElementName(
  target: ElementPath,
  projectContents: ProjectContentTreeRoot,
  duplicateNameMapping: Map<string, string>,
): string | null {
  const element = getJSXElementFromProjectContents(target, projectContents)
  if (element == null) {
    return null
  }
  return duplicateNameMapping.get(element.name.baseVariable) ?? element.name.baseVariable
}

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
      const { imports, duplicateNameMapping } = getRequiredImportsForElement(
        toReparent.target,
        projectContents,
        nodeModules,
        newTargetFilePath,
        builtInDependencies,
      )
      commands.push(addImportsToFile(whenToRun, newTargetFilePath, imports))
      const elementName = getElementName(toReparent.target, projectContents, duplicateNameMapping)
      commands.push(
        reparentElement(whenToRun, toReparent.target, targetParent, indexPosition, elementName),
      )
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
      const adjustedElement = adjustElementDuplicateName(
        toReparent,
        newTargetFilePath,
        projectContents,
      )
      newPath = EP.appendToPath(newParentElementPath, getUtopiaID(adjustedElement.element))
      commands.push(addImportsToFile(whenToRun, newTargetFilePath, adjustedElement.imports))
      commands.push(
        addElement(whenToRun, targetParent, adjustedElement.element, {
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
        const { imports, duplicateNameMapping } = getRequiredImportsForElement(
          toReparent.target,
          projectContents,
          nodeModules,
          newTargetFilePath,
          builtInDependencies,
        )
        commands.push(addImportsToFile(whenToRun, newTargetFilePath, imports))
        const elementName = getElementName(toReparent.target, projectContents, duplicateNameMapping)
        commands.push(
          reparentElement(whenToRun, toReparent.target, newParent, indexPosition, elementName),
        )
        break
      case 'ELEMENT_TO_REPARENT':
        const adjustedElement = adjustElementDuplicateName(
          toReparent,
          newTargetFilePath,
          projectContents,
        )
        commands.push(addImportsToFile(whenToRun, newTargetFilePath, adjustedElement.imports))
        elementsToInsert.push(adjustedElement.element)
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

export interface SiblingReparentTargetForPaste {
  type: 'sibling'
  siblingPath: ElementPath
  siblingBounds: CanvasRectangle
  parentPath: InsertionPath
}

export interface ParentReparentTargetForPaste {
  type: 'parent'
  parentPath: InsertionPath
}

export type ReparentTargetForPaste = SiblingReparentTargetForPaste | ParentReparentTargetForPaste

export function isSiblingReparentTargetForPaste(
  target: ReparentTargetForPaste,
): target is SiblingReparentTargetForPaste {
  return target.type === 'sibling'
}

export function isParentReparentTargetForPaste(
  target: ReparentTargetForPaste,
): target is ParentReparentTargetForPaste {
  return target.type === 'parent'
}

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
  propertyControlsInfo: PropertyControlsInfo,
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
    propertyControlsInfo,
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

function canInsertIntoTarget(
  projectContents: ProjectContentTreeRoot,
  metadata: ElementInstanceMetadataMap,
  elementPathTree: ElementPathTrees,
  parentTarget: ElementPath,
  elementsToInsert: JSXElementChild[],
  propertyControlsInfo: PropertyControlsInfo,
): boolean {
  const pastedElementNames = mapDropNulls(
    (element) => (element.type === 'JSX_ELEMENT' ? element.name : null),
    elementsToInsert,
  )

  // paste into parent
  const targetElementSupportsInsertedElement = MetadataUtils.canInsertElementsToTargetText(
    parentTarget,
    metadata,
    pastedElementNames,
  )
  const supportsChildren = MetadataUtils.targetSupportsChildren(
    projectContents,
    metadata,
    parentTarget,
    elementPathTree,
    propertyControlsInfo,
  )

  return targetElementSupportsInsertedElement && supportsChildren
}

function pasteIntoParentOrGrandparent(
  elementsToInsert: JSXElementChild[],
  projectContents: ProjectContentTreeRoot,
  selectedViews: NonEmptyArray<ElementPath>,
  metadata: ElementInstanceMetadataMap,
  elementPathTree: ElementPathTrees,
  propertyControlsInfo: PropertyControlsInfo,
): ReparentTargetForPaste | null {
  const parentTarget = EP.getCommonParentOfNonemptyPathArray(selectedViews, true)

  if (
    canInsertIntoTarget(
      projectContents,
      metadata,
      elementPathTree,
      parentTarget,
      elementsToInsert,
      propertyControlsInfo,
    )
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
      propertyControlsInfo,
    )
  ) {
    return { type: 'parent', parentPath: childInsertionPath(parentOfSelected) }
  }
  return null
}

const intendedPathOptic = fromTypeGuard(isParentReparentTargetForPaste)
  .compose(fromField('parentPath'))
  .compose(fromField('intendedParentPath'))

export function applyElementCeilingToReparentTarget(
  projectContents: ProjectContentTreeRoot,
  metadata: ElementInstanceMetadataMap,
  elementsToInsert: JSXElementChild[],
  elementPathTree: ElementPathTrees,
  reparentTarget: Either<PasteParentNotFoundError, ReparentTargetForPaste>,
  elementCeiling: ElementPath | null,
  propertyControlsInfo: PropertyControlsInfo,
): Either<PasteParentNotFoundError, ReparentTargetForPaste> {
  if (elementCeiling == null) {
    return reparentTarget
  } else {
    return flatMapEither((targetForPaste) => {
      switch (targetForPaste.type) {
        case 'sibling':
          return left('Cannot find a suitable parent')
        case 'parent':
          switch (targetForPaste.parentPath.type) {
            case 'CHILD_INSERTION':
              const intendedParentPath = targetForPaste.parentPath.intendedParentPath
              // If the intended parent path is above the ceiling path then
              // change it to the ceiling path instead.
              const ceilingStaticPath = EP.dynamicPathToStaticPath(elementCeiling)
              if (EP.fullDepth(intendedParentPath) < EP.fullDepth(ceilingStaticPath)) {
                // Make sure it's valid to insert into.
                if (
                  canInsertIntoTarget(
                    projectContents,
                    metadata,
                    elementPathTree,
                    ceilingStaticPath,
                    elementsToInsert,
                    propertyControlsInfo,
                  )
                ) {
                  return right(set(intendedPathOptic, ceilingStaticPath, targetForPaste))
                } else {
                  return left('Cannot find a suitable parent')
                }
              } else {
                return right(targetForPaste)
              }
            case 'CONDITIONAL_CLAUSE_INSERTION':
              return left('Cannot find a suitable parent')
            default:
              return assertNever(targetForPaste.parentPath)
          }
        default:
          assertNever(targetForPaste)
      }
    }, reparentTarget)
  }
}

export function getTargetParentForOneShotInsertion(
  storyboardPath: ElementPath,
  projectContents: ProjectContentTreeRoot,
  selectedViews: Array<ElementPath>,
  metadata: ElementInstanceMetadataMap,
  elementsToInsert: JSXElementChild[],
  elementPathTree: ElementPathTrees,
  insertionCeiling: ElementPath | null,
  propertyControlsInfo: PropertyControlsInfo,
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
    propertyControlsInfo,
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
    propertyControlsInfo,
  )
  if (pasteIntoParentOrGrandparentResult != null) {
    return applyElementCeilingToReparentTarget(
      projectContents,
      metadata,
      elementsToInsert,
      elementPathTree,
      right(pasteIntoParentOrGrandparentResult),
      insertionCeiling,
      propertyControlsInfo,
    )
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
  propertyControlsInfo: PropertyControlsInfo,
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
    propertyControlsInfo,
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
    propertyControlsInfo,
  )
  if (pasteIntoParentOrGrandparentResult != null) {
    return right(pasteIntoParentOrGrandparentResult)
  }
  return left('Cannot find a suitable parent')
}
