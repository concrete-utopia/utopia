import { ProjectContentTreeRoot } from '../../../assets'
import {
  addImport,
  emptyImports,
  mergeImports,
} from '../../../../core/workers/common/project-file-utils'
import { withUnderlyingTarget } from '../../../editor/store/editor-state'
import { ElementPath, Imports, NodeModules } from '../../../../core/shared/project-file-types'
import { CanvasCommand } from '../../commands/commands'
import { reparentElement } from '../../commands/reparent-element-command'
import {
  ElementInstanceMetadataMap,
  isIntrinsicElement,
  isJSXElement,
  JSXElement,
  JSXElementChild,
  walkElement,
} from '../../../../core/shared/element-template'
import * as EP from '../../../../core/shared/element-path'
import {
  getImportsFor,
  getRequiredImportsForElement,
  importedFromWhere,
} from '../../../editor/import-utils'
import { forceNotNull } from '../../../../core/shared/optional-utils'
import { addImportsToFile } from '../../commands/add-imports-to-file-command'
import { BuiltInDependencies } from '../../../../core/es-modules/package-manager/built-in-dependencies-list'
import { CSSCursor } from '../../canvas-types'
import { addToReparentedToPaths } from '../../commands/add-to-reparented-to-paths-command'
import { getStoryboardElementPath } from '../../../../core/model/scene-utils'
import { generateUidWithExistingComponents } from '../../../../core/model/element-template-utils'
import { addElement } from '../../commands/add-element-command'
import {
  CustomStrategyState,
  InteractionCanvasState,
  InteractionLifecycle,
} from '../canvas-strategy-types'
import { duplicateElement } from '../../commands/duplicate-element-command'
import { wildcardPatch } from '../../commands/wildcard-patch-command'
import { hideInNavigatorCommand } from '../../commands/hide-in-navigator-command'
import { MetadataUtils } from '../../../../core/model/element-metadata-utils'
import {
  childInsertionPath,
  getElementPathFromInsertionPath,
  InsertionPath,
  isChildInsertionPath,
} from '../../../editor/store/insertion-path'
import { getUtopiaID } from '../../../../core/shared/uid-utils'

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
  builtInDependencies: BuiltInDependencies,
  projectContents: ProjectContentTreeRoot,
  nodeModules: NodeModules,
  openFile: string | null | undefined,
  toReparent: ToReparent,
  targetParent: InsertionPath | null,
  whenToRun: 'always' | 'on-complete',
): GetReparentOutcomeResult | null {
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

  // Early exit if there's no need to make any change.
  if (
    toReparent.type === 'PATH_TO_REPARENT' &&
    isChildInsertionPath(newParent) &&
    EP.pathsEqual(newParent.intendedParentPath, EP.parentPath(toReparent.target))
  ) {
    return {
      commands: [],
      newPath: toReparent.target,
    }
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
      nodeModules,
      openFile,
      null,
      (success, element, underlyingTarget, underlyingFilePath) => {
        return underlyingFilePath
      },
    ),
  )

  let commands: Array<CanvasCommand> = []
  let newPath: ElementPath

  switch (toReparent.type) {
    case 'PATH_TO_REPARENT':
      const importsToAdd = getRequiredImportsForElement(
        toReparent.target,
        projectContents,
        nodeModules,
        openFile,
        newTargetFilePath,
        builtInDependencies,
      )
      commands.push(addImportsToFile(whenToRun, newTargetFilePath, importsToAdd))
      commands.push(reparentElement(whenToRun, toReparent.target, newParent))
      newPath = EP.appendToPath(newParentElementPath, EP.toUid(toReparent.target))
      break
    case 'ELEMENT_TO_REPARENT':
      newPath = EP.appendToPath(newParentElementPath, getUtopiaID(toReparent.element))
      commands.push(addImportsToFile(whenToRun, newTargetFilePath, toReparent.imports))
      commands.push(addElement(whenToRun, newParent, toReparent.element))
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
