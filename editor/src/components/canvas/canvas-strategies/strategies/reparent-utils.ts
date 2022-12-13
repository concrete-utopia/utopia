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
import { getUtopiaID } from '../../../../core/model/element-template-utils'
import { addElement } from '../../commands/add-element-command'
import { InteractionLifecycle } from '../canvas-strategy-types'

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
  targetParent: ElementPath | null,
  whenToRun: 'always' | 'on-complete',
  strategyLifecycle: InteractionLifecycle,
): GetReparentOutcomeResult | null {
  // Cater for something being reparented to the canvas.
  let newParent: ElementPath
  if (targetParent == null) {
    const storyboardElementPath = getStoryboardElementPath(projectContents, openFile)
    if (storyboardElementPath == null) {
      console.warn(`Unable to find storyboard path.`)
      return null
    } else {
      newParent = storyboardElementPath
    }
  } else {
    newParent = targetParent
  }

  // Early exit if there's no need to make any change.
  if (
    toReparent.type === 'PATH_TO_REPARENT' &&
    EP.pathsEqual(newParent, EP.parentPath(toReparent.target))
  ) {
    return {
      commands: [],
      newPath: toReparent.target,
    }
  }

  // Lookup the filename that will be added to.
  const newTargetFilePath = forceNotNull(
    `Unable to determine target path for ${newParent == null ? null : EP.toString(newParent)}`,
    withUnderlyingTarget(
      newParent,
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
      commands.push(reparentElement(whenToRun, toReparent.target, newParent, strategyLifecycle))
      newPath = EP.appendToPath(newParent, EP.toUid(toReparent.target))
      break
    case 'ELEMENT_TO_REPARENT':
      newPath = EP.appendToPath(newParent, getUtopiaID(toReparent.element))
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
