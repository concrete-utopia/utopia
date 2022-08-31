import { ProjectContentTreeRoot } from '../../../components/assets'
import {
  addImport,
  emptyImports,
  mergeImports,
} from '../../../core/workers/common/project-file-utils'
import { withUnderlyingTarget } from '../../../components/editor/store/editor-state'
import { ElementPath, Imports, NodeModules } from '../../../core/shared/project-file-types'
import { CanvasCommand } from '../commands/commands'
import { reparentElement } from '../commands/reparent-element-command'
import {
  ElementInstanceMetadataMap,
  isIntrinsicElement,
  isJSXElement,
  JSXElement,
  JSXElementChild,
  walkElement,
} from '../../../core/shared/element-template'
import * as EP from '../../../core/shared/element-path'
import { getImportsFor, importedFromWhere } from '../../../components/editor/import-utils'
import { forceNotNull } from '../../../core/shared/optional-utils'
import { addImportsToFile } from '../commands/add-imports-to-file-command'
import { BuiltInDependencies } from '../../../core/es-modules/package-manager/built-in-dependencies-list'
import { CSSCursor } from '../canvas-types'
import { addToReparentedToPaths } from './add-to-reparented-to-paths-command'
import { getStoryboardElementPath } from '../../../core/model/scene-utils'
import { getUtopiaID } from '../../../core/model/element-template-utils'
import { addElement } from '../commands/add-element-command'

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
      const importsToAdd = getReparentImports(
        toReparent.target,
        projectContents,
        nodeModules,
        openFile,
        newTargetFilePath,
        builtInDependencies,
      )
      commands.push(addImportsToFile('always', newTargetFilePath, importsToAdd))
      commands.push(reparentElement('always', toReparent.target, newParent))
      newPath = EP.appendToPath(newParent, EP.toUid(toReparent.target))
      break
    case 'ELEMENT_TO_REPARENT':
      newPath = EP.appendToPath(newParent, getUtopiaID(toReparent.element))
      commands.push(addImportsToFile('always', newTargetFilePath, toReparent.imports))
      commands.push(addElement('always', newParent, toReparent.element))
      break
    default:
      const _exhaustiveCheck: never = toReparent
      throw new Error(`Unhandled to reparent value ${JSON.stringify(toReparent)}`)
  }

  commands.push(addToReparentedToPaths('mid-interaction', [newPath]))

  return {
    commands: commands,
    newPath: newPath,
  }
}

function getReparentImports(
  target: ElementPath,
  projectContents: ProjectContentTreeRoot,
  nodeModules: NodeModules,
  openFile: string | null | undefined,
  newTargetFilePath: string,
  builtInDependencies: BuiltInDependencies,
): Imports {
  return withUnderlyingTarget<Imports>(
    target,
    projectContents,
    nodeModules,
    openFile,
    emptyImports(),
    (success, element, underlyingTarget, underlyingFilePath) => {
      const importsInOriginFile = success.imports
      const topLevelElementsInOriginFile = success.topLevelElements
      const lastPathPart =
        EP.lastElementPathForPath(underlyingTarget) ?? EP.emptyStaticElementPathPart()

      let importsToAdd: Imports = emptyImports()
      // Walk down through the elements as elements within the element being reparented might also be imported.
      walkElement(element, lastPathPart, 0, (elem, subPath, depth) => {
        if (isJSXElement(elem)) {
          // Straight up ignore intrinsic elements as they wont be imported.
          if (!isIntrinsicElement(elem.name)) {
            const importedFromResult = importedFromWhere(
              underlyingFilePath,
              elem.name.baseVariable,
              topLevelElementsInOriginFile,
              importsInOriginFile,
            )

            if (importedFromResult != null) {
              switch (importedFromResult.type) {
                case 'SAME_FILE_ORIGIN':
                  importsToAdd = mergeImports(
                    newTargetFilePath,
                    importsToAdd,
                    getImportsFor(
                      builtInDependencies,
                      importsInOriginFile,
                      projectContents,
                      nodeModules,
                      underlyingFilePath,
                      elem.name.baseVariable,
                    ),
                  )
                  break
                case 'IMPORTED_ORIGIN':
                  if (importedFromResult.exportedName != null) {
                    importsToAdd = mergeImports(
                      newTargetFilePath,
                      importsToAdd,
                      getImportsFor(
                        builtInDependencies,
                        importsInOriginFile,
                        projectContents,
                        nodeModules,
                        underlyingFilePath,
                        importedFromResult.exportedName,
                      ),
                    )
                  }
                  break
                default:
                  const _exhaustiveCheck: never = importedFromResult
                  throw new Error(
                    `Unhandled imported from result ${JSON.stringify(importedFromResult)}`,
                  )
              }
            }
          }
        }
      })

      return importsToAdd
    },
  )
}

export function cursorForMissingReparentedItems(
  reparentedToPaths: Array<ElementPath>,
  spyMetadata: ElementInstanceMetadataMap,
): CSSCursor | null {
  for (const reparentedToPath of reparentedToPaths) {
    if (!(EP.toString(reparentedToPath) in spyMetadata)) {
      return CSSCursor.ReparentNotPermitted
    }
  }

  return null
}
