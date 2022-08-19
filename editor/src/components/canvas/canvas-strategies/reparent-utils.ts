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

interface GetReparentOutcomeResult {
  commands: Array<CanvasCommand>
  newPath: ElementPath
}

export function getReparentOutcome(
  builtInDependencies: BuiltInDependencies,
  projectContents: ProjectContentTreeRoot,
  nodeModules: NodeModules,
  openFile: string | null | undefined,
  selectedElement: ElementPath,
  targetParent: ElementPath | null,
): GetReparentOutcomeResult {
  // Cater for something being reparented to the canvas.
  let newParent: ElementPath
  if (targetParent == null) {
    const storyboardElementPath = getStoryboardElementPath(projectContents, openFile)
    if (storyboardElementPath == null) {
      console.warn(`Unable to find storyboard path.`)
      return {
        commands: [],
        newPath: selectedElement,
      }
    } else {
      newParent = storyboardElementPath
    }
  } else {
    newParent = targetParent
  }

  // Early exit if there's no need to make any change.
  if (EP.pathsEqual(newParent, EP.parentPath(selectedElement))) {
    return {
      commands: [],
      newPath: selectedElement,
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

  // Determine what imports need to also be carried over to the new location.
  const commandsToAddImports = withUnderlyingTarget<Array<CanvasCommand>>(
    selectedElement,
    projectContents,
    nodeModules,
    openFile,
    [],
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

      return [addImportsToFile('always', newTargetFilePath, importsToAdd)]
    },
  )

  let commands: Array<CanvasCommand> = []
  commands.push(reparentElement('always', selectedElement, newParent))

  const newPath = EP.appendToPath(newParent, EP.toUid(selectedElement))
  commands.push(addToReparentedToPaths('mid-interaction', [newPath]))
  commands.push(...commandsToAddImports)

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
      return CSSCursor.ReparentNotPermitted
    }
  }

  return null
}
