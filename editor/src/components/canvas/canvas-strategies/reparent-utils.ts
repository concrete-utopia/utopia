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

export function getReparentCommands(
  builtInDependencies: BuiltInDependencies,
  projectContents: ProjectContentTreeRoot,
  nodeModules: NodeModules,
  openFile: string | null | undefined,
  selectedElement: ElementPath,
  newParent: ElementPath,
): Array<CanvasCommand> {
  let result: Array<CanvasCommand> = []

  const newTargetPath = forceNotNull(
    `Unable to determine target path for ${EP.toString(newParent)}`,
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
                    newTargetPath,
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
                      newTargetPath,
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

      return [addImportsToFile('always', newTargetPath, importsToAdd)]
    },
  )

  result.push(reparentElement('always', selectedElement, newParent))

  const newPath = EP.appendToPath(newParent, EP.toUid(selectedElement))
  result.push(addToReparentedToPaths('mid-interaction', [newPath]))
  result.push(...commandsToAddImports)
  return result
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
