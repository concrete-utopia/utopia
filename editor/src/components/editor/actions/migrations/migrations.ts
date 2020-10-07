import {
  PersistentModel,
  EditorTab,
  modifyParseSuccessWithSimple,
  modifyOpenJSXElements,
} from '../../store/editor-state'
import { objectMap } from '../../../../core/shared/object-utils'
import {
  isUIJSFile,
  ProjectFile,
  isParseSuccess,
  SceneMetadata,
  UIJSFile,
  CanvasMetadataParseResult,
  isCodeFile,
} from '../../../../core/shared/project-file-types'
import { eitherToMaybe, isRight, right } from '../../../../core/shared/either'
import { convertScenesToUtopiaCanvasComponent } from '../../../../core/model/scene-utils'
import { codeFile } from '../../../../core/model/project-file-utils'
import { contentsToTree, treeToContents } from '../../../assets'
import { isJSXElement, transformAllElements } from '../../../../core/shared/element-template'
import { jsxSimpleAttributeToValue } from '../../../../core/shared/jsx-attributes'

export const CURRENT_PROJECT_VERSION = 6

export function applyMigrations(
  persistentModel: PersistentModel,
): PersistentModel & { projectVersion: typeof CURRENT_PROJECT_VERSION } {
  const version1 = migrateFromVersion0(persistentModel)
  const version2 = migrateFromVersion1(version1)
  const version3 = migrateFromVersion2(version2)
  const version4 = migrateFromVersion3(version3)
  const version5 = migrateFromVersion4(version4)
  const version6 = migrateFromVersion5(version5)
  return version6
}

function migrateFromVersion0(
  persistentModel: PersistentModel,
): PersistentModel & { projectVersion: 1 } {
  if (persistentModel.projectVersion != null && persistentModel.projectVersion !== 0) {
    return persistentModel as any
  } else {
    function updateOpenFilesEntry(openFile: string): EditorTab {
      return {
        type: 'OPEN_FILE_TAB',
        filename: openFile,
      }
    }

    const updatedOpenFiles = persistentModel.openFiles.map((openFile) =>
      updateOpenFilesEntry(openFile as any),
    )
    let updatedSelectedFile: EditorTab | null = null
    const selectedFileAsString: string = persistentModel.selectedFile as any
    if (selectedFileAsString != '') {
      updatedSelectedFile = updateOpenFilesEntry(selectedFileAsString)
    }

    return {
      ...persistentModel,
      openFiles: updatedOpenFiles,
      selectedFile: updatedSelectedFile,
      projectVersion: 1,
    }
  }
}

function migrateFromVersion1(
  persistentModel: PersistentModel,
): PersistentModel & { projectVersion: 2 } {
  if (persistentModel.projectVersion != null && persistentModel.projectVersion !== 1) {
    return persistentModel as any
  } else {
    const updatedFiles = objectMap((file: ProjectFile, fileName) => {
      if (
        isUIJSFile(file) &&
        isParseSuccess(file.fileContents) &&
        isRight(file.fileContents.value.canvasMetadata)
      ) {
        const canvasMetadataParseSuccess = file.fileContents.value.canvasMetadata.value
        // this old canvas metadata might store an array of `scenes: Array<SceneMetadata>`, whereas we expect a UtopiaJSXComponent here
        if (
          (canvasMetadataParseSuccess as any).utopiaCanvasJSXComponent == null &&
          (canvasMetadataParseSuccess as any)['scenes'] != null
        ) {
          const scenes = (canvasMetadataParseSuccess as any)['scenes'] as Array<SceneMetadata>
          const utopiaCanvasComponent = convertScenesToUtopiaCanvasComponent(scenes)
          const updatedCanvasMetadataParseSuccess: CanvasMetadataParseResult = right({
            utopiaCanvasJSXComponent: utopiaCanvasComponent,
          })
          return {
            ...file,
            fileContents: {
              ...file.fileContents,
              value: {
                ...file.fileContents.value,
                canvasMetadata: updatedCanvasMetadataParseSuccess,
              },
            },
          } as UIJSFile
        } else {
          return file
        }
      } else {
        return file
      }
    }, persistentModel.projectContents as any)
    return {
      ...persistentModel,
      projectContents: updatedFiles as any,
      projectVersion: 2,
    }
  }
}

function migrateFromVersion2(
  persistentModel: PersistentModel,
): PersistentModel & { projectVersion: 3 } {
  if (persistentModel.projectVersion != null && persistentModel.projectVersion !== 2) {
    return persistentModel as any
  } else {
    const updatedFiles = objectMap((file: ProjectFile, fileName) => {
      if (isUIJSFile(file) && isParseSuccess(file.fileContents)) {
        if (
          isRight(file.fileContents.value.canvasMetadata) &&
          // the parseSuccess contained a utopiaCanvasJSXComponent which we now merge to the array of topLevelElements
          (file.fileContents.value.canvasMetadata.value as any).utopiaCanvasJSXComponent != null
        ) {
          const utopiaCanvasJSXComponent = (file.fileContents.value.canvasMetadata.value as any)
            .utopiaCanvasJSXComponent
          const updatedTopLevelElements = [
            ...file.fileContents.value.topLevelElements,
            utopiaCanvasJSXComponent,
          ]
          return {
            ...file,
            fileContents: {
              ...file.fileContents,
              value: {
                ...file.fileContents.value,
                topLevelElements: updatedTopLevelElements,
                canvasMetadata: right({}),
                projectContainedOldSceneMetadata: true,
              },
            },
          } as UIJSFile
        } else {
          return {
            ...file,
            fileContents: {
              ...file.fileContents,
              value: {
                ...file.fileContents.value,
                projectContainedOldSceneMetadata: true,
              },
            },
          }
        }
      } else {
        return file
      }
    }, persistentModel.projectContents as any)
    return {
      ...persistentModel,
      projectContents: updatedFiles as any,
      projectVersion: 3,
    }
  }
}

const PackageJsonUrl = '/package.json'

function migrateFromVersion3(
  persistentModel: PersistentModel,
): PersistentModel & { projectVersion: 4 } {
  if (persistentModel.projectVersion != null && persistentModel.projectVersion !== 3) {
    return persistentModel as any
  } else {
    const packageJsonFile = (persistentModel.projectContents as any)[PackageJsonUrl]
    if (packageJsonFile != null && isCodeFile(packageJsonFile)) {
      const parsedPackageJson = JSON.parse(packageJsonFile.fileContents)
      const updatedPackageJson = {
        ...parsedPackageJson,
        utopia: {
          ...parsedPackageJson.utopia,
          html: `public/${parsedPackageJson.utopia.html}`,
          js: `public/${parsedPackageJson.utopia.js}`,
        },
      }
      const printedPackageJson = JSON.stringify(updatedPackageJson, null, 2)
      const updatedPackageJsonFile = codeFile(printedPackageJson, null)

      return {
        ...persistentModel,
        projectVersion: 4,
        projectContents: {
          ...persistentModel.projectContents,
          [PackageJsonUrl]: updatedPackageJsonFile as any,
        },
      }
    } else {
      console.error('Error migrating project: package.json not found, skipping')
      return { ...persistentModel, projectVersion: 4 }
    }
  }
}

function migrateFromVersion4(
  persistentModel: PersistentModel,
): PersistentModel & { projectVersion: 5 } {
  if (persistentModel.projectVersion != null && persistentModel.projectVersion !== 4) {
    return persistentModel as any
  } else {
    return {
      ...persistentModel,
      projectVersion: 5,
      projectContents: contentsToTree(persistentModel.projectContents as any),
    }
  }
}

function migrateFromVersion5(
  persistentModel: PersistentModel,
): PersistentModel & { projectVersion: 6 } {
  if (persistentModel.projectVersion != null && persistentModel.projectVersion !== 5) {
    return persistentModel as any
  } else {
    const updatedFiles = objectMap((file: ProjectFile, fileName) => {
      if (isUIJSFile(file) && isParseSuccess(file.fileContents)) {
        const updatedParseSuccess = modifyParseSuccessWithSimple((s) => {
          const components = transformAllElements(s.utopiaComponents, (element) => {
            if (isJSXElement(element) && element.props['layout'] != null) {
              const hasPinnedSystem =
                eitherToMaybe(jsxSimpleAttributeToValue(element.props['layout']))?.layoutSystem ===
                'pinSystem'
              if (hasPinnedSystem) {
                const updatedProps = { ...element.props }
                delete updatedProps['layout']
                return {
                  ...element,
                  props: updatedProps,
                }
              } else {
                return element
              }
            } else {
              return element
            }
          })
          return {
            ...s,
            utopiaComponents: components,
          }
        }, file.fileContents.value)
        return {
          ...file,
          fileContents: {
            ...file.fileContents,
            value: updatedParseSuccess,
          },
        }
      } else {
        return file
      }
    }, treeToContents(persistentModel.projectContents))
    return {
      ...persistentModel,
      projectVersion: 6,
      projectContents: contentsToTree(updatedFiles),
    }
  }
}
