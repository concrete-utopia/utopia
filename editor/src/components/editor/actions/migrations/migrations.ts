import type { PersistentModel } from '../../store/editor-state'
import { StoryboardFilePath } from '../../store/editor-state'
import { objectMap } from '../../../../core/shared/object-utils'
import type {
  ProjectFile,
  SceneMetadata,
  TextFile,
} from '../../../../core/shared/project-file-types'
import {
  isParseSuccess,
  isTextFile,
  textFile,
  textFileContents,
  unparsed,
  RevisionsState,
} from '../../../../core/shared/project-file-types'
import { isRight, right } from '../../../../core/shared/either'
import {
  BakedInStoryboardVariableName,
  convertScenesToUtopiaCanvasComponent,
} from '../../../../core/model/scene-utils'
import type { ProjectContentFile, ProjectContentsTree } from '../../../assets'
import {
  addFileToProjectContents,
  contentsToTree,
  getProjectFileByFilePath,
  projectContentFile,
  removeFromProjectContents,
  transformContentsTree,
  walkContentsTree,
} from '../../../assets'
import { isUtopiaJSXComponent } from '../../../../core/shared/element-template'

export const CURRENT_PROJECT_VERSION = 15

export function applyMigrations(
  persistentModel: PersistentModel,
): PersistentModel & { projectVersion: typeof CURRENT_PROJECT_VERSION } {
  const version1 = migrateFromVersion0(persistentModel)
  const version2 = migrateFromVersion1(version1)
  const version3 = migrateFromVersion2(version2)
  const version4 = migrateFromVersion3(version3)
  const version5 = migrateFromVersion4(version4)
  const version6 = migrateFromVersion5(version5)
  const version7 = migrateFromVersion6(version6)
  const version8 = migrateFromVersion7(version7)
  const version9 = migrateFromVersion8(version8)
  const version10 = migrateFromVersion9(version9)
  const version11 = migrateFromVersion10(version10)
  const version12 = migrateFromVersion11(version11)
  const version13 = migrateFromVersion12(version12)
  const version14 = migrateFromVersion13(version13)
  const version15 = migrateFromVersion14(version14)
  return version15
}

function migrateFromVersion0(
  persistentModel: PersistentModel,
): PersistentModel & { projectVersion: 1 } & { openFiles: any; selectedFile: any } {
  if (persistentModel.projectVersion != null && persistentModel.projectVersion !== 0) {
    return persistentModel as any
  } else {
    function updateOpenFilesEntry(openFile: string): any {
      return {
        type: 'OPEN_FILE_TAB',
        filename: openFile,
      }
    }

    const updatedOpenFiles = (persistentModel as any).openFiles.map((openFile: any) =>
      updateOpenFilesEntry(openFile),
    )
    let updatedSelectedFile: any | null = null
    const selectedFileAsString: string = (persistentModel as any).selectedFile as any
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
        isTextFile(file) &&
        isParseSuccess(file.fileContents as any) &&
        isRight((file.fileContents as any).value.canvasMetadata)
      ) {
        const canvasMetadataParseSuccess = (file.fileContents as any).value.canvasMetadata.value
        // this old canvas metadata might store an array of `scenes: Array<SceneMetadata>`, whereas we expect a UtopiaJSXComponent here
        if (
          (canvasMetadataParseSuccess as any).utopiaCanvasJSXComponent == null &&
          (canvasMetadataParseSuccess as any)['scenes'] != null
        ) {
          const scenes = (canvasMetadataParseSuccess as any)['scenes'] as Array<SceneMetadata>
          const utopiaCanvasComponent = convertScenesToUtopiaCanvasComponent(scenes)
          const updatedCanvasMetadataParseSuccess: any = right({
            utopiaCanvasJSXComponent: utopiaCanvasComponent,
          })
          return {
            ...file,
            fileContents: {
              ...file.fileContents,
              value: {
                ...(file.fileContents as any).value,
                canvasMetadata: updatedCanvasMetadataParseSuccess,
              },
            },
          } as TextFile
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
      if (isTextFile(file) && isParseSuccess(file.fileContents as any)) {
        if (
          isRight((file.fileContents as any).value.canvasMetadata) &&
          // the parseSuccess contained a utopiaCanvasJSXComponent which we now merge to the array of topLevelElements
          ((file.fileContents as any).value.canvasMetadata.value as any).utopiaCanvasJSXComponent !=
            null
        ) {
          const utopiaCanvasJSXComponent = (
            (file.fileContents as any).value.canvasMetadata.value as any
          ).utopiaCanvasJSXComponent
          const updatedTopLevelElements = [
            ...(file.fileContents as any).value.topLevelElements,
            utopiaCanvasJSXComponent,
          ]
          return {
            ...file,
            fileContents: {
              ...file.fileContents,
              value: {
                ...(file.fileContents as any).value,
                topLevelElements: updatedTopLevelElements,
                canvasMetadata: right({}),
                projectContainedOldSceneMetadata: true,
              },
            },
          } as TextFile
        } else {
          return {
            ...file,
            fileContents: {
              ...file.fileContents,
              value: {
                ...(file.fileContents as any).value,
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
    if (packageJsonFile != null && isTextFile(packageJsonFile)) {
      const parsedPackageJson = JSON.parse(packageJsonFile.fileContents as any)
      const updatedPackageJson = {
        ...parsedPackageJson,
        utopia: {
          ...parsedPackageJson.utopia,
          html: `public/${parsedPackageJson.utopia.html}`,
          js: `public/${parsedPackageJson.utopia.js}`,
        },
      }
      const printedPackageJson = JSON.stringify(updatedPackageJson, null, 2)
      const updatedPackageJsonFile = {
        type: 'CODE_FILE',
        fileContents: printedPackageJson,
        lastSavedContents: null,
      }

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
    return {
      ...persistentModel,
      projectVersion: 6,
      projectContents: transformContentsTree(
        persistentModel.projectContents,
        (tree: ProjectContentsTree) => {
          if (tree.type === 'PROJECT_CONTENT_FILE') {
            const file: ProjectContentFile['content'] = tree.content
            const fileType = file.type as string
            if (fileType === 'CODE_FILE') {
              const newFile = textFile(
                textFileContents((file as any).fileContents, unparsed, RevisionsState.CodeAhead),
                null,
                null,
                0,
              )
              return projectContentFile(tree.fullPath, newFile)
            } else if (fileType === 'UI_JS_FILE') {
              const code = (file as any).fileContents.value.code
              const newFile = textFile(
                textFileContents(code, unparsed, RevisionsState.CodeAhead),
                null,
                null,
                0,
              )
              return projectContentFile(tree.fullPath, newFile)
            } else {
              return tree
            }
          } else {
            return tree
          }
        },
      ),
    }
  }
}

function migrateFromVersion6(
  persistentModel: PersistentModel,
): PersistentModel & { projectVersion: 7 } {
  if (persistentModel.projectVersion != null && persistentModel.projectVersion !== 6) {
    return persistentModel as any
  } else {
    let storyboardTarget: string | null = null
    walkContentsTree(persistentModel.projectContents, (fullPath: string, file: ProjectFile) => {
      // Don't bother looking further if there's already something to work with.
      if (storyboardTarget == null) {
        if (isTextFile(file) && isParseSuccess(file.fileContents.parsed)) {
          for (const topLevelElement of file.fileContents.parsed.topLevelElements) {
            if (
              isUtopiaJSXComponent(topLevelElement) &&
              topLevelElement.name === BakedInStoryboardVariableName
            ) {
              storyboardTarget = fullPath
            }
          }
        }
      }
    })

    let updatedProjectContents = persistentModel.projectContents
    if (storyboardTarget != null) {
      const file = getProjectFileByFilePath(updatedProjectContents, storyboardTarget)
      if (file == null) {
        throw new Error(`Internal error in migration: Unable to find file ${storyboardTarget}.`)
      } else {
        // Move the file around.
        updatedProjectContents = removeFromProjectContents(updatedProjectContents, storyboardTarget)
        updatedProjectContents = addFileToProjectContents(
          updatedProjectContents,
          StoryboardFilePath,
          file,
        )
      }
    }
    return {
      ...persistentModel,
      projectVersion: 7,
      projectContents: updatedProjectContents,
    }
  }
}

function migrateFromVersion7(
  persistentModel: PersistentModel,
): PersistentModel & { projectVersion: 8 } {
  if (persistentModel.projectVersion != null && persistentModel.projectVersion !== 7) {
    return persistentModel as any
  } else {
    return {
      ...persistentModel,
      projectVersion: 8,
      githubSettings: {
        targetRepository: null,
      } as any,
    }
  }
}

function migrateFromVersion8(
  persistentModel: PersistentModel,
): PersistentModel & { projectVersion: 9 } {
  if (persistentModel.projectVersion != null && persistentModel.projectVersion !== 8) {
    return persistentModel as any
  } else {
    return {
      ...persistentModel,
      projectVersion: 9,
      githubSettings: {
        ...persistentModel.githubSettings,
        originCommit: null,
      },
    }
  }
}

function migrateFromVersion9(
  persistentModel: PersistentModel,
): PersistentModel & { projectVersion: 10 } {
  if (persistentModel.projectVersion != null && persistentModel.projectVersion !== 9) {
    return persistentModel as any
  } else {
    return {
      ...persistentModel,
      projectVersion: 10,
      githubSettings: {
        ...persistentModel.githubSettings,
        originCommit: null,
        branchName: null,
      },
      githubChecksums: null,
      branchContents: null,
    } as any
  }
}

function migrateFromVersion10(
  persistentModel: PersistentModel,
): PersistentModel & { projectVersion: 11 } {
  if (persistentModel.projectVersion != null && persistentModel.projectVersion !== 10) {
    return persistentModel as any
  } else {
    return {
      ...persistentModel,
      projectVersion: 11,
      githubSettings: {
        ...persistentModel.githubSettings,
        originCommit: null,
        branchName: null,
        branchLoaded: false,
      },
      githubChecksums: null,
      branchContents: null,
    } as any
  }
}

function migrateFromVersion11(
  persistentModel: PersistentModel,
): PersistentModel & { projectVersion: 12 } {
  if (persistentModel.projectVersion != null && persistentModel.projectVersion !== 11) {
    return persistentModel as any
  } else {
    return {
      ...persistentModel,
      projectVersion: 12,
      assetChecksums: {},
    } as any
  }
}

function migrateFromVersion12(
  persistentModel: PersistentModel,
): PersistentModel & { projectVersion: 13 } {
  if (persistentModel.projectVersion != null && persistentModel.projectVersion !== 12) {
    return persistentModel as any
  } else {
    return {
      ...persistentModel,
      projectVersion: 13,
      colorSwatches: [],
    }
  }
}

function migrateFromVersion13(
  persistentModel: PersistentModel,
): PersistentModel & { projectVersion: 14 } {
  if (persistentModel.projectVersion != null && persistentModel.projectVersion !== 13) {
    return persistentModel as any
  } else {
    return {
      ...persistentModel,
      projectVersion: 14,
      projectContents: transformContentsTree(
        persistentModel.projectContents,
        (tree: ProjectContentsTree) => {
          if (tree.type === 'PROJECT_CONTENT_FILE') {
            const file: ProjectContentFile['content'] = tree.content
            if (file.type === 'TEXT_FILE') {
              // We replaced lastRevisedTime (a timestamp) with versionNumber
              const migratedFile = textFile(
                file.fileContents,
                file.lastSavedContents,
                file.lastParseSuccess,
                0,
              )
              return projectContentFile(tree.fullPath, migratedFile)
            } else {
              return tree
            }
          } else {
            return tree
          }
        },
      ),
    }
  }
}

function migrateFromVersion14(
  persistentModel: PersistentModel,
): PersistentModel & { projectVersion: 15 } {
  if (persistentModel.projectVersion != null && persistentModel.projectVersion !== 14) {
    return persistentModel as any
  } else {
    return {
      ...persistentModel,
      projectVersion: 15,
      codeEditorErrors: {
        ...persistentModel.codeEditorErrors,
        componentDescriptorErrors: persistentModel.codeEditorErrors.componentDescriptorErrors ?? {},
      },
    }
  }
}
