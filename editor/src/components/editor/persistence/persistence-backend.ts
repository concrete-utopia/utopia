import localforage from 'localforage'
import {
  deleteLocalProject,
  fetchLocalProject as loadLocalProject,
  localProjectKey,
} from '../../../common/persistence'
import { checkProjectOwnership } from '../../../common/server'
import { getFileExtension } from '../../../core/shared/file-utils'
import type { AssetFile, ImageFile, ProjectFile } from '../../../core/shared/project-file-types'
import { assetFile, imageFile, isImageFile } from '../../../core/shared/project-file-types'
import { arrayContains } from '../../../core/shared/utils'
import { addFileToProjectContents, getAllProjectAssetFiles } from '../../assets'
import type { AssetToSave } from '../server'
import {
  assetToSave,
  createNewProject,
  createNewProjectID,
  downloadAssetsFromProject,
  loadProject as loadServerProject,
  saveAssets,
  updateSavedProject,
} from '../server'
import type {
  FileWithFileName,
  PersistenceBackendAPI,
  ProjectLoadResult,
  ProjectModel,
  ProjectWithFileChanges,
  LocalProject,
  ProjectOwnership,
  ProjectCreationResult,
} from './generic/persistence-types'
import { fileWithFileName, projectWithFileChanges } from './generic/persistence-types'
import type { PersistentModel } from '../store/editor-state'
import { IS_TEST_ENVIRONMENT } from '../../../common/env-vars'
import { readParamFromUrl } from '../url-utils'

export async function projectIsStoredLocally(projectId: string): Promise<boolean> {
  const keys = await localforage.keys().catch(() => [])
  const targetKey = localProjectKey(projectId)
  return arrayContains(keys, targetKey)
}

async function getNewProjectId(): Promise<string> {
  return createNewProjectID()
}

export async function checkProjectOwned(
  loggedIn: boolean,
  projectId: string,
): Promise<ProjectOwnership> {
  const existsLocally = await projectIsStoredLocally(projectId)
  if (existsLocally) {
    return {
      ownerId: null,
      isOwner: true,
    }
  } else if (loggedIn) {
    const ownerState = await checkProjectOwnership(projectId)
    return ownerState === 'unowned'
      ? { ownerId: null, isOwner: true }
      : { ownerId: ownerState.ownerId, isOwner: ownerState.isOwner }
  } else if (IS_TEST_ENVIRONMENT) {
    return {
      ownerId: null,
      isOwner: true,
    }
  } else {
    return {
      ownerId: null,
      isOwner: false,
    }
  }
}

async function loadProject(projectId: string): Promise<ProjectLoadResult<PersistentModel>> {
  // Attempt to load a local version first in case changes were made whilst the user was offline
  const localProject = (await loadLocalProject(projectId)) as LocalProject<PersistentModel> | null

  if (localProject == null) {
    const serverProject = await loadServerProject(projectId)

    switch (serverProject.type) {
      case 'ProjectLoaded':
        return {
          type: 'PROJECT_LOAD_SUCCESS',
          projectId: projectId,
          projectModel: {
            content: serverProject.content,
            name: serverProject.title,
          },
        }
      case 'ProjectNotFound':
        return {
          type: 'PROJECT_NOT_FOUND',
          projectId: projectId,
        }

      case 'ProjectNotAuthorized':
        return {
          type: 'PROJECT_NOT_AUTHORIZED',
          projectId: projectId,
        }

      default:
        throw new Error(`Invalid project load response: ${JSON.stringify(serverProject)}`)
    }
  } else {
    return {
      type: 'PROJECT_LOAD_SUCCESS',
      projectId: projectId,
      projectModel: {
        content: localProject.model,
        name: localProject.name,
      },
    }
  }
}

export const AccessLevelParamKey = 'accessLevel'
export const CloneParamKey = 'clone'
export const GithubBranchParamKey = 'github_branch'

async function createNewProjectInServer(
  projectModel: ProjectModel<PersistentModel>,
): Promise<ProjectCreationResult<PersistentModel, ProjectFile>> {
  const { projectWithChanges } = prepareAssetsForUploading(projectModel)

  const accessLevel = readParamFromUrl(AccessLevelParamKey)

  const { id } = await createNewProject(
    projectWithChanges.projectModel.content,
    projectWithChanges.projectModel.name,
    accessLevel,
  )

  return { projectId: id, projectWithChanges: projectWithChanges }
}

async function saveProjectToServer(
  projectId: string,
  projectModel: ProjectModel<PersistentModel>,
): Promise<ProjectWithFileChanges<PersistentModel, ProjectFile>> {
  const { assetsToUpload, projectWithChanges } = prepareAssetsForUploading(projectModel)

  await updateSavedProject(
    projectId,
    projectWithChanges.projectModel.content,
    projectWithChanges.projectModel.name,
  )
  if (assetsToUpload.length > 0) {
    await saveAssets(projectId, assetsToUpload)
  }

  void deleteLocalProject(projectId)

  return projectWithChanges
}

async function saveProjectLocally(
  projectId: string,
  projectModel: ProjectModel<PersistentModel>,
): Promise<ProjectWithFileChanges<PersistentModel, ProjectFile>> {
  const existing = await localforage.getItem<LocalProject<PersistentModel> | null>(
    localProjectKey(projectId),
  )
  const existingThumbnail = existing == null ? '' : existing.thumbnail
  const now = new Date().toISOString()
  const createdAt = existing == null ? now : existing.createdAt
  const modifiedAt = now

  const localProject: LocalProject<PersistentModel> = {
    model: projectModel.content,
    createdAt: createdAt,
    lastModified: modifiedAt,
    thumbnail: existingThumbnail,
    name: projectModel.name,
  }

  await localforage.setItem(localProjectKey(projectId), localProject)

  return projectWithFileChanges([], projectModel)
}

async function downloadAssets(
  projectId: string,
  projectModel: ProjectModel<PersistentModel>,
): Promise<ProjectWithFileChanges<PersistentModel, ProjectFile>> {
  const allProjectAssets = getAllProjectAssetFiles(projectModel.content.projectContents)
  const allProjectAssetsDownloaded = await downloadAssetsFromProject(projectId, allProjectAssets)
  const updatedProjectContents = allProjectAssetsDownloaded.reduce(
    (workingProjectContents, { fileName: assetPath, file: asset }) => {
      return addFileToProjectContents(workingProjectContents, assetPath, asset)
    },
    projectModel.content.projectContents,
  )
  const updatedProjectModel: ProjectModel<PersistentModel> = {
    name: projectModel.name,
    content: {
      ...projectModel.content,
      projectContents: updatedProjectContents,
    },
  }

  return projectWithFileChanges(allProjectAssetsDownloaded, updatedProjectModel)
}

function scrubBase64FromFile(file: ImageFile | AssetFile): ImageFile | AssetFile {
  if (isImageFile(file)) {
    return imageFile(undefined, undefined, file.width, file.height, file.hash, file.gitBlobSha)
  } else {
    return assetFile(undefined, file.gitBlobSha)
  }
}

interface PreparedProject {
  assetsToUpload: Array<AssetToSave>
  projectWithChanges: ProjectWithFileChanges<PersistentModel, ProjectFile>
}

function prepareAssetsForUploading(projectModel: ProjectModel<PersistentModel>): PreparedProject {
  const allProjectAssets = getAllProjectAssetFiles(projectModel.content.projectContents)
  let assetsToUpload: Array<AssetToSave> = []
  let updatedAssets: Array<FileWithFileName<ProjectFile>> = []

  const updatedProjectContents = allProjectAssets.reduce(
    (workingProjectContents, { fileName: assetPath, file: asset }) => {
      const fileType = getFileExtension(assetPath)
      if (asset.base64 != null) {
        const updatedFile = scrubBase64FromFile(asset)
        assetsToUpload.push(assetToSave(fileType, asset.base64, assetPath))
        updatedAssets.push(fileWithFileName(assetPath, updatedFile))
        return addFileToProjectContents(workingProjectContents, assetPath, updatedFile)
      } else {
        return workingProjectContents
      }
    },
    projectModel.content.projectContents,
  )

  const updatedProjectModel: ProjectModel<PersistentModel> = {
    name: projectModel.name,
    content: {
      ...projectModel.content,
      projectContents: updatedProjectContents,
    },
  }

  return {
    assetsToUpload: assetsToUpload,
    projectWithChanges: projectWithFileChanges(updatedAssets, updatedProjectModel),
  }
}

export const PersistenceBackend: PersistenceBackendAPI<PersistentModel, ProjectFile> = {
  getNewProjectId: getNewProjectId,
  checkProjectOwned: checkProjectOwned,
  loadProject: loadProject,
  saveProjectToServer: saveProjectToServer,
  createNewProjectInServer: createNewProjectInServer,
  saveProjectLocally: saveProjectLocally,
  downloadAssets: downloadAssets,
}
