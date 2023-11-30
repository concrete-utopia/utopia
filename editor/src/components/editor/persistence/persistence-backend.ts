import localforage from 'localforage'
import { actions } from 'xstate'
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
import { getPNGBufferOfElementWithID } from '../screenshot-utils'
import type { AssetToSave } from '../server'
import {
  assetToSave,
  createNewProjectID,
  downloadAssetsFromProject,
  loadProject as loadServerProject,
  saveAssets,
  saveThumbnail,
  updateSavedProject,
} from '../server'
import type {
  FileWithFileName,
  PersistenceBackendAPI,
  ProjectLoadResult,
  ProjectModel,
  ProjectWithFileChanges,
  LocalProject,
} from './generic/persistence-types'
import { fileWithFileName, projectWithFileChanges } from './generic/persistence-types'
import type { PersistentModel } from '../store/editor-state'
import { isFeatureEnabled } from '../../../utils/feature-switches'

let _lastThumbnailGenerated: number = 0
const THUMBNAIL_THROTTLE = 300000

async function generateThumbnail(force: boolean): Promise<Buffer | null> {
  const now = Date.now()
  if (now - _lastThumbnailGenerated > THUMBNAIL_THROTTLE || force) {
    _lastThumbnailGenerated = now
    return getPNGBufferOfElementWithID('canvas-root', { width: 1152, height: 720 })
  } else {
    return Promise.resolve(null)
  }
}

export async function updateRemoteThumbnail(projectId: string, force: boolean): Promise<void> {
  if (isFeatureEnabled('Project Thumbnail Generation')) {
    const buffer = await generateThumbnail(force)
    if (buffer != null) {
      await saveThumbnail(buffer, projectId)
    }
  }
}

export async function projectIsStoredLocally(projectId: string): Promise<boolean> {
  const keys = await localforage.keys().catch(() => [])
  const targetKey = localProjectKey(projectId)
  return arrayContains(keys, targetKey)
}

async function getNewProjectId(): Promise<string> {
  return createNewProjectID()
}

export async function checkProjectOwned(
  projectId: string,
): Promise<{ ownerId: string | null; owned: boolean }> {
  const existsLocally = await projectIsStoredLocally(projectId)
  if (existsLocally) {
    return { ownerId: null, owned: true }
  } else {
    const ownerState = await checkProjectOwnership(projectId)
    if (ownerState === 'unowned') {
      return { ownerId: null, owned: false }
    } else {
      return { ownerId: ownerState.ownerId, owned: ownerState.isOwner }
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

  void updateRemoteThumbnail(projectId, false)
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
  saveProjectLocally: saveProjectLocally,
  downloadAssets: downloadAssets,
}
