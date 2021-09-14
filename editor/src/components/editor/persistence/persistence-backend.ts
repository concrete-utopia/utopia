import localforage from 'localforage'
import { actions } from 'xstate'
import {
  deleteLocalProject,
  fetchLocalProject as loadLocalProject,
  localProjectKey,
} from '../../../common/persistence'
import { checkProjectOwnership } from '../../../common/server'
import { assetFile, imageFile, isImageFile } from '../../../core/model/project-file-utils'
import { getFileExtension } from '../../../core/shared/file-utils'
import { AssetFile, ImageFile } from '../../../core/shared/project-file-types'
import { arrayContains } from '../../../core/shared/utils'
import { addFileToProjectContents, getAllProjectAssetFiles } from '../../assets'
import { LocalProject } from '../persistence'
import { getPNGBufferOfElementWithID } from '../screenshot-utils'
import {
  assetToSave,
  AssetToSave,
  createNewProjectID,
  downloadAssetsFromProject,
  loadProject as loadServerProject,
  saveAssets,
  saveThumbnail,
  updateSavedProject,
} from '../server'
import {
  FileWithFileName,
  fileWithFileName,
  PersistenceBackendAPI,
  ProjectLoadResult,
  ProjectModel,
  ProjectWithFileChanges,
  projectWithFileChanges,
} from './persistence-types'
const { choose } = actions

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
  const buffer = await generateThumbnail(force)
  if (buffer != null) {
    await saveThumbnail(buffer, projectId)
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

async function checkProjectOwned(projectId: string): Promise<boolean> {
  const existsLocally = await projectIsStoredLocally(projectId)
  if (existsLocally) {
    return true
  } else {
    const ownerState = await checkProjectOwnership(projectId)
    return ownerState === 'unowned' || ownerState.isOwner
  }
}

async function loadProject(projectId: string): Promise<ProjectLoadResult> {
  // Attempt to load a local version first in case changes were made whilst the user was offline
  const localProject = (await loadLocalProject(projectId)) as LocalProject | null

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
        throw new Error(`Invalid project load response: ${serverProject}`)
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
  projectModel: ProjectModel,
): Promise<ProjectWithFileChanges> {
  const { assetsToUpload, projectWithChanges } = prepareAssetsForUploading(projectModel)

  await updateSavedProject(
    projectId,
    projectWithChanges.projectModel.content,
    projectWithChanges.projectModel.name,
  )
  if (assetsToUpload.length > 0) {
    await saveAssets(projectId, assetsToUpload)
  }

  updateRemoteThumbnail(projectId, false)
  deleteLocalProject(projectId)

  return projectWithChanges
}

async function saveProjectLocally(
  projectId: string,
  projectModel: ProjectModel,
): Promise<ProjectWithFileChanges> {
  const existing = await localforage.getItem<LocalProject | null>(localProjectKey(projectId))
  const existingThumbnail = existing == null ? '' : existing.thumbnail
  const now = new Date().toISOString()
  const createdAt = existing == null ? now : existing.createdAt
  const modifiedAt = now

  const localProject: LocalProject = {
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
  projectModel: ProjectModel,
): Promise<ProjectWithFileChanges> {
  const allProjectAssets = getAllProjectAssetFiles(projectModel.content.projectContents)
  const allProjectAssetsDownloaded = await downloadAssetsFromProject(projectId, allProjectAssets)
  const updatedProjectContents = allProjectAssetsDownloaded.reduce(
    (workingProjectContents, { fileName: assetPath, file: asset }) => {
      return addFileToProjectContents(workingProjectContents, assetPath, asset)
    },
    projectModel.content.projectContents,
  )
  const updatedProjectModel: ProjectModel = {
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
    return imageFile(undefined, undefined, file.width, file.height, file.hash)
  } else {
    return assetFile(undefined)
  }
}

interface PreparedProject {
  assetsToUpload: Array<AssetToSave>
  projectWithChanges: ProjectWithFileChanges
}

function prepareAssetsForUploading(projectModel: ProjectModel): PreparedProject {
  const allProjectAssets = getAllProjectAssetFiles(projectModel.content.projectContents)
  let assetsToUpload: Array<AssetToSave> = []
  let updatedAssets: Array<FileWithFileName> = []

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

  const updatedProjectModel: ProjectModel = {
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

export const PersistenceBackend: PersistenceBackendAPI = {
  getNewProjectId: getNewProjectId,
  checkProjectOwned: checkProjectOwned,
  loadProject: loadProject,
  saveProjectToServer: saveProjectToServer,
  saveProjectLocally: saveProjectLocally,
  downloadAssets: downloadAssets,
}
