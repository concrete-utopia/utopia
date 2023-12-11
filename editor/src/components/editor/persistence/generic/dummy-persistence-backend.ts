import type {
  PersistenceBackendAPI,
  ProjectLoadResult,
  ProjectModel,
  ProjectOwnership,
  ProjectWithFileChanges,
} from './persistence-types'
import { projectWithFileChanges } from './persistence-types'

// Keep this file as simple as possible so that it can be used in https://stately.ai/viz
let projectCounter = 0

function getNewProjectId(): Promise<string> {
  return Promise.resolve(`Project_${projectCounter++}`)
}

function checkProjectOwned(_projectId: string): Promise<ProjectOwnership> {
  return Promise.resolve({ isOwner: true, ownerId: 'the-owner' })
}

function loadProject<ModelType>(projectId: string): Promise<ProjectLoadResult<ModelType>> {
  return Promise.resolve({
    type: 'PROJECT_NOT_FOUND',
  })
}

function saveProjectToServer<ModelType, FileType>(
  _projectId: string,
  projectModel: ProjectModel<ModelType>,
): Promise<ProjectWithFileChanges<ModelType, FileType>> {
  return Promise.resolve(projectWithFileChanges([], projectModel))
}

function saveProjectLocally<ModelType, FileType>(
  _projectId: string,
  projectModel: ProjectModel<ModelType>,
): Promise<ProjectWithFileChanges<ModelType, FileType>> {
  return Promise.resolve(projectWithFileChanges([], projectModel))
}

function downloadAssets<ModelType, FileType>(
  _projectId: string,
  projectModel: ProjectModel<ModelType>,
): Promise<ProjectWithFileChanges<ModelType, FileType>> {
  return Promise.resolve(projectWithFileChanges([], projectModel))
}

export function createDummyPersistenceBackend<ModelType, FileType>(): PersistenceBackendAPI<
  ModelType,
  FileType
> {
  return {
    getNewProjectId: getNewProjectId,
    checkProjectOwned: checkProjectOwned,
    loadProject: loadProject,
    saveProjectToServer: saveProjectToServer,
    saveProjectLocally: saveProjectLocally,
    downloadAssets: downloadAssets,
  }
}

export const VisualiserBackend: PersistenceBackendAPI<string, never> =
  createDummyPersistenceBackend()
