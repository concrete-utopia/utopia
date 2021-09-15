import { v4 as UUID } from 'uuid'
import {
  PersistenceBackendAPI,
  ProjectLoadResult,
  ProjectModel,
  ProjectWithFileChanges,
  projectWithFileChanges,
} from './persistence-types'

// Keep this file as simple as possible so that it can be used in https://stately.ai/viz

function getNewProjectId(): Promise<string> {
  return Promise.resolve(UUID())
}

function checkProjectOwned(projectId: string): Promise<boolean> {
  return Promise.resolve(true)
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

export const VisualiserBackend: PersistenceBackendAPI<
  string,
  never
> = createDummyPersistenceBackend()
