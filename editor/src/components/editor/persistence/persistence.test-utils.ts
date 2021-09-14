import { generateUID } from '../../../core/shared/uid-utils'
import { NO_OP } from '../../../core/shared/utils'
import { PersistenceMachine } from './persistence-machine'
import {
  PersistenceBackendAPI,
  ProjectLoadResult,
  ProjectModel,
  ProjectWithFileChanges,
  projectWithFileChanges,
} from './persistence-types'

let allProjectIds: Array<string> = []
function getNewProjectId(): Promise<string> {
  const newId = generateUID(allProjectIds)
  allProjectIds.push(newId)
  return Promise.resolve(newId)
}

function checkProjectOwned(projectId: string): Promise<boolean> {
  return Promise.resolve(true)
}

function loadProject(projectId: string): Promise<ProjectLoadResult> {
  return Promise.resolve({
    type: 'PROJECT_NOT_FOUND',
  })
}

function saveProjectToServer(
  projectId: string,
  projectModel: ProjectModel,
): Promise<ProjectWithFileChanges> {
  return Promise.resolve(projectWithFileChanges([], projectModel))
}

function saveProjectLocally(
  projectId: string,
  projectModel: ProjectModel,
): Promise<ProjectWithFileChanges> {
  return Promise.resolve(projectWithFileChanges([], projectModel))
}

function downloadAssets(
  projectId: string,
  projectModel: ProjectModel,
): Promise<ProjectWithFileChanges> {
  return Promise.resolve(projectWithFileChanges([], projectModel))
}

export const DummyPersistenceBackend: PersistenceBackendAPI = {
  getNewProjectId: getNewProjectId,
  checkProjectOwned: checkProjectOwned,
  loadProject: loadProject,
  saveProjectToServer: saveProjectToServer,
  saveProjectLocally: saveProjectLocally,
  downloadAssets: downloadAssets,
}

export const DummyPersistenceMachine: PersistenceMachine = new PersistenceMachine(
  DummyPersistenceBackend,
  NO_OP,
  NO_OP,
  NO_OP,
)
