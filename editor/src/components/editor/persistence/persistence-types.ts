import { actions } from 'xstate'
import { ProjectFile } from '../../../core/shared/project-file-types'
import { PersistentModel } from '../store/editor-state'
const { choose } = actions

export interface LocalProject {
  model: PersistentModel
  createdAt: string
  lastModified: string
  thumbnail: string
  name: string
}

export interface ProjectModel {
  name: string
  content: PersistentModel
}

export interface ProjectModelWithId {
  projectId: string
  projectModel: ProjectModel
}

export interface ProjectLoadSuccess extends ProjectModelWithId {
  type: 'PROJECT_LOAD_SUCCESS'
}

export interface ProjectNotFount {
  type: 'PROJECT_NOT_FOUND'
}

export type ProjectLoadResult = ProjectLoadSuccess | ProjectNotFount

export interface FileWithFileName {
  fileName: string
  file: ProjectFile
}

export function fileWithFileName(fileName: string, file: ProjectFile): FileWithFileName {
  return {
    fileName: fileName,
    file: file,
  }
}

export interface ProjectWithFileChanges {
  filesWithFileNames: Array<FileWithFileName>
  projectModel: ProjectModel
}

export function projectWithFileChanges(
  filesWithFileNames: Array<FileWithFileName>,
  projectModel: ProjectModel,
): ProjectWithFileChanges {
  return {
    filesWithFileNames: filesWithFileNames,
    projectModel: projectModel,
  }
}

export interface PersistenceBackendAPI {
  getNewProjectId: () => Promise<string>
  checkProjectOwned: (projectId: string) => Promise<boolean>
  loadProject: (projectId: string) => Promise<ProjectLoadResult>
  saveProjectToServer: (
    projectId: string,
    projectModel: ProjectModel,
  ) => Promise<ProjectWithFileChanges>
  saveProjectLocally: (
    projectId: string,
    projectModel: ProjectModel,
  ) => Promise<ProjectWithFileChanges>
  downloadAssets: (projectId: string, projectModel: ProjectModel) => Promise<ProjectWithFileChanges>
}
