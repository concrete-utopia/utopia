// Keep this file as simple as possible so that it can be used in https://stately.ai/viz

export interface LocalProject<ModelType> {
  model: ModelType
  createdAt: string
  lastModified: string
  thumbnail: string
  name: string
}

export interface ProjectModel<ModelType> {
  name: string
  content: ModelType
}

export interface ProjectModelWithId<ModelType> {
  projectId: string
  projectModel: ProjectModel<ModelType>
}

export interface PersistenceContext<ModelType> {
  projectId?: string
  project?: ProjectModel<ModelType>
  rollbackProjectId?: string
  rollbackProject?: ProjectModel<ModelType>
  queuedSave?: ProjectModel<ModelType>
  projectOwnership: ProjectOwnership
  loggedIn: boolean
}

export interface ProjectLoadSuccess<ModelType> extends ProjectModelWithId<ModelType> {
  type: 'PROJECT_LOAD_SUCCESS'
}

export interface ProjectNotFound {
  type: 'PROJECT_NOT_FOUND'
  projectId: string
}

export interface ProjectNotAuthorized {
  type: 'PROJECT_NOT_AUTHORIZED'
  projectId: string
}

export type ProjectLoadResult<ModelType> =
  | ProjectLoadSuccess<ModelType>
  | ProjectNotFound
  | ProjectNotAuthorized

export interface FileWithFileName<FileType> {
  fileName: string
  file: FileType
}

export function fileWithFileName<FileType>(
  fileName: string,
  file: FileType,
): FileWithFileName<FileType> {
  return {
    fileName: fileName,
    file: file,
  }
}

export interface ProjectCreationResult<ModelType, FileType> {
  projectId: string
  projectWithChanges: ProjectWithFileChanges<ModelType, FileType>
}

export function projectCreationResult<ModelType, FileType>(
  projectId: string,
  project: ProjectWithFileChanges<ModelType, FileType>,
): ProjectCreationResult<ModelType, FileType> {
  return {
    projectId: projectId,
    projectWithChanges: project,
  }
}

export interface ProjectWithFileChanges<ModelType, FileType> {
  filesWithFileNames: Array<FileWithFileName<FileType>>
  projectModel: ProjectModel<ModelType>
}

export function projectWithFileChanges<ModelType, FileType>(
  filesWithFileNames: Array<FileWithFileName<FileType>>,
  projectModel: ProjectModel<ModelType>,
): ProjectWithFileChanges<ModelType, FileType> {
  return {
    filesWithFileNames: filesWithFileNames,
    projectModel: projectModel,
  }
}

export type ProjectOwnership = {
  ownerId: string | null
  isOwner: boolean
}

export interface PersistenceBackendAPI<ModelType, FileType> {
  getNewProjectId: () => Promise<string>
  checkProjectOwned: (loggedIn: boolean, projectId: string) => Promise<ProjectOwnership>
  loadProject: (projectId: string) => Promise<ProjectLoadResult<ModelType>>
  saveProjectToServer: (
    projectId: string,
    projectModel: ProjectModel<ModelType>,
  ) => Promise<ProjectWithFileChanges<ModelType, FileType>>
  createNewProjectInServer: (
    projectModel: ProjectModel<ModelType>,
  ) => Promise<ProjectCreationResult<ModelType, FileType>>
  saveProjectLocally: (
    projectId: string,
    projectModel: ProjectModel<ModelType>,
  ) => Promise<ProjectWithFileChanges<ModelType, FileType>>
  downloadAssets: (
    projectId: string,
    projectModel: ProjectModel<ModelType>,
  ) => Promise<ProjectWithFileChanges<ModelType, FileType>>
}
