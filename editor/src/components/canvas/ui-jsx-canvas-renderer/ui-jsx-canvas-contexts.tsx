import * as React from 'react'
import type { MapLike } from 'typescript'
import { createContext } from 'use-context-selector'
import { Either, left } from '../../../core/shared/either'
import type { ElementPath } from '../../../core/shared/project-file-types'
import { ProjectContentTreeRoot } from '../../assets'
import type { TransientFilesState, UIFileBase64Blobs } from '../../editor/store/editor-state'

export interface MutableUtopiaContextProps {
  [filePath: string]: {
    mutableContext: {
      requireResult: MapLike<any>
      fileBlobs: UIFileBase64Blobs
      rootScope: MapLike<any>
      jsxFactoryFunctionName: string | null
    }
  }
}

export const MutableUtopiaContext = React.createContext<{ current: MutableUtopiaContextProps }>({
  current: {},
})
MutableUtopiaContext.displayName = 'MutableUtopiaContext'

export function updateMutableUtopiaContextWithNewProps(
  ref: React.MutableRefObject<MutableUtopiaContextProps>,
  newProps: MutableUtopiaContextProps,
): void {
  ref.current = newProps
}

export interface RerenderUtopiaContextProps {
  hiddenInstances: Array<ElementPath>
  canvasIsLive: boolean
  shouldIncludeCanvasRootInTheSpy: boolean
}

export const RerenderUtopiaContext = createContext<RerenderUtopiaContextProps>({
  hiddenInstances: [],
  canvasIsLive: false,
  shouldIncludeCanvasRootInTheSpy: false,
})
RerenderUtopiaContext.displayName = 'RerenderUtopiaContext'

export interface UtopiaProjectContextProps {
  projectContents: ProjectContentTreeRoot
  openStoryboardFilePathKILLME: string | null
  transientFilesState: TransientFilesState | null
  resolve: (importOrigin: string, toImport: string) => Either<string, string>
}
const EmptyResolve = (importOrigin: string, toImport: string): Either<string, string> => {
  return left(`Error while resolving ${toImport}, the resolver is missing`)
}

export const UtopiaProjectContext = createContext<UtopiaProjectContextProps>({
  projectContents: {},
  openStoryboardFilePathKILLME: null,
  transientFilesState: null,
  resolve: EmptyResolve,
})

interface SceneLevelContextProps {
  validPaths: Array<ElementPath>
}

export const SceneLevelUtopiaContext = React.createContext<SceneLevelContextProps>({
  validPaths: [],
})
SceneLevelUtopiaContext.displayName = 'SceneLevelUtopiaContext'

interface ParentLevelUtopiaContextProps {
  elementPath: ElementPath | null
}

export const ParentLevelUtopiaContext = createContext<ParentLevelUtopiaContextProps>({
  elementPath: null,
})
