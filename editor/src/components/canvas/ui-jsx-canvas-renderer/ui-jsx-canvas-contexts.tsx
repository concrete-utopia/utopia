import * as React from 'react'
import type { MapLike } from 'typescript'
import { createContext } from 'use-context-selector'
import { atomWithPubSub } from '../../../core/shared/atom-with-pub-sub'
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

interface RerenderUtopiaContextProps {
  validPaths: Array<ElementPath>
  hiddenInstances: Array<ElementPath>
  canvasIsLive: boolean
  shouldIncludeCanvasRootInTheSpy: boolean
}

export const RerenderUtopiaContextAtom = atomWithPubSub<RerenderUtopiaContextProps>({
  key: 'RerenderUtopiaContextAtom',
  defaultValue: {
    validPaths: [],
    hiddenInstances: [],
    canvasIsLive: false,
    shouldIncludeCanvasRootInTheSpy: false,
  },
})

interface ContextForScenesData {
  canvasIsLive: boolean
}

export const ContextForScenes = createContext<ContextForScenesData>({
  canvasIsLive: false,
})
ContextForScenes.displayName = 'ContextForScenes'

export interface UtopiaProjectContextData {
  projectContents: ProjectContentTreeRoot
  openStoryboardFilePathKILLME: string | null
  transientFilesState: TransientFilesState | null
  resolve: (importOrigin: string, toImport: string) => Either<string, string>
}
const EmptyResolve = (importOrigin: string, toImport: string): Either<string, string> => {
  return left(`Error while resolving ${toImport}, the resolver is missing`)
}

export const UtopiaProjectContextAtom = atomWithPubSub<UtopiaProjectContextData>({
  key: 'UtopiaProjectContext',
  defaultValue: {
    projectContents: {},
    openStoryboardFilePathKILLME: null,
    transientFilesState: null,
    resolve: EmptyResolve,
  },
})

interface ParentLevelUtopiaContextProps {
  elementPath: ElementPath | null
}

export const ParentLevelUtopiaContext = createContext<ParentLevelUtopiaContextProps>({
  elementPath: null,
})
