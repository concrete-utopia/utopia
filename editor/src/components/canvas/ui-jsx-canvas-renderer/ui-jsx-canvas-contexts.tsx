import * as React from 'react'
import type { MapLike } from 'typescript'
import { createContext } from 'use-context-selector'
import { EmptyScenePathForStoryboard } from '../../../core/model/scene-utils'
import { Either, left } from '../../../core/shared/either'
import type { TopLevelElement, UtopiaJSXComponent } from '../../../core/shared/element-template'
import type { InstancePath, ScenePath, TemplatePath } from '../../../core/shared/project-file-types'
import { ProjectContentTreeRoot } from '../../assets'
import type { TransientFileState, UIFileBase64Blobs } from '../../editor/store/editor-state'

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
  hiddenInstances: Array<TemplatePath>
  canvasIsLive: boolean
  shouldIncludeCanvasRootInTheSpy: boolean
  focusedElementPath: ScenePath | null
}

export const RerenderUtopiaContext = createContext<RerenderUtopiaContextProps>({
  hiddenInstances: [],
  canvasIsLive: false,
  shouldIncludeCanvasRootInTheSpy: false,
  focusedElementPath: null,
})
RerenderUtopiaContext.displayName = 'RerenderUtopiaContext'

interface UtopiaProjectContextProps {
  projectContents: ProjectContentTreeRoot
  openStoryboardFilePathKILLME: string | null
  transientFileState: TransientFileState | null
  resolve: (importOrigin: string, toImport: string) => Either<string, string>
}
const EmptyResolve = (importOrigin: string, toImport: string): Either<string, string> => {
  return left(`Error while resolving ${toImport}, the resolver is missing`)
}

export const UtopiaProjectContext = createContext<UtopiaProjectContextProps>({
  projectContents: {},
  openStoryboardFilePathKILLME: null,
  transientFileState: null,
  resolve: EmptyResolve,
})

interface SceneLevelContextProps {
  validPaths: Array<InstancePath>
}

export const SceneLevelUtopiaContext = React.createContext<SceneLevelContextProps>({
  validPaths: [],
})
SceneLevelUtopiaContext.displayName = 'SceneLevelUtopiaContext'

interface ParentLevelUtopiaContextProps {
  templatePath: TemplatePath | null
}

export const ParentLevelUtopiaContext = createContext<ParentLevelUtopiaContextProps>({
  templatePath: null,
})
