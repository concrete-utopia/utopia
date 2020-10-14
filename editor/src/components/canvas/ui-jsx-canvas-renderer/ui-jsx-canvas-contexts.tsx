import * as React from 'react'
import type { MapLike } from 'typescript'
import { EmptyScenePathForStoryboard } from '../../../core/model/scene-utils'
import type { UtopiaJSXComponent } from '../../../core/shared/element-template'
import type { InstancePath, ScenePath, TemplatePath } from '../../../core/shared/project-file-types'
import type { UIFileBase64Blobs } from '../../editor/store/editor-state'

export interface MutableUtopiaContextProps {
  requireResult: MapLike<any>
  fileBlobs: UIFileBase64Blobs
  rootScope: MapLike<any>
  jsxFactoryFunctionName: string | null
}

export const MutableUtopiaContext = React.createContext<{ current: MutableUtopiaContextProps }>({
  current: {
    requireResult: {},
    fileBlobs: {},
    rootScope: {},
    jsxFactoryFunctionName: null,
  },
})
MutableUtopiaContext.displayName = 'MutableUtopiaContext'

export function updateMutableUtopiaContextWithNewProps(
  ref: React.MutableRefObject<MutableUtopiaContextProps>,
  newProps: MutableUtopiaContextProps,
): void {
  ref.current = newProps
}

interface RerenderUtopiaContextProps {
  topLevelElements: ReadonlyMap<string, UtopiaJSXComponent>
  hiddenInstances: Array<TemplatePath>
  canvasIsLive: boolean
  shouldIncludeCanvasRootInTheSpy: boolean
}

export const RerenderUtopiaContext = React.createContext<RerenderUtopiaContextProps>({
  topLevelElements: new Map(),
  hiddenInstances: [],
  canvasIsLive: false,
  shouldIncludeCanvasRootInTheSpy: false,
})
RerenderUtopiaContext.displayName = 'RerenderUtopiaContext'

interface SceneLevelContextProps {
  validPaths: Array<InstancePath>
  scenePath: ScenePath
}

export const SceneLevelUtopiaContext = React.createContext<SceneLevelContextProps>({
  validPaths: [],
  scenePath: EmptyScenePathForStoryboard,
})
SceneLevelUtopiaContext.displayName = 'SceneLevelUtopiaContext'
