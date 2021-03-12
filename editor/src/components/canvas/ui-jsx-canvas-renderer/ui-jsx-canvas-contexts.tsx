import * as React from 'react'
import type { MapLike } from 'typescript'
import { createContext } from 'use-context-selector'
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
  focusedElementPath: ScenePath | null
}

export const RerenderUtopiaContext = createContext<RerenderUtopiaContextProps>({
  topLevelElements: new Map(),
  hiddenInstances: [],
  canvasIsLive: false,
  shouldIncludeCanvasRootInTheSpy: false,
  focusedElementPath: null,
})
RerenderUtopiaContext.displayName = 'RerenderUtopiaContext'

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
