import React from 'react'
import { emptySet } from '../../../core/shared/set-utils'
import type { MapLike } from 'typescript'
import { atomWithPubSub } from '../../../core/shared/atom-with-pub-sub'
import type { Either } from '../../../core/shared/either'
import { left } from '../../../core/shared/either'
import type { ElementPath } from '../../../core/shared/project-file-types'
import type { ProjectContentTreeRoot } from '../../assets'
import type { TransientFilesState, UIFileBase64Blobs } from '../../editor/store/editor-state'
import type { VariableData } from '../ui-jsx-canvas'
import type { FilePathMappings } from '../../../core/model/project-file-utils'
import type {
  ElementOrSelector,
  DOMKeyframesDefinition,
  DynamicAnimationOptions,
  AnimationPlaybackControls,
} from 'framer-motion'

export interface MutableUtopiaCtxRefData {
  [filePath: string]: {
    mutableContext: {
      requireResult: MapLike<any>
      fileBlobs: UIFileBase64Blobs
      rootScope: MapLike<any>
      spiedVariablesDeclaredInRootScope: VariableData
      jsxFactoryFunctionName: string | null
    }
  }
}

export function updateMutableUtopiaCtxRefWithNewProps(
  ref: React.MutableRefObject<MutableUtopiaCtxRefData>,
  newProps: MutableUtopiaCtxRefData,
): void {
  ref.current = newProps
}

interface RerenderUtopiaContextProps {
  hiddenInstances: Array<ElementPath>
  displayNoneInstances: Array<ElementPath>
  canvasIsLive: boolean
  shouldIncludeCanvasRootInTheSpy: boolean
  editedText: ElementPath | null
  filePathMappings: FilePathMappings
}

export const RerenderUtopiaCtxAtom = atomWithPubSub<RerenderUtopiaContextProps>({
  key: 'RerenderUtopiaCtxAtom',
  defaultValue: {
    hiddenInstances: [],
    displayNoneInstances: [],
    canvasIsLive: false,
    shouldIncludeCanvasRootInTheSpy: false,
    editedText: null,
    filePathMappings: [],
  },
})

interface UtopiaProjectCtxProps {
  projectContents: ProjectContentTreeRoot
  openStoryboardFilePathKILLME: string | null
  resolve: (importOrigin: string, toImport: string) => Either<string, string>
}
const EmptyResolve = (importOrigin: string, toImport: string): Either<string, string> => {
  return left(`Error while resolving ${toImport}, the resolver is missing`)
}

export const UtopiaProjectCtxAtom = atomWithPubSub<UtopiaProjectCtxProps>({
  key: 'UtopiaProjectCtxAtom',
  defaultValue: {
    projectContents: {},
    openStoryboardFilePathKILLME: null,
    resolve: EmptyResolve,
  },
})

interface SceneLevelContextProps {
  validPaths: Set<string>
}

export const SceneLevelUtopiaCtxAtom = atomWithPubSub<SceneLevelContextProps>({
  key: 'SceneLevelUtopiaCtxAtom',
  defaultValue: {
    validPaths: new Set(),
  },
})

export type AnimationCtx = {
  animate:
    | ((
        value: ElementOrSelector,
        keyframes: DOMKeyframesDefinition,
        options?: DynamicAnimationOptions | undefined,
      ) => AnimationPlaybackControls)
    | null
}

export const AnimationContext = React.createContext<AnimationCtx>({
  animate: null,
})
