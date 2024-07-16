import React from 'react'
import type { ElementPath } from 'utopia-shared/src/types'
import {
  CanvasStateContext,
  EditorStateContext,
  LowPriorityStateContext,
  Substores,
  useEditorState,
} from '../../editor/store/store-hook'
import { ElementsToRerenderGLOBAL } from '../ui-jsx-canvas'
import { UtopiaProjectCtxAtom } from './ui-jsx-canvas-contexts'
import { useKeepShallowReferenceEquality } from '../../../utils/react-performance'
import { optionalMap } from '../../../core/shared/optional-utils'
import { toString } from '../../../core/shared/element-path'
import { enableWhyDidYouRenderOnComponent } from '../../../utils/react-memoize.test-utils'
import { left } from '../../../core/shared/either'
import type { ResolveFn } from '../../custom-code/code-file'

type CanvasStateMeta = {
  providedStoreType: 'canvas' | 'low-priority'
  rerenderExpected: boolean
}

export const CanvasStateMetaContext = React.createContext<CanvasStateMeta | null>(null)

export const PerformanceOptimizedCanvasContextProvider: React.FunctionComponent<
  React.PropsWithChildren<{ targetPath: ElementPath | null }>
> = (props) => {
  const realtimeCanvasStore = React.useContext(CanvasStateContext)
  const maybeOldLowPriorityStore = React.useContext(LowPriorityStateContext)

  const shouldUseRealtimeStore =
    ElementsToRerenderGLOBAL.current === 'rerender-all-elements' ||
    (props.targetPath != null && ElementsToRerenderGLOBAL.current.includes(props.targetPath))

  const storeToUse = shouldUseRealtimeStore ? realtimeCanvasStore : maybeOldLowPriorityStore

  const canvasStateMeta: CanvasStateMeta = React.useMemo(() => {
    return {
      providedStoreType: shouldUseRealtimeStore ? 'canvas' : 'low-priority',
      rerenderExpected: shouldUseRealtimeStore,
    }
  }, [shouldUseRealtimeStore])

  // console.log(
  //   'CanvasStateMetaContext.Provider',
  //   optionalMap(toString, props.targetPath),
  //   canvasStateMeta,
  // )

  return (
    <CanvasStateMetaContext.Provider value={canvasStateMeta}>
      <EditorStateContext.Provider value={storeToUse}>
        <CanvasContextProviderInner>{props.children}</CanvasContextProviderInner>
      </EditorStateContext.Provider>
    </CanvasStateMetaContext.Provider>
  )
}
PerformanceOptimizedCanvasContextProvider.displayName = 'PerformanceOptimizedCanvasContextProvider'

const CanvasContextProviderInner: React.FunctionComponent<React.PropsWithChildren<unknown>> = (
  props,
) => {
  const utopiaProjectContextValue = useCreateUtopiaProjectCtx()
  const previousValue = React.useRef(utopiaProjectContextValue)

  // if (previousValue.current !== utopiaProjectContextValue) {
  //   console.log(
  //     'LOST CanvasContextProviderInner utopiaProjectContextValue LOST',
  //     utopiaProjectContextValue,
  //   )
  // } else {
  //   console.log('PRESERVED UtopiaProjectCtxAtom', utopiaProjectContextValue)
  // }
  previousValue.current = utopiaProjectContextValue

  return (
    <UtopiaProjectCtxAtom.Provider value={utopiaProjectContextValue}>
      {props.children}
    </UtopiaProjectCtxAtom.Provider>
  )
}
CanvasContextProviderInner.displayName = 'CanvasContextProviderInner'
// enableWhyDidYouRenderOnComponent(CanvasContextProviderInner)

function useCreateUtopiaProjectCtx() {
  const projectContents = useEditorState(
    Substores.projectContents,
    (store) => store.editor.projectContents,
    'useCreateUtopiaProjectCtx projectContents',
  )

  const uiFilePath = useEditorState(
    Substores.canvas,
    (store) => store.editor.canvas.openFile?.filename ?? null,
    'useCreateUtopiaProjectCtx uiFilePath',
  )

  const curriedResolveFn = useEditorState(
    Substores.restOfEditor,
    (store) => store.editor.codeResultCache.curriedResolveFn,
    'useCreateUtopiaProjectCtx curriedResolveFn',
  )

  const resolve: ResolveFn = React.useMemo(() => {
    return curriedResolveFn(projectContents)
  }, [curriedResolveFn, projectContents])

  const utopiaProjectContextValue = useKeepShallowReferenceEquality({
    projectContents: projectContents,
    openStoryboardFilePathKILLME: uiFilePath,
    resolve: resolve,
  })

  return utopiaProjectContextValue
}
