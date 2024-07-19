import React from 'react'
import { ResolvingRemoteDependencyErrorName } from '../../core/es-modules/package-manager/package-manager'
import type { FancyError } from '../../core/shared/code-exec-utils'
import { processErrorWithSourceMap } from '../../core/shared/code-exec-utils'
import { useWriteOnlyRuntimeErrors } from '../../core/shared/runtime-report-logs'
import { isHooksErrorMessage } from '../../utils/canvas-react-utils'
import { when } from '../../utils/react-conditionals'
import type { ProjectContentTreeRoot } from '../assets'
import type { CurriedUtopiaRequireFn } from '../custom-code/code-file'
import { resetCanvas } from '../editor/actions/action-creators'
import { useDispatch } from '../editor/store/dispatch-context'
import {
  CanvasStateContext,
  EditorStateContext,
  Substores,
  useEditorState,
} from '../editor/store/store-hook'
import { CanvasLoadingScreen } from './canvas-loading-screen'
import { useApplyCanvasOffsetToStyle } from './controls/canvas-offset-wrapper'
import { useDomWalkerInvalidateCallbacks } from './dom-walker'
import type {
  CanvasReactReportErrorCallback,
  UiJsxCanvasProps,
  UiJsxCanvasPropsWithErrorCallback,
} from './ui-jsx-canvas'
import { DomWalkerInvalidatePathsCtxAtom, UiJsxCanvas, pickUiJsxCanvasProps } from './ui-jsx-canvas'

interface CanvasComponentEntryProps {}

export const CanvasComponentEntry = React.memo((props: CanvasComponentEntryProps) => {
  const canvasStore = React.useContext(CanvasStateContext)

  return (
    <EditorStateContext.Provider value={canvasStore == null ? null : canvasStore}>
      <CanvasComponentEntryInner {...props} />
    </EditorStateContext.Provider>
  )
})

const CanvasComponentEntryInner = React.memo((props: CanvasComponentEntryProps) => {
  const dispatch = useDispatch()

  const canvasScrollAnimation = useEditorState(
    Substores.canvas,
    (store) => store.editor.canvas.scrollAnimation,
    'CanvasComponentEntry scrollAnimation',
  )
  const { addToRuntimeErrors, clearRuntimeErrors } = useWriteOnlyRuntimeErrors()

  const canvasProps = useEditorState(
    Substores.fullStore,
    (store) => {
      return pickUiJsxCanvasProps(store.editor, store.derived)
    },
    'CanvasComponentEntry canvasProps',
  )

  const canvasEditOrSelect = React.useMemo(() => {
    // Explicitly target the case where the canvas is not live, needs to handle `undefined`.
    return canvasProps?.canvasIsLive === false
  }, [canvasProps?.canvasIsLive])

  const [lastRenderReactHookError, setLastRenderReactHookError] = React.useState(false)

  const onRuntimeError = React.useCallback(
    (editedFile: string, error: FancyError, errorInfo?: React.ErrorInfo) => {
      addToRuntimeErrors(editedFile, error, errorInfo)
      // Reset the canvas if we get a hooks error while the canvas is in edit/select modes.
      if (canvasEditOrSelect && isHooksErrorMessage(error.message) && !lastRenderReactHookError) {
        setLastRenderReactHookError(true)
        dispatch([resetCanvas()], 'everyone')
      }
    },
    [addToRuntimeErrors, canvasEditOrSelect, dispatch, lastRenderReactHookError],
  )

  const localClearRuntimeErrors = React.useCallback(() => {
    setLastRenderReactHookError(false)
    clearRuntimeErrors()
  }, [clearRuntimeErrors])

  const containerRef = useApplyCanvasOffsetToStyle(true)

  return (
    <>
      {when(canvasProps == null, <CanvasLoadingScreen />)}
      <div
        id='canvas-container-outer'
        ref={containerRef}
        style={{
          position: 'absolute',
          transition: canvasScrollAnimation ? 'transform 0.3s ease-in-out' : 'initial',
          transform: 'translate3d(0px, 0px, 0px)',
        }}
      >
        {canvasProps == null ? null : (
          <CanvasInner
            canvasProps={canvasProps}
            onRuntimeError={onRuntimeError}
            localClearRuntimeErrors={localClearRuntimeErrors}
          />
        )}
      </div>
    </>
  )
})

function CanvasInner({
  canvasProps,
  onRuntimeError,
  localClearRuntimeErrors,
}: {
  canvasProps: UiJsxCanvasProps
  onRuntimeError: (editedFile: string, error: FancyError, errorInfo?: React.ErrorInfo) => void
  localClearRuntimeErrors: () => void
}) {
  const invalidatedCanvasData = useInvalidatedCanvasRemount(
    canvasProps?.mountCount ?? 0,
    canvasProps?.domWalkerInvalidateCount ?? 0,
  )
  return (
    <CanvasErrorBoundary
      filePath={canvasProps.uiFilePath}
      projectContents={canvasProps.projectContents}
      reportError={onRuntimeError}
      requireFn={canvasProps.curriedRequireFn}
      key={`canvas-error-boundary-${canvasProps.mountCount}`}
    >
      <RemoteDependencyBoundary
        projectContents={canvasProps.projectContents}
        requireFn={canvasProps.curriedRequireFn}
      >
        <DomWalkerWrapper
          {...canvasProps}
          clearErrors={localClearRuntimeErrors}
          invalidatedCanvasData={invalidatedCanvasData}
        />
      </RemoteDependencyBoundary>
    </CanvasErrorBoundary>
  )
}

export function useInvalidatedCanvasRemount(
  mountCount: number,
  domWalkerInvalidateCount: number,
): {
  mountCountInvalidated: boolean
  domWalkerInvalidated: boolean
} {
  const previousMountCount = React.useRef<number>(mountCount)
  const previousDomWalkerInvalidateCount = React.useRef<number>(domWalkerInvalidateCount)

  const mountCountInvalidated = previousMountCount.current !== mountCount
  const domWalkerInvalidated = previousDomWalkerInvalidateCount.current !== domWalkerInvalidateCount

  previousMountCount.current = mountCount
  previousDomWalkerInvalidateCount.current = domWalkerInvalidateCount

  return {
    mountCountInvalidated,
    domWalkerInvalidated,
  }
}

function DomWalkerWrapper(props: UiJsxCanvasPropsWithErrorCallback) {
  let [updateInvalidatedPaths] = useDomWalkerInvalidateCallbacks()

  return (
    <DomWalkerInvalidatePathsCtxAtom.Provider value={updateInvalidatedPaths}>
      <UiJsxCanvas {...props} />
    </DomWalkerInvalidatePathsCtxAtom.Provider>
  )
}

interface CanvasErrorBoundaryProps extends CanvasReactReportErrorCallback {
  children?: React.ReactNode
  filePath: string
  projectContents: ProjectContentTreeRoot
  requireFn: CurriedUtopiaRequireFn | null
}

function isErrorObject(e: unknown): e is Error {
  return typeof e === 'object' && e != null && 'name' in e && 'message' in e
}

function asErrorObject(e: unknown): Error {
  // Required because JS supports throwing anything at all
  if (isErrorObject(e)) {
    return e
  } else if (typeof e === 'string') {
    return new Error(e)
  } else {
    return new Error(JSON.stringify(e))
  }
}

interface CanvasErrorBoundaryState {
  hasError: boolean
}

export class CanvasErrorBoundary extends React.Component<
  CanvasErrorBoundaryProps,
  CanvasErrorBoundaryState
> {
  constructor(props: CanvasErrorBoundaryProps) {
    super(props)
    this.state = { hasError: false }
  }

  static getDerivedStateFromError(_error: unknown): CanvasErrorBoundaryState {
    return {
      hasError: true,
    }
  }

  componentDidUpdate(
    prevProps: CanvasErrorBoundaryProps,
    _prevState: CanvasErrorBoundaryState,
  ): void {
    if (
      prevProps.projectContents !== this.props.projectContents ||
      prevProps.requireFn !== this.props.requireFn
    ) {
      // eslint-disable-next-line react/no-did-update-set-state
      this.setState({ hasError: false })
    }
  }

  componentDidCatch(error: Error, errorInfo: React.ErrorInfo): void {
    const fancyError = processErrorWithSourceMap(null, this.props.filePath, error, true)
    this.props.reportError(this.props.filePath, asErrorObject(fancyError), errorInfo)
  }

  render(): React.ReactNode {
    if (this.state.hasError) {
      return null
    } else {
      return this.props.children
    }
  }
}

interface RemoteDependencyBoundaryProps {
  children?: React.ReactNode
  projectContents: ProjectContentTreeRoot
  requireFn: CurriedUtopiaRequireFn | null
}

function maybeGetResolvingMessage(error: any): string | null {
  if (error?.name === ResolvingRemoteDependencyErrorName && error?.message != null) {
    return error.message
  } else {
    return null
  }
}

interface RemoteDependencyBoundaryState {
  resolvingMessage: string | null
}
export class RemoteDependencyBoundary extends React.Component<
  RemoteDependencyBoundaryProps,
  RemoteDependencyBoundaryState
> {
  constructor(props: RemoteDependencyBoundaryProps) {
    super(props)
    this.state = { resolvingMessage: null }
  }

  static getDerivedStateFromError(e: any): RemoteDependencyBoundaryState {
    return {
      resolvingMessage: maybeGetResolvingMessage(e),
    }
  }

  componentDidUpdate(prevProps: RemoteDependencyBoundaryProps): void {
    if (
      prevProps.projectContents !== this.props.projectContents ||
      prevProps.requireFn !== this.props.requireFn
    ) {
      // eslint-disable-next-line react/no-did-update-set-state
      this.setState({ resolvingMessage: null })
    }
  }

  componentDidCatch(error: Error): void {
    if (error?.name !== ResolvingRemoteDependencyErrorName) {
      throw error
    }
  }

  render(): React.ReactNode {
    if (this.state.resolvingMessage != null) {
      return <div>{this.state.resolvingMessage}</div>
    } else {
      return this.props.children
    }
  }
}
