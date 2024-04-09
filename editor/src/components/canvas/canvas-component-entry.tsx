import React from 'react'
import {
  CanvasStateContext,
  EditorStateContext,
  Substores,
  useEditorState,
} from '../editor/store/store-hook'
import type {
  CanvasReactReportErrorCallback,
  UiJsxCanvasPropsWithErrorCallback,
} from './ui-jsx-canvas'
import {
  UiJsxCanvas,
  pickUiJsxCanvasProps,
  CanvasReactErrorCallback,
  DomWalkerInvalidatePathsCtxAtom,
  UiJsxCanvasProps,
} from './ui-jsx-canvas'
import { resetCanvas, saveDOMReport } from '../editor/actions/action-creators'
import { ElementInstanceMetadata } from '../../core/shared/element-template'
import { ConsoleLog } from '../editor/store/editor-state'
import type { CurriedUtopiaRequireFn } from '../custom-code/code-file'
import { UtopiaRequireFn } from '../custom-code/code-file'
import { ElementPath } from '../../core/shared/project-file-types'
import {
  useWriteOnlyConsoleLogs,
  useWriteOnlyRuntimeErrors,
} from '../../core/shared/runtime-report-logs'
import type { ProjectContentTreeRoot } from '../assets'
import type { FancyError } from '../../core/shared/code-exec-utils'
import { processErrorWithSourceMap } from '../../core/shared/code-exec-utils'
import { DomWalkerProps, useDomWalkerInvalidateCallbacks } from './dom-walker'
import { ResolvingRemoteDependencyErrorName } from '../../core/es-modules/package-manager/package-manager'
import { CanvasLoadingScreen } from './canvas-loading-screen'
import { isHooksErrorMessage } from '../../utils/canvas-react-utils'
import { useApplyCanvasOffsetToStyle } from './controls/canvas-offset-wrapper'
import { when } from '../../utils/react-conditionals'
import { useDispatch } from '../editor/store/dispatch-context'

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
  const { addToConsoleLogs, clearConsoleLogs } = useWriteOnlyConsoleLogs()

  const canvasProps = useEditorState(
    Substores.fullStore,
    (store) => {
      return pickUiJsxCanvasProps(
        store.editor,
        store.derived,
        dispatch,
        clearConsoleLogs,
        addToConsoleLogs,
      )
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
          width: '100%',
          height: '100%',
          transition: canvasScrollAnimation ? 'transform 0.3s ease-in-out' : 'initial',
          transform: 'translate3d(0px, 0px, 0px)',
        }}
      >
        {canvasProps == null ? null : (
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
              <DomWalkerWrapper {...canvasProps} clearErrors={localClearRuntimeErrors} />
            </RemoteDependencyBoundary>
          </CanvasErrorBoundary>
        )}
      </div>
    </>
  )
})

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
