import * as React from 'react'
import { useEditorState } from '../editor/store/store-hook'
import {
  UiJsxCanvas,
  pickUiJsxCanvasProps,
  CanvasReactErrorCallback,
  CanvasReactReportErrorCallback,
  DomWalkerInvalidateScenesCtxAtom,
  DomWalkerInvalidatePathsCtxAtom,
  UiJsxCanvasProps,
  UiJsxCanvasPropsWithErrorCallback,
} from './ui-jsx-canvas'
import { saveDOMReport } from '../editor/actions/action-creators'
import { ElementInstanceMetadata } from '../../core/shared/element-template'
import { ConsoleLog } from '../editor/store/editor-state'
import { CurriedUtopiaRequireFn, UtopiaRequireFn } from '../custom-code/code-file'
import { betterReactMemo } from '../../uuiui-deps'
import { ElementPath } from '../../core/shared/project-file-types'
import {
  useWriteOnlyConsoleLogs,
  useWriteOnlyRuntimeErrors,
} from '../../core/shared/runtime-report-logs'
import { ProjectContentTreeRoot } from '../assets'
import { processErrorWithSourceMap } from '../../core/shared/code-exec-utils'
import { DomWalkerProps, useDomWalker } from './dom-walker'
import { ResolvingRemoteDependencyErrorName } from '../../core/es-modules/package-manager/package-manager'

interface CanvasComponentEntryProps {}

export const CanvasComponentEntry = betterReactMemo(
  'CanvasComponentEntry',
  (props: CanvasComponentEntryProps) => {
    const dispatch = useEditorState((store) => store.dispatch, 'CanvasComponentEntry dispatch')
    const onDomReport = React.useCallback(
      (
        elementMetadata: ReadonlyArray<ElementInstanceMetadata>,
        cachedPaths: Array<ElementPath>,
      ) => {
        dispatch([saveDOMReport(elementMetadata, cachedPaths)])
      },
      [dispatch],
    )
    const { onRuntimeError, clearRuntimeErrors } = useWriteOnlyRuntimeErrors()
    const { addToConsoleLogs, clearConsoleLogs } = useWriteOnlyConsoleLogs()

    const canvasProps = useEditorState((store) => {
      return pickUiJsxCanvasProps(
        store.editor,
        store.derived,
        true,
        onDomReport,
        clearConsoleLogs,
        addToConsoleLogs,
      )
    }, 'CanvasComponentEntry canvasProps')

    if (canvasProps == null) {
      return null
    } else {
      return (
        <div
          id='canvas-container-outer'
          style={{
            position: 'absolute',
            zoom: canvasProps.scale >= 1 ? `${canvasProps.scale * 100}%` : 1,
            transform:
              (canvasProps.scale < 1 ? `scale(${canvasProps.scale})` : '') +
              ` translate3d(${canvasProps.offset.x}px, ${canvasProps.offset.y}px, 0)`,
            transition: canvasProps.scrollAnimation ? 'transform 0.3s ease-in-out' : 'initial',
          }}
        >
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
              <DomWalkerWrapper {...canvasProps} clearErrors={clearRuntimeErrors} />
            </RemoteDependencyBoundary>
          </CanvasErrorBoundary>
        </div>
      )
    }
  },
)

function DomWalkerWrapper(props: UiJsxCanvasPropsWithErrorCallback) {
  const selectedViews = useEditorState(
    (store) => store.editor.selectedViews,
    'DomWalkerWrapper selectedViews',
  )
  let [updateInvalidatedPaths, updateInvalidatedScenes, containerRef] = useDomWalker({
    selectedViews: selectedViews,
    canvasInteractionHappening: props.transientFilesState != null,
    mountCount: props.mountCount,
    domWalkerInvalidateCount: props.domWalkerInvalidateCount,
    scale: props.scale,
    onDomReport: props.onDomReport,
  })

  return (
    <DomWalkerInvalidatePathsCtxAtom.Provider value={updateInvalidatedPaths}>
      <DomWalkerInvalidateScenesCtxAtom.Provider value={updateInvalidatedScenes}>
        <UiJsxCanvas {...props} ref={containerRef} />
      </DomWalkerInvalidateScenesCtxAtom.Provider>
    </DomWalkerInvalidatePathsCtxAtom.Provider>
  )
}

interface CanvasErrorBoundaryProps extends CanvasReactReportErrorCallback {
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
    const fancyError = processErrorWithSourceMap(error, true)
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
