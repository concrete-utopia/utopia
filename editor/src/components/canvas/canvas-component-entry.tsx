import * as React from 'react'
import { useEditorState } from '../editor/store/store-hook'
import {
  UiJsxCanvas,
  pickUiJsxCanvasProps,
  CanvasReactErrorCallback,
  CanvasReactReportErrorCallback,
  DomWalkerInvalidateScenesContext,
  DomWalkerInvalidatePathsContext,
  UiJsxCanvasProps,
  UiJsxCanvasPropsWithErrorCallback,
} from './ui-jsx-canvas'
import { saveDOMReport } from '../editor/actions/action-creators'
import { ElementInstanceMetadata } from '../../core/shared/element-template'
import { ConsoleLog } from '../editor/store/editor-state'
import { UtopiaRequireFn } from '../custom-code/code-file'
import { betterReactMemo } from '../../uuiui-deps'
import { ElementPath } from '../../core/shared/project-file-types'
import {
  useWriteOnlyConsoleLogs,
  useWriteOnlyRuntimeErrors,
} from '../../core/shared/runtime-report-logs'
import { ProjectContentTreeRoot } from '../assets'
import { processErrorWithSourceMap } from '../../core/shared/code-exec-utils'
import { DomWalkerProps, useDomWalker } from './dom-walker'

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
            requireFn={canvasProps.requireFn}
            key={`canvas-error-boundary-${canvasProps.mountCount}`}
          >
            <DomWalkerWrapper {...canvasProps} clearErrors={clearRuntimeErrors} />
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
    <DomWalkerInvalidatePathsContext.Provider value={updateInvalidatedPaths}>
      <DomWalkerInvalidateScenesContext.Provider value={updateInvalidatedScenes}>
        <UiJsxCanvas {...props} ref={containerRef} />
      </DomWalkerInvalidateScenesContext.Provider>
    </DomWalkerInvalidatePathsContext.Provider>
  )
}

interface CanvasErrorBoundaryProps extends CanvasReactReportErrorCallback {
  filePath: string
  projectContents: ProjectContentTreeRoot
  requireFn: UtopiaRequireFn | null
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
