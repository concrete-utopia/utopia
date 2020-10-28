import * as React from 'react'
import { useEditorState } from '../editor/store/store-hook'
import {
  UiJsxCanvas,
  pickUiJsxCanvasProps,
  CanvasReactErrorCallback,
  CanvasReactReportErrorCallback,
} from './ui-jsx-canvas'
import { betterReactMemo } from 'uuiui-deps'
import { saveDOMReport } from '../editor/actions/actions'
import { ElementInstanceMetadata } from '../../core/shared/element-template'
import { ConsoleLog } from '../editor/store/editor-state'
import { UtopiaRequireFn } from '../custom-code/code-file'
interface CanvasComponentEntryProps extends CanvasReactErrorCallback {
  clearConsoleLogs: () => void
  addToConsoleLogs: (log: ConsoleLog) => void
  canvasConsoleLogs: Array<ConsoleLog>
}

export const CanvasComponentEntry = betterReactMemo(
  'CanvasComponentEntry',
  (props: CanvasComponentEntryProps) => {
    const dispatch = useEditorState((store) => store.dispatch, 'CanvasComponentEntry dispatch')
    const onDomReport = React.useCallback(
      (elementMetadata: Array<ElementInstanceMetadata>) => {
        dispatch([saveDOMReport(elementMetadata)])
      },
      [dispatch],
    )
    const { canvasProps } = useEditorState(
      (store) => ({
        canvasProps: pickUiJsxCanvasProps(
          store.editor,
          store.derived,
          true,
          onDomReport,
          props.clearConsoleLogs,
          props.addToConsoleLogs,
          store.dispatch,
        ),
      }),
      'CanvasComponentEntry canvasProps',
    )

    if (canvasProps == null) {
      return null
    } else {
      return (
        <CanvasErrorBoundary
          fileCode={canvasProps.uiFileCode}
          filePath={canvasProps.uiFilePath}
          reportError={props.reportError}
          requireFn={canvasProps.requireFn}
          key={`canvas-error-boundary-${canvasProps.mountCount}`}
        >
          <UiJsxCanvas {...canvasProps} clearErrors={props.clearErrors} />
        </CanvasErrorBoundary>
      )
    }
  },
)

interface CanvasErrorBoundaryProps extends CanvasReactReportErrorCallback {
  filePath: string
  fileCode: string
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
      prevProps.fileCode !== this.props.fileCode ||
      prevProps.requireFn !== this.props.requireFn
    ) {
      // eslint-disable-next-line react/no-did-update-set-state
      this.setState({ hasError: false })
    }
  }

  componentDidCatch(error: unknown, errorInfo: React.ErrorInfo): void {
    this.props.reportError(this.props.filePath, asErrorObject(error), errorInfo)
  }

  render(): React.ReactNode {
    if (this.state.hasError) {
      return null
    } else {
      return this.props.children
    }
  }
}
