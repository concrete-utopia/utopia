import * as React from 'react'

import type { RuntimeErrorInfo } from '../../core/shared/code-exec-utils'
import type {
  AssetFile,
  Directory,
  HighlightBounds,
  HighlightBoundsForUids,
  ImageFile,
  ProjectFile,
  TemplatePath,
  TextFile,
} from '../../core/shared/project-file-types'
import { EditorPanel, setFocus } from '../common/actions'
import type { ConsoleLog } from '../editor/store/editor-state'
import type { CursorPosition } from './code-editor-utils'
import { betterReactMemo } from '../../utils/react-performance'
import type {
  PossiblyUnversionedNpmDependency,
  TypeDefinitions,
} from '../../core/shared/npm-dependency-types'
import type { ErrorMessage } from '../../core/shared/error-messages'
import type { ProjectContentTreeRoot } from '../assets'
import { Notice } from '../common/notice'

import {
  BridgeTowardsMainEditor,
  CodeEditorAction,
  useBridgeFromMainEditor,
} from './code-editor-bridge'

import * as EditorActions from '../../components/editor/actions/action-creators'
import { ScriptEditor } from './script-editor'
import { EditorTab } from '../editor/store/editor-tabs'

export interface JSONStringifiedCodeEditorProps {
  relevantPanel: EditorPanel
  runtimeErrors: Array<RuntimeErrorInfo>
  canvasConsoleLogs: Array<ConsoleLog>
  selectedViews: TemplatePath[]
  filePath: string | null
  openFile: TextFile | ImageFile | Directory | AssetFile | null
  cursorPositionFromOpenFile: CursorPosition | null
  typeDefinitions: TypeDefinitions
  lintErrors: ErrorMessage[]
  parserPrinterErrors: ErrorMessage[]
  projectContents: ProjectContentTreeRoot
  parsedHighlightBounds: HighlightBoundsForUids | null
  allTemplatePaths: TemplatePath[]
  focusedPanel: EditorPanel | null
  codeEditorTheme: string
  selectedViewBounds: HighlightBounds[]
  highlightBounds: HighlightBounds[]
  npmDependencies: PossiblyUnversionedNpmDependency[]
}

export const CodeEditorIframeEntryPoint = betterReactMemo('CodeEditorIframeEntryPoint', () => {
  const propsFromMainEditor = useBridgeFromMainEditor()
  const dispatch = BridgeTowardsMainEditor.sendCodeEditorAction

  return <CodeEditorEntryPoint propsFromMainEditor={propsFromMainEditor} dispatch={dispatch} />
})

export interface CodeEditorEntryPointProps {
  propsFromMainEditor: JSONStringifiedCodeEditorProps | null
  dispatch: (actions: CodeEditorAction[]) => void
}

export const CodeEditorEntryPoint = betterReactMemo<CodeEditorEntryPointProps>(
  'CodeEditorEntryPoint',
  (props) => {
    const { dispatch, propsFromMainEditor } = props
    const setHighlightedViews = React.useCallback(
      (targets: TemplatePath[]) => {
        const actions = targets.map((path) => EditorActions.setHighlightedView(path))
        dispatch(actions)
      },
      [dispatch],
    )

    const selectComponents = React.useCallback(
      (targets: TemplatePath[]) => {
        dispatch([EditorActions.selectComponents(targets, false)])
      },
      [dispatch],
    )

    const onSave = React.useCallback(
      (
        manualSave: boolean,
        toast: Notice | undefined,
        filePath: string,
        updatedFile: ProjectFile,
      ) => {
        const updateFileActions: CodeEditorAction[] = [
          EditorActions.updateFile(filePath, updatedFile, false),
          EditorActions.setCodeEditorBuildErrors({}),
        ]
        const withToastAction =
          toast == null
            ? updateFileActions
            : updateFileActions.concat(EditorActions.addToast(toast))

        const actions = manualSave
          ? withToastAction.concat(EditorActions.saveCurrentFile())
          : withToastAction
        dispatch(actions)
      },
      [dispatch],
    )

    const openEditorTab = React.useCallback(
      (editorTab: EditorTab, cursorPosition: CursorPosition | null) => {
        dispatch([EditorActions.openEditorTab(editorTab, cursorPosition)])
      },
      [dispatch],
    )

    const setCodeEditorVisibility = React.useCallback(
      (visible: boolean) => {
        dispatch([EditorActions.setCodeEditorVisibility(visible)])
      },
      [dispatch],
    )

    const setFocusCallback = React.useCallback(
      (panel: EditorPanel | null) => {
        dispatch([setFocus(panel)])
      },
      [dispatch],
    )

    const sendLinterRequestMessage = React.useCallback(
      (filePath: string, content: string) => {
        dispatch([EditorActions.sendLinterRequestMessage(filePath, content)])
      },
      [dispatch],
    )

    if (propsFromMainEditor == null) {
      return null
    } else {
      return (
        <ScriptEditor
          sendLinterRequestMessage={sendLinterRequestMessage}
          setHighlightedViews={setHighlightedViews}
          selectComponents={selectComponents}
          onSave={onSave}
          openEditorTab={openEditorTab}
          setCodeEditorVisibility={setCodeEditorVisibility}
          setFocus={setFocusCallback}
          {...propsFromMainEditor}
        />
      )
    }
  },
)
