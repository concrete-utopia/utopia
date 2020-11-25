import * as React from 'react'
import * as ReactDOM from 'react-dom'
import * as TP from '../../core/shared/template-path'
import * as EditorActions from '../editor/actions/actions'
import { getDependencyTypeDefinitions } from '../../core/es-modules/package-manager/package-manager'
import { RuntimeErrorInfo } from '../../core/shared/code-exec-utils'
import {
  AssetFile,
  Directory,
  HighlightBounds,
  HighlightBoundsForUids,
  ImageFile,
  isParseSuccess,
  isTextFile,
  ProjectFile,
  TemplatePath,
  TextFile,
} from '../../core/shared/project-file-types'
import { betterReactMemo, Utils } from '../../uuiui-deps'
import {
  ConsoleLog,
  EditorStore,
  EditorTab,
  getAllLintErrors,
  getOpenEditorTab,
  getOpenFile,
  getOpenUIJSFile,
  getOpenUIJSFileKey,
  isOpenFileTab,
  parseFailureAsErrorMessages,
} from '../editor/store/editor-state'
import { useEditorState } from '../editor/store/store-hook'
import { useKeepReferenceEqualityIfPossible } from '../inspector/common/property-path-hooks'
import { ScriptEditor, ScriptEditorProps } from './script-editor'
import { MetadataUtils } from '../../core/model/element-metadata-utils'
import { Notice } from '../common/notices'
import { CursorPosition } from './code-editor-utils'
import { EditorPanel, setFocus } from '../common/actions'
import { usePossiblyResolvedPackageDependencies } from '../editor/npm-dependency/npm-dependency'
import {
  PossiblyUnversionedNpmDependency,
  TypeDefinitions,
} from '../../core/shared/npm-dependency-types'
import { ErrorMessage } from '../../core/shared/error-messages'
import { ProjectContentTreeRoot } from '../assets'
import {
  BridgeTowardsMainEditor,
  CodeEditorAction,
  sendMonacoFullPropsMessage,
  useBridgeFromMainEditor,
  useBridgeTowardsIframe,
} from './code-editor-bridge'
import { MONACO_EDITOR_IFRAME_BASE_URL } from '../../common/env-vars'
import urljoin = require('url-join')
import { isFeatureEnabled } from '../../utils/feature-switches'

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

interface CodeEditorIframeEntryPointProps {}

export const CodeEditorIframeEntryPoint = betterReactMemo<CodeEditorIframeEntryPointProps>(
  'CodeEditorIframeEntryPoint',
  (props) => {
    const propsFromMainEditor = useBridgeFromMainEditor()
    const dispatch = BridgeTowardsMainEditor.sendCodeEditorAction

    return <CodeEditorEntryPoint propsFromMainEditor={propsFromMainEditor} dispatch={dispatch} />
  },
)

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
            : updateFileActions.concat(EditorActions.pushToast(toast))

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

const CodeEditorIframeID = 'code-editor-iframe'

const CodeEditorIframeContainer = betterReactMemo<{ propsToSend: JSONStringifiedCodeEditorProps }>(
  'CodeEditorIframeContainer',
  (props) => {
    const ref = React.useRef<HTMLIFrameElement>(null)
    // set up communications with the iframe
    useBridgeTowardsIframe(props.propsToSend, ref)

    const iframeSrc = urljoin(MONACO_EDITOR_IFRAME_BASE_URL, 'editor', 'monaco-editor-iframe.html')

    return (
      <iframe
        ref={ref}
        key={CodeEditorIframeID}
        id={CodeEditorIframeID}
        // width='0px'
        // height='0px'
        src={iframeSrc}
        allow='autoplay'
        style={{
          flex: 1,
          backgroundColor: 'transparent',
          // width: '0px',
          // height: '0px',
          borderWidth: 0,
        }}
      />
    )
  },
)

export const CodeEditorWrapper = betterReactMemo('CodeEditorWrapper', (props) => {
  const runtimeErrors: RuntimeErrorInfo[] = []
  const canvasConsoleLogs: ConsoleLog[] = []

  const selectedProps = useEditorState((store) => {
    const openEditorTab = getOpenEditorTab(store.editor)
    const openFilePath =
      openEditorTab != null && isOpenFileTab(openEditorTab) ? openEditorTab.filename : null
    const selectedFile = getOpenFile(store.editor)
    const openUIJSFile = getOpenUIJSFile(store.editor)
    const openUIJSFileKey = getOpenUIJSFileKey(store.editor)

    return {
      filePath: openFilePath,
      openFile: selectedFile,
      cursorPositionFromOpenFile:
        store.editor.selectedFile == null ? null : store.editor.selectedFile.initialCursorPosition,
      typeDefinitions: getDependencyTypeDefinitions(store.editor.nodeModules.files),
      lintErrors: getAllLintErrors(store.editor),
      parserPrinterErrors: parseFailureAsErrorMessages(openUIJSFileKey, openUIJSFile),
      projectContents: store.editor.projectContents,
      parsedHighlightBounds:
        openUIJSFile != null && isParseSuccess(openUIJSFile.fileContents.parsed)
          ? openUIJSFile.fileContents.parsed.highlightBounds
          : null,
      allTemplatePaths: store.derived.navigatorTargets,
      focusedPanel: store.editor.focusedPanel,
      codeEditorTheme: store.editor.codeEditorTheme,
      selectedViews: store.editor.selectedViews,
    }
  }, 'CodeEditorWrapper')

  const selectedViewBounds = useKeepReferenceEqualityIfPossible(
    useEditorState((store) => {
      return Utils.stripNulls(
        store.editor.selectedViews.map((selectedView) =>
          getHighlightBoundsForTemplatePath(selectedView, store),
        ),
      )
    }, 'CodeEditorWrapper selectedViewBounds'),
  )

  const highlightBounds = useKeepReferenceEqualityIfPossible(
    useEditorState((store) => {
      return Utils.stripNulls(
        store.editor.highlightedViews.map((highlightedView) =>
          getHighlightBoundsForTemplatePath(highlightedView, store),
        ),
      )
    }, 'CodeEditorWrapper highlightBounds'),
  )

  const npmDependencies = usePossiblyResolvedPackageDependencies()

  const dispatch = useEditorState((store) => store.dispatch, 'CodeEditorWrapper dispatch')

  const propsToSend: JSONStringifiedCodeEditorProps = {
    relevantPanel: 'uicodeeditor',
    runtimeErrors: runtimeErrors,
    canvasConsoleLogs: canvasConsoleLogs,
    filePath: selectedProps.filePath,
    openFile: selectedProps.openFile,
    cursorPositionFromOpenFile: selectedProps.cursorPositionFromOpenFile,
    typeDefinitions: selectedProps.typeDefinitions,
    lintErrors: selectedProps.lintErrors,
    parserPrinterErrors: selectedProps.parserPrinterErrors,
    projectContents: selectedProps.projectContents,
    parsedHighlightBounds: selectedProps.parsedHighlightBounds,
    allTemplatePaths: selectedProps.allTemplatePaths,
    focusedPanel: selectedProps.focusedPanel,
    codeEditorTheme: selectedProps.codeEditorTheme,
    selectedViews: selectedProps.selectedViews,
    selectedViewBounds: selectedViewBounds,
    highlightBounds: highlightBounds,
    npmDependencies: npmDependencies,
  }

  if (isFeatureEnabled('iFrame Code Editor')) {
    return <CodeEditorIframeContainer propsToSend={propsToSend} />
  } else {
    return <CodeEditorEntryPoint propsFromMainEditor={propsToSend} dispatch={dispatch} />
  }
})

function getHighlightBoundsForTemplatePath(path: TemplatePath, store: EditorStore) {
  const selectedFile = getOpenFile(store.editor)
  if (isTextFile(selectedFile) && isParseSuccess(selectedFile.fileContents.parsed)) {
    const parseSuccess = selectedFile.fileContents.parsed
    if (TP.isInstancePath(path)) {
      const highlightedUID = Utils.optionalMap(
        TP.toUid,
        MetadataUtils.dynamicPathToStaticPath(path),
      )
      if (highlightedUID != null) {
        return parseSuccess.highlightBounds[highlightedUID]
      }
    }
  }
  return null
}
