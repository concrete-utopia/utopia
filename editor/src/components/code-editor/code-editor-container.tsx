import * as React from 'react'
import * as ReactDOM from 'react-dom'
import * as TP from '../../core/shared/template-path'
import * as EditorActions from '../editor/actions/action-creators'
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
  getAllLintErrors,
  getOpenEditorTab,
  getOpenFile,
  getOpenUIJSFile,
  getOpenUIJSFileKey,
  parseFailureAsErrorMessages,
} from '../editor/store/editor-state'
import { useEditorState } from '../editor/store/store-hook'
import { ScriptEditor, ScriptEditorProps } from './script-editor'
import { MetadataUtils } from '../../core/model/element-metadata-utils'
import { Notice } from '../common/notice'
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
import { isFeatureEnabled } from '../../utils/feature-switches'
import {
  CodeEditorNonIframeEntryPoint,
  JSONStringifiedCodeEditorProps,
} from './code-editor-iframe-entry-point'
import { isOpenFileTab } from '../editor/store/editor-tabs'
import { useKeepReferenceEqualityIfPossible } from '../../utils/react-performance'
import {
  useUpdateOnConsoleLogs,
  useUpdateOnRuntimeErrors,
} from '../../core/shared/runtime-report-logs'
import { createIframeUrl } from '../../core/shared/utils'

const CodeEditorIframeID = 'code-editor-iframe'

const CodeEditorIframeContainer = betterReactMemo<{ propsToSend: JSONStringifiedCodeEditorProps }>(
  'CodeEditorIframeContainer',
  (props) => {
    const ref = React.useRef<HTMLIFrameElement>(null)
    // set up communications with the iframe
    const { sendRuntimeErrors, sendCanvasConsoleLogs } = useBridgeTowardsIframe(
      props.propsToSend,
      ref,
    )
    useUpdateOnRuntimeErrors(sendRuntimeErrors)
    useUpdateOnConsoleLogs(sendCanvasConsoleLogs)

    const iframeSrc = createIframeUrl(MONACO_EDITOR_IFRAME_BASE_URL, 'monaco-editor-iframe.html')

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
    return <CodeEditorNonIframeEntryPoint propsFromMainEditor={propsToSend} dispatch={dispatch} />
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
