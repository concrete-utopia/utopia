import * as deepEquals from 'fast-deep-equal'
import * as monaco from 'monaco-editor'
import { SimpleEditorModelResolverService } from 'monaco-editor/esm/vs/editor/standalone/browser/simpleServices'
import { StaticServices } from 'monaco-editor/esm/vs/editor/standalone/browser/standaloneServices'
import * as React from 'react'
import { isDirectory } from '../../core/model/project-file-utils'
import { ErrorMessage, ErrorMessageSeverity } from '../../core/shared/error-messages'
import {
  TypeDefinitions,
  PossiblyUnversionedNpmDependency,
  isResolvedNpmDependency,
} from '../../core/shared/npm-dependency-types'
import {
  HighlightBounds,
  ProjectContents,
  isCodeFile,
  isUIJSFile,
  TemplatePath,
} from '../../core/shared/project-file-types'
import { isJsFile, isJsOrTsFile } from '../../core/workers/ts/ts-worker'
import { UtopiaTsWorkers } from '../../core/workers/common/worker-types'
import utils from '../../utils/utils'
import { RuntimeErrorInfo } from '../../core/shared/code-exec-utils'
import { getFilePathToImport } from '../filebrowser/filepath-utils'
import { CodeEditorTheme, getThemeDefinition } from './code-editor-themes'
import { CursorPosition, cursorPositionsEqual } from './code-editor-utils'
import { liftOff } from './text-mate-utils'
import { convertVSThemeToMonacoTheme } from './theme-converter'
import { OPENING_TAG_REGEX } from './monaco-wrapper-helper'
import { arrayEquals, fastForEach } from '../../core/shared/utils'
import * as TP from '../../core/shared/template-path'
import * as FontFaceObserver from 'fontfaceobserver'
import { splitIntoLines } from '../../core/shared/string-utils'

const CodeEditorFont = 'utopian-inconsolata'

interface MonacoWrapperProps {
  value: string
  filename: string
  options: monaco.editor.IEditorConstructionOptions
  tabSize: number
  onChange: (newValue: string) => void
  onChangeFromProps: (newValue: string) => void
  onChangeCursorPosition: (position: CursorPosition, shouldChangeSelection: boolean) => void
  saveCursorPosition: (position: CursorPosition) => void
  onHover: (line: number, column: number) => void
  onOpenFile: (path: string, cursorPosition: CursorPosition) => void
  npmTypeDefinitions: {
    npmDependencies: Array<PossiblyUnversionedNpmDependency>
    typeDefinitions: TypeDefinitions
  }
  cursorPosition: CursorPosition
  projectContents: ProjectContents
  errorMessages: Array<ErrorMessage> | null
  workers: UtopiaTsWorkers
  selectedViews: Array<TemplatePath>
  selectedViewsBounds: Array<HighlightBounds>
  highlightedViewsBounds: Array<HighlightBounds>
  selectedTheme: CodeEditorTheme
}

export function runtimeErrorInfoToErrorMessage(
  projectContents: ProjectContents,
  runtimeError: RuntimeErrorInfo,
): ErrorMessage {
  let filename: string = ''
  let lineNumber: number | null = null
  let columnNumber: number | null = null
  if (runtimeError.error.stackFrames != null) {
    const projectContentsKeys = Object.keys(projectContents)
    for (const stackFrame of runtimeError.error.stackFrames) {
      const originalFilenameAbsolutePath =
        stackFrame._originalFileName != null ? toAbsolutePath(stackFrame._originalFileName) : null
      if (
        originalFilenameAbsolutePath != null &&
        projectContentsKeys.includes(originalFilenameAbsolutePath)
      ) {
        filename = originalFilenameAbsolutePath
        lineNumber = stackFrame._originalLineNumber
        columnNumber = stackFrame._originalColumnNumber
        break
      } else if (stackFrame.fileName != null && projectContentsKeys.includes(stackFrame.fileName)) {
        filename = stackFrame.fileName
        lineNumber = stackFrame.lineNumber
        columnNumber = stackFrame.columnNumber
        break
      }
    }
  }
  return {
    fileName: filename,
    startLine: lineNumber,
    startColumn: columnNumber,
    endLine: lineNumber,
    endColumn: columnNumber,
    codeSnippet: '',
    severity: 'error',
    type: '',
    message: runtimeError.error.message,
    errorCode: '',
    source: 'runtime',
    passTime: null,
  }
}

interface MonacoWrapperState {
  cursorPosition: CursorPosition
}

type ViewStateCache = { [filename: string]: monaco.editor.ICodeEditorViewState }

// This makes 'Find All References' shortcut/contextmenu work with multiple files.
// The issue below refers to a merged PR but the problem is still not solved.
// https://github.com/Microsoft/monaco-editor/issues/779#issuecomment-374258435
SimpleEditorModelResolverService.prototype.findModel = (_: any, resource: any) => {
  return monaco.editor.getModels().find((model) => model.uri.toString() === resource.toString())
}

let extraLibs: { [fileuri: string]: { js: monaco.IDisposable; ts: monaco.IDisposable } } = {}
let completionProviders: { js: monaco.IDisposable; ts: monaco.IDisposable } | null = null
let hoverProviders: { js: monaco.IDisposable; ts: monaco.IDisposable } | null = null

const MaxSupportedLineLength = 200

function areAnyCodeLinesTooLong(code: string): boolean {
  const lines = splitIntoLines(code)
  const offendingLine = lines.find((line) => line.length > MaxSupportedLineLength)
  return offendingLine != null
}

export class MonacoWrapper extends React.Component<MonacoWrapperProps, MonacoWrapperState> {
  private rootRef: HTMLDivElement | null = null
  private monacoEditor: monaco.editor.IStandaloneCodeEditor | null = null
  private selectedViewDecorations: string[] = []
  private highlightedViewDecorations: string[] = []
  private viewStateCache: ViewStateCache
  private onChangeCallbacksEnabled: boolean = true

  constructor(props: MonacoWrapperProps) {
    super(props)
    this.viewStateCache = {}

    this.initMonacoTypescript()
    this.state = {
      cursorPosition: this.props.cursorPosition,
    }
  }

  componentDidMount() {
    this.initMonacoEditor()
    this.scheduleFocus()
  }

  componentDidUpdate(prevProps: MonacoWrapperProps) {
    if (
      this.props.npmTypeDefinitions.npmDependencies !== prevProps.npmTypeDefinitions.npmDependencies
    ) {
      // Checking the `npmDependencies` here is enough, since the reference will change after any changes
      // to the project's package.json
      this.initMonacoTypescript()
    }

    if (prevProps.selectedTheme !== this.props.selectedTheme) {
      initMonacoStyle(this.props.selectedTheme)
    }

    if (prevProps.filename != this.props.filename) {
      this.saveViewState(prevProps.filename)
      this.updateMonacoModel()
      this.restoreViewState()
    }
    if (this.monacoEditor != null) {
      if (this.monacoEditor.getValue() != this.props.value) {
        this.updateValueFromProps()
      }

      if (
        !cursorPositionsEqual(this.props.cursorPosition, this.state.cursorPosition) &&
        !cursorPositionsEqual(this.props.cursorPosition, prevProps.cursorPosition)
      ) {
        this.updateCursorPosition()
      }

      const newLineToScrollTo = this.props.selectedViewsBounds[0]
      const oldLineToScrollTo = prevProps.selectedViewsBounds[0]

      if (newLineToScrollTo != null) {
        if (
          oldLineToScrollTo == null ||
          (newLineToScrollTo.startLine !== oldLineToScrollTo.startLine &&
            newLineToScrollTo.endLine !== oldLineToScrollTo.endLine)
        ) {
          // we don't want to change the scroll position while the user is typing in the code editor
          const monacoEditorIsInFocus = this.monacoEditor.hasTextFocus()
          if (!monacoEditorIsInFocus) {
            // scroll the editor to the new lineToScrollTo
            this.monacoEditor.revealLines(
              newLineToScrollTo.startLine,
              newLineToScrollTo.endLine,
              monaco.editor.ScrollType.Smooth, // TODO why doesn't this actually animate? :(
            )
          }
        }
      }

      // update the line highlights for highlighted views
      this.highlightedViewDecorations = this.monacoEditor.deltaDecorations(
        this.highlightedViewDecorations,
        this.props.highlightedViewsBounds.map(
          (highlightBounds): monaco.editor.IModelDeltaDecoration => {
            return {
              range: {
                startLineNumber: highlightBounds.startLine + 1, // TS line numbers are zero based, monaco is 1-based
                startColumn: highlightBounds.startCol,
                endLineNumber: highlightBounds.endLine + 1, // TS line numbers are zero based, monaco is 1-based
                endColumn: highlightBounds.endCol,
              },
              options: {
                isWholeLine: true,
                className: 'highlight-highlighted-view-background-color',
              },
            }
          },
        ),
      )
      this.selectedViewDecorations = this.monacoEditor.deltaDecorations(
        this.selectedViewDecorations,
        this.props.selectedViewsBounds.map(
          (highlightBounds): monaco.editor.IModelDeltaDecoration => {
            return {
              range: {
                startLineNumber: highlightBounds.startLine + 1, // TS line numbers are zero based, monaco is 1-based
                startColumn: highlightBounds.startCol,
                endLineNumber: highlightBounds.endLine + 1, // TS line numbers are zero based, monaco is 1-based
                endColumn: highlightBounds.endCol,
              },
              options: {
                isWholeLine: true,
                className: 'highlight-selected-view-background-color',
              },
            }
          },
        ),
      )

      // As the bounds have potentially changed under the cursor,
      // notify the caller
      if (this.monacoEditor.hasTextFocus()) {
        if (
          prevProps.selectedViewsBounds !== this.props.selectedViewsBounds ||
          prevProps.highlightedViewsBounds !== this.props.highlightedViewsBounds
        ) {
          // If the selection has changed we shouldn't update it ourselves.
          const targetingSameElements = arrayEquals(
            prevProps.selectedViews,
            this.props.selectedViews,
            TP.pathsEqual,
          )
          this.props.onChangeCursorPosition(this.state.cursorPosition, targetingSameElements)
        }
      }

      // deepEquals is not scary here, because the options object is small, and I wanted to make
      // sure updateOptions do not run on every render
      if (!deepEquals(this.props.options, prevProps.options)) {
        this.monacoEditor.updateOptions(this.props.options)
      }
    }
    if (prevProps.errorMessages != this.props.errorMessages && isJsFile(this.props.filename)) {
      const errors = this.props.errorMessages == null ? [] : this.props.errorMessages
      const model = this.getOrCreateModel(monaco.Uri.file(this.props.filename))
      if (model != null) {
        monaco.editor.setModelMarkers(model, 'eslint', errors.map(errorMessageToMonacoMarkerData))
      }
    }
  }

  componentWillUnmount() {
    if (this.monacoEditor != null) {
      this.props.saveCursorPosition(this.state.cursorPosition)
      this.monacoEditor.dispose()
    }
  }

  initMonacoTypescript = () => {
    const compilerOptions = {
      module: monaco.languages.typescript.ModuleKind.CommonJS,
      moduleResolution: monaco.languages.typescript.ModuleResolutionKind.NodeJs,
      target: monaco.languages.typescript.ScriptTarget.ES2015,
      jsx: monaco.languages.typescript.JsxEmit.React,
      allowJs: true,
      allowSyntheticDefaultImports: true,
      esModuleInterop: true,
    }

    monaco.languages.typescript.typescriptDefaults.setCompilerOptions(compilerOptions)
    monaco.languages.typescript.javascriptDefaults.setCompilerOptions(compilerOptions)

    // This allows intelliSense for user files without using addextralib
    monaco.languages.typescript.typescriptDefaults.setEagerModelSync(true)
    monaco.languages.typescript.javascriptDefaults.setEagerModelSync(true)

    const definitions = this.props.npmTypeDefinitions.typeDefinitions
    Object.keys(definitions).forEach((filename) => {
      const fileUri = monaco.Uri.file(filename).toString()

      const extraLib = extraLibs[fileUri]
      if (extraLib) {
        extraLib.js.dispose()
        extraLib.ts.dispose()
      }

      const js = monaco.languages.typescript.javascriptDefaults.addExtraLib(
        definitions[filename],
        fileUri,
      )
      const ts = monaco.languages.typescript.typescriptDefaults.addExtraLib(
        definitions[filename],
        fileUri,
      )

      extraLibs[fileUri] = { js, ts }
    })

    this.initCompletionProvider()
    this.initHoverProvider()
    this.measureCustomFont(CodeEditorFont)
  }

  initCompletionProvider = () => {
    // Completion provider to provide autocomplete for files and dependencies
    const completionProvider: monaco.languages.CompletionItemProvider = {
      triggerCharacters: ["'", '"', '.', '/'],
      provideCompletionItems: (
        model: monaco.editor.ITextModel,
        position: monaco.Position,
      ): monaco.languages.ProviderResult<monaco.languages.CompletionList> => {
        // Get editor content before the pointer
        const textUntilPosition = model.getValueInRange({
          startLineNumber: 1,
          startColumn: 1,
          endLineNumber: position.lineNumber,
          endColumn: position.column,
        })
        // this matches `import "`, `from "`, `require("`
        const importRegexp = /(([\s|\n]+(import|from)\s+)|(\brequire\b\s*\())["|'][^'^"]*$/
        if (importRegexp.test(textUntilPosition)) {
          // importing from a file, '/' character triggers suggestion list
          if (textUntilPosition.endsWith('/') || textUntilPosition.endsWith('.')) {
            const textAfterQuotes = textUntilPosition.match(/[^'"]+$/)
            const typedPath = textAfterQuotes ? textAfterQuotes[0] : ''

            let suggestions: monaco.languages.CompletionItem[] = []
            utils.fastForEach(Object.keys(this.props.projectContents), (filename) => {
              const file = this.props.projectContents[filename]
              if (
                filename !== this.props.filename &&
                (isDirectory(file) ||
                  isUIJSFile(file) ||
                  (isCodeFile(file) && isJsOrTsFile(filename)))
              ) {
                let fileWithRelativePath = getFilePathToImport(filename, this.props.filename)
                const isInSameDirAsTyped =
                  fileWithRelativePath.split('/').length <= typedPath.split('/').length
                if (fileWithRelativePath.startsWith(typedPath) && isInSameDirAsTyped) {
                  // the already written path is removed from the filepath
                  const filePathStripped = fileWithRelativePath.slice(typedPath.length)
                  const fileKind = isDirectory(file)
                    ? monaco.languages.CompletionItemKind.Folder
                    : monaco.languages.CompletionItemKind.File

                  const textToInsert =
                    isUIJSFile(file) || isCodeFile(file)
                      ? filePathStripped.replace(/\.(js|tsx?)$/, '')
                      : filePathStripped

                  if (
                    filePathStripped !== '' &&
                    suggestions.find((item) => item.insertText === textToInsert) == null
                  ) {
                    suggestions.push({
                      label: filePathStripped,
                      insertText: textToInsert,
                      kind: fileKind,
                    } as monaco.languages.CompletionItem)
                  }
                }
              }
            })

            return { suggestions }
          } else {
            // importing from npm dependencies
            const dependencies = this.props.npmTypeDefinitions.npmDependencies

            let suggestions: monaco.languages.CompletionItem[] = []
            fastForEach(dependencies, (dependency) => {
              if (isResolvedNpmDependency(dependency)) {
                suggestions.push({
                  label: dependency.name,
                  insertText: dependency.name,
                  kind: monaco.languages.CompletionItemKind.Module,
                } as monaco.languages.CompletionItem)
              }
            })
            return {
              suggestions: suggestions,
            }
          }
        }

        return null
      },
    }

    if (completionProviders != null) {
      completionProviders.js.dispose()
      completionProviders.ts.dispose()
    }

    const js = monaco.languages.registerCompletionItemProvider('javascript', completionProvider)
    const ts = monaco.languages.registerCompletionItemProvider('typescript', completionProvider)
    completionProviders = {
      js: js,
      ts: ts,
    }
  }

  initHoverProvider = () => {
    const hoverProvider: monaco.languages.HoverProvider = {
      provideHover: (
        model: monaco.editor.ITextModel,
        position: monaco.Position,
        token: monaco.CancellationToken,
      ) => {
        this.props.onHover(position.lineNumber, position.column)
        return {
          contents: [],
        }
      },
    }

    if (hoverProviders != null) {
      hoverProviders.js.dispose()
      hoverProviders.ts.dispose()
    }

    const js = monaco.languages.registerHoverProvider('javascript', hoverProvider)
    const ts = monaco.languages.registerHoverProvider('typescript', hoverProvider)

    hoverProviders = {
      js: js,
      ts: ts,
    }
  }

  measureCustomFont = (fontFamilyName: string) => {
    const codeEditorFont = new FontFaceObserver(fontFamilyName)
    codeEditorFont.load().then(() => {
      monaco.editor.remeasureFonts()
    })
  }

  assignRef = (element: HTMLDivElement) => {
    this.rootRef = element
  }

  initMonacoEditor = () => {
    const fileUri = monaco.Uri.file(this.props.filename)

    const onContentChange = (event: monaco.editor.IModelContentChangedEvent) => {
      if (this.onChangeCallbacksEnabled) {
        const monacoEditor = utils.forceNotNull(
          'the monaco editor should not be null here',
          this.monacoEditor,
        )
        const newValue = monacoEditor.getValue()
        if (newValue !== this.props.value) {
          this.props.onChange(newValue)
        }
      }
      this.closeJsxTagsOnChange(event)
    }

    // Loading all project files to monaco to allow intelliSense
    // it's used in combination with setEagerModelSync
    utils.fastForEach(Object.keys(this.props.projectContents), (key: string) => {
      const file = this.props.projectContents[key]
      const filename = monaco.Uri.file(key)

      let code: string | null = null
      if (isCodeFile(file)) {
        code = file.fileContents
      } else if (isUIJSFile(file)) {
        code = file.fileContents.value.code
      }
      if (code != null) {
        let model = findModel(filename.toString())
        if (model == null || model.isDisposed()) {
          model = this.createModel(code, filename)
          model.updateOptions({ tabSize: this.props.tabSize })
        }
      }
    })

    const onCursorChange = (e: monaco.editor.ICursorPositionChangedEvent) => {
      const position: CursorPosition = {
        line: e.position.lineNumber,
        column: e.position.column,
      }
      this.setState({ cursorPosition: position })

      if (this.onChangeCallbacksEnabled) {
        this.props.onChangeCursorPosition(position, true)
      }
    }

    if (this.rootRef != null) {
      const options: monaco.editor.IStandaloneEditorConstructionOptions = {
        ...this.props.options,
        value: 'model: null',
        language: 'model: null',
        model: this.getOrCreateModel(fileUri),
        fontFamily: `${CodeEditorFont}, Akkurat-Mono Menlo, Monaco, fixed`,
        fontSize: 14,
        wordWrap: 'on',
        smoothScrolling: true,
        autoIndent: 'full',
      }
      const serviceOverride: monaco.editor.IEditorOverrideServices = {
        codeEditorService: this.getPatchedCodeEditorService(),
      }
      const editor = monaco.editor.create(this.rootRef, options, serviceOverride)
      editor.onDidChangeModelContent(onContentChange)
      editor.onDidChangeCursorPosition(onCursorChange)
      this.monacoEditor = editor
      if (this.monacoEditor.getValue() != this.props.value) {
        this.updateValueFromProps()
      }
      this.updateCursorPosition()

      // Hack for passing through the CMD + Enter live canvas keyboard event to the window
      // if this needs updating, make sure to update global-shortcuts.ts as well
      editor.addCommand(monaco.KeyMod.CtrlCmd | monaco.KeyCode.Enter, () =>
        window.dispatchEvent(
          new KeyboardEvent('keydown', {
            keyCode: 13,
            key: 'Enter',
            code: 'Enter',
            metaKey: true,
            bubbles: true,
          } as any), // https://github.com/Microsoft/TypeScript/issues/15228
        ),
      )

      editor.addCommand(monaco.KeyMod.Alt | monaco.KeyCode.Enter, () => {
        editor.trigger('', 'editor.action.insertLineAfter', '')
      })
      editor.addCommand(monaco.KeyMod.Alt | monaco.KeyCode.Shift | monaco.KeyCode.Enter, () => {
        editor.trigger('', 'editor.action.insertLineBefore', '')
      })
    }

    monaco.languages.typescript.getJavaScriptWorker().then(() => {
      liftOff(monaco).then(() => {
        initMonacoStyle(this.props.selectedTheme)
      })
    })
  }

  createModel = (code: string, fileUri: monaco.Uri): monaco.editor.ITextModel => {
    const linesAreTooLong = areAnyCodeLinesTooLong(code)
    const language: string | undefined = linesAreTooLong ? 'plaintext' : undefined // disable language support of files with long lines
    return monaco.editor.createModel(code, language, fileUri)
  }

  getOrCreateModel = (fileUri: monaco.Uri) => {
    let model = findModel(fileUri.toString())
    if (model == null || model.isDisposed()) {
      model = this.createModel(this.props.value, fileUri)
      model.updateOptions({ tabSize: this.props.tabSize })
    }
    return model
  }

  updateMonacoModel = () => {
    const fileUri = monaco.Uri.file(this.props.filename)
    const model = this.getOrCreateModel(fileUri)
    const monacoEditor = utils.forceNotNull(
      'the monaco editor should not be null here',
      this.monacoEditor,
    )
    monacoEditor.setModel(model)
  }

  updateValueFromProps = () => {
    // update monaco editor's value based on props
    const monacoEditor = utils.forceNotNull(
      'the monaco editor should not be null here',
      this.monacoEditor,
    )
    const model = monacoEditor.getModel()
    if (model != null) {
      this.onChangeCallbacksEnabled = false
      model.pushEditOperations(
        [],
        [
          {
            range: model.getFullModelRange(),
            text: this.props.value,
          },
        ],
        () => [],
      )
      this.onChangeCallbacksEnabled = true
    }
    this.props.onChangeFromProps(this.props.value)
  }

  updateCursorPosition = () => {
    const monacoEditor = utils.forceNotNull(
      'the monaco editor should not be null here',
      this.monacoEditor,
    )

    monacoEditor.setPosition({
      lineNumber: this.props.cursorPosition.line,
      column: this.props.cursorPosition.column,
    })
    monacoEditor.revealLineInCenterIfOutsideViewport(this.props.cursorPosition.line)
    this.scheduleFocus()
  }

  saveViewState = (filename: string) => {
    const monacoEditor = utils.forceNotNull(
      'the monaco editor should not be null here',
      this.monacoEditor,
    )
    const currentViewState = monacoEditor.saveViewState()
    if (currentViewState != null) {
      this.viewStateCache[filename] = currentViewState
    }
  }

  restoreViewState = () => {
    const viewState = this.viewStateCache[this.props.filename]
    const monacoEditor = utils.forceNotNull(
      'the monaco editor should not be null here',
      this.monacoEditor,
    )
    if (viewState != null) {
      monacoEditor.restoreViewState(viewState)
    } else {
      monacoEditor.setPosition({ lineNumber: 1, column: 1 })
    }
    this.scheduleFocus()
  }

  scheduleFocus = () => {
    setTimeout(() => {
      if (this.monacoEditor != null) {
        this.monacoEditor.focus()
      }
    })
  }

  getPatchedCodeEditorService = () => {
    let codeEditorService = StaticServices.codeEditorService.get()
    codeEditorService.openCodeEditor = async (
      resource: any,
      editor: monaco.editor.IStandaloneCodeEditor,
    ) => {
      const cursorPosition = {
        line: resource.options.selection.startLineNumber,
        column: resource.options.selection.startColumn,
      }
      if (resource.resource.path != this.props.filename) {
        this.props.onOpenFile(resource.resource.path, cursorPosition)
      } else {
        editor.setPosition({
          ...cursorPosition,
          lineNumber: cursorPosition.line,
        })
        editor.revealLineInCenterIfOutsideViewport(cursorPosition.line)
      }

      return {
        getControl: () => editor,
      }
    }
    return codeEditorService
  }

  private closeJsxTagsOnChange(event: monaco.editor.IModelContentChangedEvent) {
    if (event.isUndoing || event.isRedoing || !isJsOrTsFile(this.props.filename)) {
      return
    }

    const changes = event.changes
    const lastChange = changes[changes.length - 1]
    const lastCharacter = lastChange.text[lastChange.text.length - 1]

    // no need to autoclose if the last added character is not '>'
    if (lastCharacter !== '>') {
      return
    }

    if (this.monacoEditor == null) {
      return
    }

    const model = this.monacoEditor.getModel()
    if (model == null) {
      return
    }

    const changeRange = lastChange.range
    const textChangeArray = lastChange.text.replace(/(\\n)/g, '').split(/[\n\r]/g)
    const textChangeNewLines = textChangeArray.length - 1
    const textChangeEndLineNumber = changeRange.startLineNumber + textChangeNewLines
    const textChangeEndColumnNumber =
      textChangeNewLines === 0
        ? changeRange.startColumn + lastChange.text.length
        : textChangeArray[textChangeNewLines].length + 1

    const rangeBefore = new monaco.Range(0, 0, textChangeEndLineNumber, textChangeEndColumnNumber)
    const contentBefore = model.getValueInRange(rangeBefore)

    const fullContent = model.getValue()
    const contentAfter = fullContent.slice(contentBefore.length, fullContent.length)

    const openingTagMatch = contentBefore.match(OPENING_TAG_REGEX)
    const tagname =
      openingTagMatch != null && openingTagMatch.length > 2 ? openingTagMatch[1] : null

    // there is no tag found, we return
    if (tagname == null) {
      return
    }

    const closingTagRegex = new RegExp(`^[^<]*<\/${tagname}\s*>`)
    const closingTagMatch = contentAfter.match(closingTagRegex)

    // we found the closing tag, no need to duplicate it
    if (closingTagMatch != null) {
      return
    }

    const closingTag = `</${tagname}>`
    const changeLines = lastChange.text.split('\n')
    const linesInserted = changeLines.length
    const lastLineLength = changeLines[changeLines.length - 1].length

    // we need to calculate where to insert, taking into account that the last change will be applied before the insertion of
    // the closing tag happens
    const insertAtLine =
      linesInserted === 1
        ? changeRange.endLineNumber
        : changeRange.endLineNumber + linesInserted - 1
    const insertAtCol =
      linesInserted === 1 ? changeRange.endColumn + lastLineLength : lastLineLength + 1
    const insertRange = new monaco.Range(insertAtLine, insertAtCol, insertAtLine, insertAtCol)
    model.pushEditOperations(
      [],
      [
        {
          range: insertRange,
          text: closingTag,
        },
      ],
      () => [],
    )
  }

  render() {
    return (
      <div
        ref={this.assignRef}
        className='monaco-editor-root'
        style={{
          // absolute positioning to preserve pane resizing capabilities
          // relative position in  monaco-editor-root
          position: 'absolute',
          width: '100%',
          height: '100%',
          flexGrow: 1,
        }}
      />
    )
  }
}

function initMonacoStyle(themeName: CodeEditorTheme) {
  monaco.editor.defineTheme(themeName, convertVSThemeToMonacoTheme(getThemeDefinition(themeName)))
  monaco.editor.setTheme(themeName)
}

function findModel(uri: string) {
  return monaco.editor.getModels().find((model) => model.uri.toString() == uri.toString())
}

function errorMessageToMonacoMarkerData(err: ErrorMessage) {
  const startLine = err.startLine == null ? 0 : err.startLine
  const startColumn = err.startColumn == null ? 0 : err.startColumn
  const endLine = err.endLine == null ? 0 : err.endLine
  const endColumn = err.endColumn == null ? 0 : err.endColumn
  return {
    startLineNumber: startLine,
    endLineNumber: endLine,
    startColumn: startColumn,
    endColumn: endColumn,
    message: err.message,
    severity: errorMessageSeverityToMonacoMarkerSeverity(err.severity),
    source: err.source,
  }
}

function errorMessageSeverityToMonacoMarkerSeverity(severity: ErrorMessageSeverity) {
  switch (severity) {
    case 'warning':
      return monaco.MarkerSeverity.Warning
    case 'error':
    case 'fatal':
      return monaco.MarkerSeverity.Error
    default:
      const _exhaustiveCheck: never = severity
      throw new Error(`Unknown severity ${severity}}`)
  }
}

function toAbsolutePath(path: string) {
  return path.length == 0 || path[0] != '/' ? `/${path}` : path
}
