/** @jsx jsx */
import { jsx } from '@emotion/core'
import * as React from 'react'
import { applyPrettier } from '../../core/workers/parser-printer/prettier-utils'
import { ErrorMessage } from '../../core/shared/error-messages'
import { UtopiaTsWorkers } from '../../core/workers/common/worker-types'
import { Keyboard, modifiersForEvent, strictCheckModifiers } from '../../utils/keyboard'
import {
  HighlightBounds,
  ProjectContents,
  TemplatePath,
} from '../../core/shared/project-file-types'
import {
  TypeDefinitions,
  PossiblyUnversionedNpmDependency,
} from '../../core/shared/npm-dependency-types'
import { ConsoleLog } from '../editor/store/editor-state'
import { CodeEditorTheme } from './code-editor-themes'
import {
  CursorPosition,
  cursorPositionToRawOffset,
  rawOffsetToCursorPosition,
} from './code-editor-utils'
import { CodeEditorTabPane } from './code-problems'
import { MonacoWrapper } from './monaco-wrapper'
import { Notice } from '../common/notices'
import { ProjectContentTreeRoot } from '../assets'

const AutoSaveDelay = 300
export interface CodeEditorProps {
  name: string
  value: string
  onSave: (value: string, manualSave: boolean, toast?: Notice) => void
  onChange: (value: string) => void
  onChangeFromProps: (value: string) => void
  saveCursorPosition: (position: CursorPosition) => void
  onOpenFile: (path: string, cursorPosition: CursorPosition | null) => void
  onHover: (line: number, column: number) => void
  onSelect: (line: number, column: number) => void
  onFocus: () => void
  close: () => void
  enabled: boolean
  autoSave: boolean
  npmTypeDefinitions: {
    npmDependencies: Array<PossiblyUnversionedNpmDependency>
    typeDefinitions: TypeDefinitions
  }
  allErrors: Array<ErrorMessage> | null
  errorsForFile: Array<ErrorMessage> | null
  readOnly: boolean
  projectContents: ProjectContentTreeRoot
  workers: UtopiaTsWorkers
  selectedViews: Array<TemplatePath>
  selectedViewsBounds: Array<HighlightBounds>
  highlightedViewsBounds: Array<HighlightBounds>
  cursorPosition: CursorPosition | null
  canvasConsoleLogs: Array<ConsoleLog>
  codeEditorTheme: CodeEditorTheme
}

interface CodeEditorState {
  code: string
  cursorPosition: CursorPosition
  propsCode: string
  propsCursorPosition: CursorPosition | null
  dirty: boolean
}

function cursorPositionsEqual(l: CursorPosition, r: CursorPosition): boolean {
  return l.column === r.column && l.line === r.line
}

export class CodeEditor extends React.Component<CodeEditorProps, CodeEditorState> {
  autoSaveTimer: NodeJS.Timer | null = null
  dirty: boolean = false

  constructor(props: CodeEditorProps) {
    super(props)
    this.state = {
      code: props.value,
      cursorPosition: initCursorPosition(props.cursorPosition),
      propsCode: props.value,
      propsCursorPosition: props.cursorPosition,
      dirty: false,
    }
    this.props.onChange(props.value)
  }

  save = (value: string, manualSave: boolean, toast?: Notice) => {
    this.setState({ dirty: false })
    this.props.onSave(value, manualSave, toast)
  }

  autoSave = () => {
    if (this.props.autoSave) {
      this.save(this.state.code, false)
    }
  }

  scheduleAutoSave = () => {
    clearTimeout(this.autoSaveTimer!)
    this.autoSaveTimer = setTimeout(this.autoSave, AutoSaveDelay)
  }

  componentDidUpdate(prevProps: CodeEditorProps) {
    if (prevProps.name != this.props.name) {
      this.props.onChange(this.props.value)
    }
  }

  static getDerivedStateFromProps(
    props: CodeEditorProps,
    state: CodeEditorState,
  ): CodeEditorState | null {
    const codeChanged = props.value !== state.propsCode && !state.dirty
    const cursorChanged = props.cursorPosition !== state.propsCursorPosition
    if (codeChanged || cursorChanged) {
      return {
        ...state,
        code: codeChanged ? props.value : state.code,
        cursorPosition: cursorChanged
          ? initCursorPosition(props.cursorPosition)
          : state.cursorPosition,
        propsCode: props.value,
        propsCursorPosition: props.cursorPosition,
      }
    } else {
      return null
    }
  }

  componentWillUnmount() {
    // Disabled here as unmounts were occurring while dragging triggering a save, which caused the canvas to snap back.
    //this.autoSave()
  }

  onKeyDown = (event: React.KeyboardEvent<HTMLDivElement>) => {
    const key = Keyboard.keyCharacterForCode(event.keyCode)
    const modifiers = modifiersForEvent(event.nativeEvent)
    const cmd = strictCheckModifiers(modifiers, ['cmd'])
    const shiftCmd = strictCheckModifiers(modifiers, ['shift', 'cmd'])
    switch (key) {
      case 's':
      case 'enter':
        if (cmd) {
          clearTimeout(this.autoSaveTimer!)
          this.setState(
            (state) => {
              const rawOffset = cursorPositionToRawOffset(state.code, state.cursorPosition)
              const prettierResult = applyPrettier(state.code, true, rawOffset)
              const newCursorOffset = rawOffsetToCursorPosition(
                prettierResult.formatted,
                prettierResult.cursorOffset,
              )
              return {
                code: prettierResult.formatted,
                cursorPosition: newCursorOffset,
              }
            },
            () => {
              this.save(this.state.code, true, {
                message: 'File saved and formatted',
                level: 'SUCCESS',
              })
            },
          )
        }
        break
      case 'z':
        if (cmd || shiftCmd) {
          event.stopPropagation()
        }
        break
      default:
        break
    }
  }

  onChange = (newValue: string) => {
    this.setState({
      dirty: true,
      code: newValue,
    })
    if (this.props.autoSave) {
      this.scheduleAutoSave()
    }
    this.props.onChange(newValue)
  }

  onChangeCursorPosition = (cursorPosition: CursorPosition, shouldChangeSelection: boolean) => {
    if (!cursorPositionsEqual(this.state.cursorPosition, cursorPosition)) {
      this.setState({
        cursorPosition: cursorPosition,
      })
      if (shouldChangeSelection) {
        this.props.onSelect(cursorPosition.line, cursorPosition.column)
      }
      this.props.saveCursorPosition(cursorPosition)
    }
  }

  render() {
    return (
      <div
        className='code-editor'
        style={{
          flexGrow: 1,
          display: 'flex',
          justifyContent: 'stretch',
          alignItems: 'stretch',
          overflowY: 'scroll',
          position: 'relative',
          // required to accurately position monaco-editor-root
        }}
        onKeyDown={this.onKeyDown}
        onFocus={this.props.onFocus}
        tabIndex={-1}
      >
        {this.props.enabled ? (
          <React.Fragment>
            <div style={{ position: 'relative', flexGrow: 1 }}>
              <MonacoWrapper
                value={this.state.code}
                filename={this.props.name}
                tabSize={2}
                options={{
                  automaticLayout: true,
                  formatOnType: true,
                  formatOnPaste: true,
                  fixedOverflowWidgets: true,
                  minimap: {
                    enabled: false,
                  },
                  readOnly: this.props.readOnly,
                }}
                onChange={this.onChange}
                onChangeFromProps={this.props.onChangeFromProps}
                onChangeCursorPosition={this.onChangeCursorPosition}
                saveCursorPosition={this.props.saveCursorPosition}
                onOpenFile={this.props.onOpenFile}
                npmTypeDefinitions={this.props.npmTypeDefinitions}
                cursorPosition={this.state.cursorPosition}
                projectContents={this.props.projectContents}
                errorMessages={this.props.errorsForFile}
                workers={this.props.workers}
                selectedViews={this.props.selectedViews}
                selectedViewsBounds={this.props.selectedViewsBounds}
                highlightedViewsBounds={this.props.highlightedViewsBounds}
                onHover={this.props.onHover}
                selectedTheme={this.props.codeEditorTheme}
              />
            </div>
            <CodeEditorTabPane
              errorMessages={this.props.allErrors ?? []}
              onOpenFile={this.props.onOpenFile}
              canvasConsoleLogs={this.props.canvasConsoleLogs}
            />
          </React.Fragment>
        ) : null}
      </div>
    )
  }
}

function initCursorPosition(cursorPosition: CursorPosition | null) {
  return cursorPosition == null
    ? {
        line: 0,
        column: 0,
      }
    : cursorPosition
}
