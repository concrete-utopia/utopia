import { Component as ReactComponent, RefObject } from 'react'
import * as React from 'react'
import * as R from 'ramda'

import * as EditorActions from './actions/actions'
import { EditorDispatch, EditorAction } from './action-types'

import {
  Editor as DraftEditor,
  EditorState as DraftEditorState,
  convertToRaw,
  RawDraftContentState,
  getDefaultKeyBinding,
  RichUtils,
  SelectionState,
  DraftHandleValue,
} from 'draft-js'
import Keyboard from '../../utils/keyboard'
import { InstancePath } from '../../core/shared/project-file-types'
import Utils from '../../utils/utils'
import { WindowPoint, CanvasPoint } from '../../core/shared/math-utils'
import {
  createContentState,
  getNewSizeFromContent,
  getDomNodeOfDraftJSBlock,
  createDraftStyles,
  draftContentToJsxAttributeValue,
} from '../../utils/draft-utils'
import * as PP from '../../core/shared/property-path'
import { jsxAttributeValue } from '../../core/shared/element-template'
import { TextSizing } from 'utopia-api'

export interface TextEditorProps {
  target: InstancePath
  dispatch: EditorDispatch
  triggerMousePosition: WindowPoint | null // stores the original mouse event which triggered the selection/editing of this text (if there is one)
  text: RawDraftContentState | string
  style: any
  className: string
  rawTextStyle: any
  textSizing: TextSizing
  scale: number
  deleteWhenEmpty: boolean
  offset: CanvasPoint
}

export interface TextEditorState {
  editorState: DraftEditorState
}

let currentEditor: TextEditor | null = null

export function focusTextEditorIfPresent() {
  if (currentEditor != null) {
    currentEditor.focusDraftEditor()
  }
}

export function closeTextEditorIfPresent() {
  if (currentEditor != null) {
    currentEditor.closeEditor()
  }
}

export class TextEditor extends ReactComponent<TextEditorProps, TextEditorState> {
  private draftEditor: RefObject<DraftEditor>
  private styles: any
  private customStyleFn: any
  private resizeRequestId: number | null = null
  private closeTriggeredAlready: boolean = false

  constructor(props: TextEditorProps) {
    super(props)
    const { styles, customStyleFn } = createDraftStyles()
    this.draftEditor = React.createRef()
    this.styles = styles
    this.customStyleFn = customStyleFn.bind(this)
    const initialContentState = createContentState(props.text)
    this.state = {
      editorState: DraftEditorState.moveFocusToEnd(
        DraftEditorState.createWithContent(initialContentState),
      ),
    }
  }

  componentDidMount() {
    currentEditor = this

    if (this.draftEditor.current != null) {
      this.draftEditor.current.focus()
    }

    if (this.props.triggerMousePosition) {
      this.positionDraftJSCursorToPoint(this.props.triggerMousePosition)
    } else {
      this.selectAll(this.state.editorState)
    }

    // autosize initially to make sure size is right
    if (this.isAutoSizing() && this.draftEditor.current != null) {
      this.autoResizeFromContent()
    }
  }

  focusDraftEditor() {
    if (this.draftEditor != null && this.draftEditor.current != null) {
      this.draftEditor.current.focus()
    }
  }

  onSubmitValue = (editorState: DraftEditorState) => {
    this.setState({ editorState: editorState })
    this.props.dispatch(
      [
        EditorActions.transientActions([
          EditorActions.setProp_UNSAFE(
            this.props.target,
            PP.create(['text']),
            draftContentToJsxAttributeValue(editorState.getCurrentContent()),
          ),
        ]),
      ],
      'canvas',
    )

    if (this.isAutoSizing()) {
      if (this.resizeRequestId != null) {
        window.cancelAnimationFrame(this.resizeRequestId)
      }
      this.resizeRequestId = window.requestAnimationFrame(() => this.autoResizeFromContent())
    }
  }

  selectAll = (editorState: DraftEditorState) => {
    const blocks = editorState.getCurrentContent().getBlockMap()
    const firstBlockKey = blocks.first().getKey()
    const lastBlock = blocks.last()
    const lastBlockKey = lastBlock.getKey()
    const lastBlockLength = lastBlock.getLength()

    const selection = new SelectionState({
      anchorKey: firstBlockKey,
      anchorOffset: 0,
      focusKey: lastBlockKey,
      focusOffset: lastBlockLength,
      hasFocus: true,
      isBackward: false,
    })

    this.setState({
      editorState: DraftEditorState.forceSelection(editorState, selection),
    })
  }

  positionDraftJSCursorToPoint = (p: WindowPoint) => {
    // first we have to find which draft-js block is under the point
    const block = this.getBlockUnderPoint(p)

    if (block != null) {
      const blockKey = block.getKey()

      // if we don't have document.caretRangeFromPoint (not standard) then we give up, cursor will not
      // be positioned, but it is not a big deal anyway
      if (document.caretRangeFromPoint == null) {
        return
      }

      // caretRange will contain the character position inside the block
      const caretRange = document.caretRangeFromPoint(p.x, p.y)

      const selection = new SelectionState({
        anchorKey: blockKey,
        anchorOffset: caretRange.startOffset,
        focusKey: blockKey,
        focusOffset: caretRange.startOffset,
        hasFocus: true,
        isBackward: false,
      })

      this.setState((prevState) => ({
        editorState: DraftEditorState.forceSelection(prevState.editorState, selection),
      }))
    }
  }

  getBlockUnderPoint = (p: WindowPoint) => {
    const rectContains = (rect: ClientRect | DOMRect, point: WindowPoint) =>
      point.x > rect.left && point.x < rect.right && point.y > rect.top && point.y < rect.bottom
    const draftEditor = Utils.forceNotNull(`DraftEditor ref is missing`, this.draftEditor.current)

    for (let block of this.state.editorState.getCurrentContent().getBlocksAsArray()) {
      const blockNode = getDomNodeOfDraftJSBlock(block, 'div', draftEditor)
      if (blockNode != null) {
        const blockRect = blockNode.getBoundingClientRect()
        if (!rectContains(blockRect, p)) {
          continue
        }
        return block
      }
    }
    return null
  }

  autoResizeFromContent = () => {
    const draftEditor = Utils.forceNotNull(`DraftEditor ref is missing`, this.draftEditor.current)
    const size = getNewSizeFromContent(
      this.state.editorState.getCurrentContent(),
      this.props.scale,
      draftEditor,
    )
    if (size.width != null && size.height != null) {
      this.props.dispatch(this.getAutoResizeActions(size.width, size.height), 'canvas')
    }
  }

  getAutoResizeActions = (width: number, height: number) => {
    return [EditorActions.updateFrameDimensions(this.props.target, width, height)]
  }

  isAutoSizing = () => {
    return this.props.textSizing === 'auto'
  }

  componentWillUnmount() {
    this.closeEditor()
  }

  closeEditor = () => {
    if (!this.closeTriggeredAlready) {
      this.closeTriggeredAlready = true
      let actions: EditorAction[] = [EditorActions.closeTextEditor()]
      if (
        this.props.deleteWhenEmpty &&
        this.state.editorState.getCurrentContent().getPlainText().length === 0
      ) {
        actions.push(EditorActions.deleteView(this.props.target))
      } else {
        const content = this.state.editorState.getCurrentContent()
        actions.push(
          EditorActions.setProp_UNSAFE(
            this.props.target,
            PP.create(['text']),
            draftContentToJsxAttributeValue(content),
          ),
        )
      }
      this.props.dispatch(actions, 'canvas')
    }

    currentEditor = null
  }

  toRaw() {
    return convertToRaw(this.state.editorState.getCurrentContent())
  }

  getCurrentFontSize(editorState: DraftEditorState): number {
    return Number(this.styles.fontSize.current(editorState)) || this.props.rawTextStyle.font.size
  }

  getCurrentLineHeight(editorState: DraftEditorState): number {
    return Number(this.styles.lineHeight.current(editorState)) || this.props.rawTextStyle.lineHeight
  }

  getCurrentLetterSpacing(editorState: DraftEditorState): number {
    return (
      Number(this.styles.letterSpacing.current(editorState)) ||
      this.props.rawTextStyle.letterSpacing
    )
  }

  // keyboard shortcuts
  keyBindingFn = (event: any): string | null => {
    const { character, modifiers } = Keyboard.keyFromEvent(event)
    if (character === 'enter' && R.contains('cmd', modifiers)) {
      return 'exit-text-edit'
    }
    if (character === 'l' && R.contains('cmd', modifiers) && R.contains('shift', modifiers)) {
      return 'align-left'
    }
    if (character === 'e' && R.contains('cmd', modifiers) && R.contains('shift', modifiers)) {
      return 'align-center'
    }
    if (character === 'r' && R.contains('cmd', modifiers) && R.contains('shift', modifiers)) {
      return 'align-right'
    }
    if (character === 'u' && R.contains('cmd', modifiers) && R.contains('shift', modifiers)) {
      return 'toggle-unordered-list'
    }
    if (character === 'o' && R.contains('cmd', modifiers) && R.contains('shift', modifiers)) {
      return 'toggle-ordered-list'
    }
    if (character === 'comma' && R.contains('cmd', modifiers) && R.contains('shift', modifiers)) {
      return 'decrease-font-size'
    }
    if (character === 'period' && R.contains('cmd', modifiers) && R.contains('shift', modifiers)) {
      return 'increase-font-size'
    }
    // up, down, left and right shortcuts need to call handleKeyCommand manually
    if (character === 'up' && R.contains('alt', modifiers)) {
      event.preventDefault()
      return this.handleKeyCommand('increase-line-height', this.state.editorState)
    }
    if (character === 'down' && R.contains('alt', modifiers)) {
      event.preventDefault()
      return this.handleKeyCommand('decrease-line-height', this.state.editorState)
    }
    return getDefaultKeyBinding(event)
  }

  handleKeyCommand = (command: string, editorState: DraftEditorState): DraftHandleValue => {
    switch (command) {
      case 'exit-text-edit':
        this.closeEditor()
        return 'handled'
      // alignment is now for the whole text not just a paragraph
      case 'align-left':
        if (!this.isAutoSizing()) {
          this.props.dispatch(
            [
              EditorActions.transientActions([
                EditorActions.setProp_UNSAFE(
                  this.props.target,
                  PP.create(['textstyle', 'textAlignment']),
                  jsxAttributeValue('left'),
                ),
              ]),
            ],
            'canvas',
          )
        }
        return 'handled'
      case 'align-center':
        if (!this.isAutoSizing()) {
          this.props.dispatch(
            [
              EditorActions.transientActions([
                EditorActions.setProp_UNSAFE(
                  this.props.target,
                  PP.create(['textstyle', 'textAlignment']),
                  jsxAttributeValue('center'),
                ),
              ]),
            ],
            'canvas',
          )
        }
        return 'handled'
      case 'align-right':
        if (!this.isAutoSizing()) {
          this.props.dispatch(
            [
              EditorActions.transientActions([
                EditorActions.setProp_UNSAFE(
                  this.props.target,
                  PP.create(['textstyle', 'textAlignment']),
                  jsxAttributeValue('right'),
                ),
              ]),
            ],
            'canvas',
          )
        }
        return 'handled'
      case 'toggle-unordered-list':
        this.setState((prevState) => ({
          editorState: RichUtils.toggleBlockType(prevState.editorState, 'unordered-list-item'),
        }))
        return 'handled'
      case 'toggle-ordered-list':
        this.setState((prevState) => ({
          editorState: RichUtils.toggleBlockType(prevState.editorState, 'ordered-list-item'),
        }))
        return 'handled'
      case 'increase-font-size':
        this.setState({
          editorState: this.styles.fontSize.add(
            editorState,
            this.getCurrentFontSize(editorState) + 1,
          ),
        })
        return 'handled'
      case 'decrease-font-size':
        this.setState({
          editorState: this.styles.fontSize.add(
            editorState,
            Math.max(this.getCurrentFontSize(editorState), 2) - 1,
          ),
        })
        return 'handled'
      case 'increase-line-height':
        this.setState({
          editorState: this.styles.lineHeight.add(
            editorState,
            this.getCurrentLineHeight(editorState) + 0.1,
          ),
        })
        return 'handled'
      case 'decrease-line-height':
        this.setState({
          editorState: this.styles.lineHeight.add(
            editorState,
            Math.max(this.getCurrentLineHeight(editorState), 0.1) - 0.1,
          ),
        })
        return 'handled'
      case 'increase-letter-spacing':
        this.setState({
          editorState: this.styles.letterSpacing.add(
            editorState,
            this.getCurrentLetterSpacing(editorState) + 1,
          ),
        })
        return 'handled'
      case 'decrease-letter-spacing':
        this.setState({
          editorState: this.styles.letterSpacing.add(
            editorState,
            this.getCurrentLetterSpacing(editorState) - 1,
          ),
        })
        return 'handled'
      default:
        // default shortcuts
        const newState = RichUtils.handleKeyCommand(editorState, command)
        if (newState) {
          this.onSubmitValue(newState)
          return 'handled'
        }
        return 'not-handled'
    }
  }

  renderClickCatcher = () => {
    return (
      <div
        id={'canvas-text-click-catcher'}
        key={'canvas-text-click-catcher'}
        style={{
          left: 0,
          top: 0,
          bottom: 0,
          right: 0,
          position: 'fixed',
        }}
        onMouseDown={this.closeEditor}
      />
    )
  }

  render() {
    const DraftEditorAny = DraftEditor as any // Forgive me. This is because we're using a fork of draft.js and haven't updated the types

    return (
      <>
        {this.renderClickCatcher()}
        <div
          key={'text-editor-wrapper'}
          style={{
            left: this.props.offset.x,
            top: this.props.offset.y,
            position: 'absolute',
            zoom: `${(1 / this.props.scale) * 100}%`,
            transformOrigin: `top left`,
            transform: `scale(${this.props.scale})`,
          }}
        >
          <div
            key={'text-editor'}
            className={'canvas-text canvas-text-editable ' + this.props.className}
            style={this.props.style}
            onKeyDown={(e) => e.stopPropagation()}
            onKeyUp={(e) => e.stopPropagation()}
            onMouseDown={(e) => e.stopPropagation()}
            onMouseUp={(e) => e.stopPropagation()}
            onMouseMove={(e) => e.stopPropagation()}
          >
            <DraftEditorAny
              key={'drafteditor'}
              readOnly={false}
              ref={this.draftEditor}
              editorState={this.state.editorState}
              onChange={this.onSubmitValue}
              keyBindingFn={this.keyBindingFn}
              handleKeyCommand={this.handleKeyCommand}
              customStyleFn={this.customStyleFn}
              textAlignment={
                this.props.style.textAlign != null ? this.props.style.textAlign : 'left'
              }
              whiteSpaceStyle={this.isAutoSizing() ? 'nowrap' : 'pre-wrap'}
            />
          </div>
        </div>
      </>
    )
  }
}
