import React from 'react'
import { Component, CSSProperties } from 'react'
import { ElementPath } from '../../../core/shared/project-file-types'
import { EditorDispatch } from '../../editor/action-types'
import * as EditorActions from '../../editor/actions/action-creators'
import * as EP from '../../../core/shared/element-path'
import { renameComponent } from '../actions'
import { StringInput, flexRowStyle, colorTheme, Icons } from '../../../uuiui'
import {
  isRegularNavigatorEntry,
  navigatorEntriesEqual,
  NavigatorEntry,
} from '../../../components/editor/store/editor-state'

interface ItemLabelProps {
  testId: string
  dispatch: EditorDispatch
  target: NavigatorEntry
  isDynamic: boolean
  selected: boolean
  name: string
  suffix?: string
  inputVisible: boolean
  style?: CSSProperties
}

interface ItemLabelState {
  name: string
  target: NavigatorEntry
}

export class ItemLabel extends Component<ItemLabelProps, ItemLabelState> {
  elementRef: HTMLInputElement | null = null
  constructor(props: ItemLabelProps) {
    super(props)
    this.state = {
      name: this.props.name,
      target: this.props.target,
    }
  }

  static getDerivedStateFromProps(
    props: ItemLabelProps,
    state: ItemLabelState,
  ): ItemLabelState | null {
    if (props.target === state.target || navigatorEntriesEqual(props.target, state.target)) {
      return null
    } else {
      return {
        name: props.name,
        target: props.target,
      }
    }
  }

  componentDidUpdate(prevProps: ItemLabelProps, prevState: ItemLabelState) {
    if (!prevProps.inputVisible && this.props.inputVisible && this.elementRef != null) {
      this.elementRef.focus()
      this.elementRef.select()
    }
  }

  cancelRename() {
    this.setState({
      name: this.props.name,
    })
    this.props.dispatch([EditorActions.setNavigatorRenamingTarget(null)], 'leftpane')
  }

  renameComponent() {
    if (isRegularNavigatorEntry(this.props.target)) {
      // if the name would be the same, or if the new name would be empty, just cancel
      if (this.props.name === this.state.name) {
        this.cancelRename()
      } else {
        const nameIsBlank = this.state.name.trim().length === 0
        const action = renameComponent(
          this.props.target.elementPath,
          nameIsBlank ? null : this.state.name,
        )
        this.props.dispatch([action, EditorActions.setNavigatorRenamingTarget(null)], 'leftpane')
      }
    } else {
      this.cancelRename()
    }
  }

  renderDefaultLabel() {
    const value =
      this.props.suffix == null ? this.props.name : `${this.props.name} ${this.props.suffix}`

    return (
      <div
        key='item-label'
        style={{
          backgroundColor: 'transparent',
          paddingTop: 3,
          paddingBottom: 3,
          marginLeft: 6,
          overflow: 'hidden',
          textOverflow: 'ellipsis',
          whiteSpace: 'nowrap',
          display: 'flex',
          flexDirection: 'row',
          alignItems: 'center',
          gap: 6,
          fontWeight: this.props.target.type === 'CONDITIONAL_CLAUSE' ? 600 : undefined,
          color: this.props.target.type === 'CONDITIONAL_CLAUSE' ? colorTheme.fg7.value : undefined,
          textTransform: this.props.target.type === 'CONDITIONAL_CLAUSE' ? 'uppercase' : undefined,
        }}
        onDoubleClick={(event) => {
          if (!this.props.isDynamic && event.altKey && isRegularNavigatorEntry(this.props.target)) {
            this.props.dispatch(
              [EditorActions.setNavigatorRenamingTarget(this.props.target.elementPath)],
              'leftpane',
            )
          }
        }}
      >
        {value}
      </div>
    )
  }

  inputLabelUpdateRef = (ref: HTMLInputElement) => {
    this.elementRef = ref
  }
  inputLabelKeyDown = (event: React.KeyboardEvent<HTMLInputElement>) => {
    if (event.key == 'Enter') {
      this.renameComponent()
    }
    if (event.key == 'Escape') {
      this.cancelRename()
    }
  }

  inputLabelBlur = () => this.renameComponent()

  inputLabelChange = (event: React.FormEvent<HTMLElement>) => {
    this.setState({ name: (event.target as any).value })
  }

  renderInputLabel() {
    return (
      <div key='item-rename-label'>
        <StringInput
          key='item-rename-input'
          testId={this.props.testId}
          className='rename-input-field'
          ref={this.inputLabelUpdateRef}
          type='text'
          value={this.state.name}
          onKeyDown={this.inputLabelKeyDown}
          onBlur={this.inputLabelBlur}
          onChange={this.inputLabelChange}
        />
      </div>
    )
  }

  render() {
    return (
      <div
        key='item-label-container'
        className='item-label-container'
        style={{
          ...this.props.style,
          ...flexRowStyle,
          fontSize: 11,
          fontStyle: this.props.isDynamic ? 'italic' : 'normal',
        }}
      >
        {this.props.inputVisible ? this.renderInputLabel() : this.renderDefaultLabel()}
      </div>
    )
  }
}
