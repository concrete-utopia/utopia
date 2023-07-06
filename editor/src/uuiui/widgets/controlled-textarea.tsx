/** @jsxRuntime classic */
/** @jsx jsx */
import type { Interpolation } from '@emotion/react'
import { jsx } from '@emotion/react'
import React from 'react'

interface ControlledTextAreaProps {
  value: string
  onSubmitValue: (value: string) => void
  className?: string
  inputType: 'textarea' | 'input'
  onClickSelectsAll: boolean
  onBlurOnCmdReturn?: boolean
  disabled: boolean
  css?: Interpolation<any>
}

interface ControlledTextAreaState {
  value: string
  propsValue: string
}

export class ControlledTextArea extends React.Component<
  ControlledTextAreaProps,
  ControlledTextAreaState
> {
  input: null | HTMLTextAreaElement | HTMLInputElement = null

  constructor(props: ControlledTextAreaProps) {
    super(props)
    this.state = {
      value: props.value,
      propsValue: props.value,
    }
  }

  static getDerivedStateFromProps(
    props: ControlledTextAreaProps,
    state: ControlledTextAreaState,
  ): ControlledTextAreaState | null {
    if (props.value === state.propsValue) {
      return null
    } else {
      return {
        value: props.value,
        propsValue: props.value,
      }
    }
  }

  triggerSelect = () => {
    this.props.onClickSelectsAll && this.input != null ? this.input.select() : null
  }

  setRef = (input: null | HTMLTextAreaElement | HTMLInputElement) => {
    this.input = input
  }

  tagOnBlur = () => {
    if (this.props.value !== this.state.value) {
      this.props.onSubmitValue(this.state.value)
    }
  }

  tagOnChange = (
    event: React.ChangeEvent<HTMLTextAreaElement> | React.ChangeEvent<HTMLInputElement>,
  ) => {
    this.setState({
      value: event.target.value,
    })
  }

  tagOnKeyDown = (
    event: React.KeyboardEvent<HTMLTextAreaElement> | React.KeyboardEvent<HTMLInputElement>,
  ) => {
    if (
      event.key === 'Enter' &&
      this.input != null &&
      (this.props.onBlurOnCmdReturn !== true ||
        (this.props.onBlurOnCmdReturn && event.metaKey === true))
    ) {
      this.input.blur()
    }
    event.stopPropagation()
  }

  render() {
    const TagToRender = this.props.inputType
    return (
      <TagToRender
        autoCapitalize='off'
        autoComplete='off'
        autoCorrect='off'
        css={this.props.css}
        spellCheck={false}
        disabled={this.props.disabled}
        className={this.props.className}
        onClick={this.triggerSelect}
        ref={this.setRef}
        onBlur={this.tagOnBlur}
        onChange={this.tagOnChange}
        onKeyDown={this.tagOnKeyDown}
        value={this.state.value}
      />
    )
  }
}
