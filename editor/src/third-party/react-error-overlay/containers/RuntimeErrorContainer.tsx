/**
 * Copyright (c) 2015-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

/* @flow */
import React from 'react'
import { PureComponent } from 'react'
import ErrorOverlay from '../components/ErrorOverlay'
import CloseButton from '../components/CloseButton'
import NavigationBar from '../components/NavigationBar'
import RuntimeError from './RuntimeError'
import Footer from '../components/Footer'

import { ErrorRecord } from './RuntimeError'
import { ErrorLocation } from '../utils/parseCompileError'
import { CursorPosition } from '../../../components/code-editor/code-editor-utils'

type Props = {
  errorRecords: ErrorRecord[]
  close: () => void
  editorHandler: (errorLoc: ErrorLocation) => void
  onOpenFile: (path: string, cursorPosition: CursorPosition | null) => void
}

type State = {
  currentIndex: number
}

class RuntimeErrorContainer extends PureComponent<Props, State> {
  state = {
    currentIndex: 0,
  }

  previous = () => {
    this.setState((state, props) => ({
      currentIndex: state.currentIndex > 0 ? state.currentIndex - 1 : props.errorRecords.length - 1,
    }))
  }

  next = () => {
    this.setState((state, props) => ({
      currentIndex: state.currentIndex < props.errorRecords.length - 1 ? state.currentIndex + 1 : 0,
    }))
  }

  shortcutHandler = (key: string) => {
    if (key === 'Escape') {
      this.props.close()
    } else if (key === 'ArrowLeft') {
      this.previous()
    } else if (key === 'ArrowRight') {
      this.next()
    }
  }

  render() {
    const { errorRecords, close } = this.props
    const totalErrors = errorRecords.length
    if (totalErrors < this.state.currentIndex) {
      return null
    }
    return (
      <ErrorOverlay shortcutHandler={this.shortcutHandler}>
        <CloseButton close={close} />
        {totalErrors > 1 && (
          <NavigationBar
            currentError={this.state.currentIndex + 1}
            totalErrors={totalErrors}
            previous={this.previous}
            next={this.next}
          />
        )}
        <RuntimeError
          errorRecord={errorRecords[this.state.currentIndex]!}
          editorHandler={this.props.editorHandler}
          onOpenFile={this.props.onOpenFile}
        />
        <Footer
          line1='This screen is visible only in development. It will not appear if the app crashes in production.'
          line2='Open your browser’s developer console to further inspect this error.'
        />
      </ErrorOverlay>
    )
  }
}

export default RuntimeErrorContainer
