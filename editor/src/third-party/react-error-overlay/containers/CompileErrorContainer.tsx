/**
 * Copyright (c) 2015-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

/* @flow */
/** @jsxRuntime classic */
/** @jsx jsx */
import { jsx } from '@emotion/react'
import React from 'react'
import { PureComponent } from 'react'
import ErrorOverlay from '../components/ErrorOverlay'
import Footer from '../components/Footer'
import Header from '../components/Header'
import CodeBlock from '../components/CodeBlock'
import generateAnsiHTML from '../utils/generateAnsiHTML'
import { ErrorLocation } from '../utils/parseCompileError'
import { ErrorMessage } from '../../../core/shared/error-messages'
import { CursorPosition } from '../../../components/code-editor/code-editor-utils'
//TODO: switch to functional component and make use of 'useColorTheme':
import { colorTheme } from '../../../uuiui'

interface CompileErrorContainerProps {
  currentBuildErrorRecords: ErrorMessage[]
  editorHandler: (errorLoc: ErrorLocation) => void
  onOpenFile: (path: string, cursorPosition: CursorPosition | null) => void
}

class CompileErrorContainer extends PureComponent<CompileErrorContainerProps, {}> {
  render() {
    const { currentBuildErrorRecords, editorHandler, onOpenFile } = this.props

    return (
      <ErrorOverlay>
        <Header headerText={'Errors during build'} />
        {currentBuildErrorRecords != null ? (
          currentBuildErrorRecords.map((record, recordIndex) => {
            return (
              <div
                key={`compile-error-${recordIndex}`}
                css={{
                  marginBottom: 24,
                  padding: 8,
                  backgroundColor: colorTheme.neutralBackground.value,
                }}
              >
                <h3>
                  <span>
                    <a
                      onClick={() => {
                        const cursorPositon =
                          record.startLine == null || record.startColumn == null
                            ? null
                            : {
                                line: record.startLine,
                                column: record.startColumn,
                              }
                        onOpenFile(record.fileName, cursorPositon)
                      }}
                      style={{ cursor: 'pointer', color: colorTheme.dynamicBlue.value }}
                    >
                      {record.fileName} {record.startLine}:{record.startColumn}&nbsp;
                    </a>
                    <span>
                      {record.source}: {record.message}
                    </span>
                  </span>
                </h3>
                <div>
                  <CodeBlock main={true} codeHTML={generateAnsiHTML(record.codeSnippet)} />
                  <Footer
                    line1={`Error code: ${record.errorCode || 'N/A'} ( ${record.type},  ${
                      record.severity
                    })`}
                  />
                </div>
              </div>
            )
          })
        ) : (
          <span style={{ marginTop: 30 }}>
            There were errors, but we don't have any information about them. It's possible that's
            because of a bug or problem in our service, not your code. Try reloading the editor.
          </span>
        )}
      </ErrorOverlay>
    )
  }
}

export default CompileErrorContainer
