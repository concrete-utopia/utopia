/**
 * Copyright (c) 2015-present, Facebook, Inc.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */

import React from 'react'
import { ErrorMessage } from '../../core/shared/error-messages'
import utils from '../../utils/utils'
import CompileErrorContainer from './containers/CompileErrorContainer'
import { ErrorRecord } from './containers/RuntimeError'
import RuntimeErrorContainer from './containers/RuntimeErrorContainer'
import { overlayStyle } from './styles'

interface ErrorOverlayProps {
  currentBuildErrorRecords: ErrorMessage[]
  currentRuntimeErrorRecords: ErrorRecord[]
  onOpenFile: (path: string) => void
  overlayOffset: number
}

export const ReactErrorOverlay = React.memo(
  ({
    currentBuildErrorRecords,
    currentRuntimeErrorRecords,
    onOpenFile,
    overlayOffset,
  }: ErrorOverlayProps) => {
    if (currentBuildErrorRecords.length > 0) {
      return (
        <div style={overlayStyle(overlayOffset)}>
          <CompileErrorContainer
            currentBuildErrorRecords={currentBuildErrorRecords}
            editorHandler={utils.NO_OP}
            onOpenFile={onOpenFile}
          />
        </div>
      )
    }
    if (currentRuntimeErrorRecords.length > 0) {
      return (
        <div style={overlayStyle(overlayOffset)}>
          <RuntimeErrorContainer
            errorRecords={currentRuntimeErrorRecords}
            onOpenFile={onOpenFile}
            close={utils.NO_OP}
            editorHandler={utils.NO_OP}
          />
        </div>
      )
    }
    return null
  },
)
