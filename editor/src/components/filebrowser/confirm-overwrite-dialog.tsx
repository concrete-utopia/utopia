/** @jsxRuntime classic */
/** @jsx jsx */
import { jsx } from '@emotion/react'
import React from 'react'
import { FileResult } from '../../core/shared/file-utils'
import { Dialog, FormButton } from '../../uuiui'
import { EditorDispatch } from '../editor/action-types'
import * as EditorActions from '../editor/actions/action-creators'
import { fileResultUploadAction } from '../editor/image-insert'
import { CancelButton } from './cancel-button'

interface ConfirmOverwriteDialogProps {
  dispatch: EditorDispatch
  fileResult: FileResult
  targetPath: string
}

export const ConfirmOverwriteDialog: React.FunctionComponent<
  React.PropsWithChildren<ConfirmOverwriteDialogProps>
> = (props) => {
  const hide = React.useCallback(() => {
    props.dispatch([EditorActions.hideModal()], 'everyone')
  }, [props])
  return (
    <Dialog
      title='Overwrite file'
      content={<DialogBody {...props} />}
      defaultButton={<AcceptButton {...props} />}
      secondaryButton={<CancelButton {...props} />}
      closeCallback={hide}
    />
  )
}

const DialogBody: React.FunctionComponent<React.PropsWithChildren<ConfirmOverwriteDialogProps>> = (
  props,
) => (
  <React.Fragment>
    <p>
      Are you sure you want to overwrite <span>{props.targetPath}</span>?
    </p>
    <p>Overwritten files are not recoverable.</p>
  </React.Fragment>
)

const AcceptButton: React.FunctionComponent<
  React.PropsWithChildren<ConfirmOverwriteDialogProps>
> = (props) => {
  const clickButton = React.useCallback(() => {
    props.dispatch(
      [fileResultUploadAction(props.fileResult, props.targetPath, true), EditorActions.hideModal()],
      'everyone',
    )
  }, [props])

  return (
    <FormButton primary danger onClick={clickButton}>
      Overwrite
    </FormButton>
  )
}
