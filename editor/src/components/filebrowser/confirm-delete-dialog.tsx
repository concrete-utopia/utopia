/** @jsx jsx */
import { jsx } from '@emotion/core'
import * as React from 'react'
import { FormButton } from 'uuiui'
import { Dialog } from 'uuiui'
import { EditorDispatch } from '../editor/action-types'
import * as EditorActions from '../editor/actions/action-creators'

interface ConfirmDeleteDialogProps {
  dispatch: EditorDispatch
  filePath: string
}

export const ConfirmDeleteDialog: React.FunctionComponent<ConfirmDeleteDialogProps> = (props) => {
  const hide = React.useCallback(() => {
    props.dispatch([EditorActions.hideModal()], 'everyone')
  }, [props])
  return (
    <Dialog
      title='Delete file'
      content={<DialogBody {...props} />}
      defaultButton={<AcceptButton {...props} />}
      secondaryButton={<CancelButton {...props} />}
      closeCallback={hide}
    />
  )
}

const DialogBody: React.FunctionComponent<ConfirmDeleteDialogProps> = (props) => (
  <React.Fragment>
    <p>
      Are you sure you want to delete <span>{props.filePath}</span>?
    </p>
    <p>Deleted files are permanently removed.</p>
  </React.Fragment>
)

const AcceptButton: React.FunctionComponent<ConfirmDeleteDialogProps> = (props) => {
  const clickButton = React.useCallback(() => {
    props.dispatch(
      [EditorActions.deleteFile(props.filePath), EditorActions.hideModal()],
      'everyone',
    )
  }, [props])

  return (
    <FormButton primary danger onClick={clickButton}>
      Delete
    </FormButton>
  )
}

const CancelButton: React.FunctionComponent<ConfirmDeleteDialogProps> = (props) => {
  const clickButton = React.useCallback(() => {
    props.dispatch([EditorActions.hideModal()], 'everyone')
  }, [props])

  return <FormButton onClick={clickButton}>Cancel</FormButton>
}
