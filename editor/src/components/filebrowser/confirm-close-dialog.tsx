/** @jsx jsx */
import { jsx } from '@emotion/react'
import * as React from 'react'
import { Dialog, FormButton } from '../../uuiui'
import { EditorAction, EditorDispatch } from '../editor/action-types'
import * as EditorActions from '../editor/actions/action-creators'

interface ConfirmCloseDialogProps {
  dispatch: EditorDispatch
  currentDesignerFile: string | null
}

export const ConfirmCloseDialog: React.FunctionComponent<ConfirmCloseDialogProps> = (props) => {
  const close = React.useCallback(() => props.dispatch([EditorActions.hideModal()], 'everyone'), [
    props,
  ])
  return (
    <Dialog
      title='Close file'
      content={<DialogBody {...props} />}
      defaultButton={<SaveAndCloseButton {...props} />}
      secondaryButton={<CancelButton {...props} />}
      subduedButton={<DontSaveButton {...props} />}
      closeCallback={close}
    />
  )
}

const DialogBody: React.FunctionComponent<ConfirmCloseDialogProps> = (props) => {
  let message: string = `Do you want to save the changes you made?`
  if (props.currentDesignerFile != null) {
    message = `Do you want to save the changes you made to {props.currentDesignerFile}?`
  }
  return (
    <React.Fragment>
      <p>{message}</p>
      <p>Your changes will be lost if you don't save them.</p>
    </React.Fragment>
  )
}

const SaveAndCloseButton: React.FunctionComponent<ConfirmCloseDialogProps> = (props) => {
  const closeClick = React.useCallback(() => {
    props.dispatch([EditorActions.saveCurrentFile(), EditorActions.hideModal()], 'everyone')
  }, [props])
  return (
    <FormButton primary onClick={closeClick}>
      Save and Close
    </FormButton>
  )
}

const DontSaveButton: React.FunctionComponent<ConfirmCloseDialogProps> = (props) => {
  const dontSaveClick = React.useCallback(() => {
    let actions: Array<EditorAction> = []
    if (props.currentDesignerFile != null) {
      actions.push(EditorActions.closeDesignerFile(props.currentDesignerFile))
    }
    actions.push(EditorActions.hideModal())
    props.dispatch(actions, 'everyone')
  }, [props])

  return (
    <FormButton danger onClick={dontSaveClick}>
      Don't save
    </FormButton>
  )
}

const CancelButton: React.FunctionComponent<ConfirmCloseDialogProps> = (props) => {
  const cancelClick = React.useCallback(() => {
    props.dispatch([EditorActions.hideModal()], 'everyone')
  }, [props])
  return <FormButton onClick={cancelClick}>Cancel</FormButton>
}
