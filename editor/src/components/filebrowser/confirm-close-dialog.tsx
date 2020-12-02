/** @jsx jsx */
import { jsx } from '@emotion/core'
import * as React from 'react'
import { FormButton } from 'uuiui'
import { Dialog } from 'uuiui'
import { EditorDispatch } from '../editor/action-types'
import * as EditorActions from '../editor/actions/action-creators'
import { EditorTab, isOpenFileTab } from '../editor/store/editor-tabs'

interface ConfirmCloseDialogProps {
  dispatch: EditorDispatch
  editorTab: EditorTab
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
  if (isOpenFileTab(props.editorTab)) {
    message = `Do you want to save the changes you made to {props.editorTab.filename}?`
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
    props.dispatch(
      [EditorActions.closeEditorTab(props.editorTab), EditorActions.hideModal()],
      'everyone',
    )
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
