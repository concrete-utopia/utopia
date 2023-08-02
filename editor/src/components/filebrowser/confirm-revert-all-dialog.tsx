/** @jsxRuntime classic */
/** @jsx jsx */
import { jsx } from '@emotion/react'
import React from 'react'
import { revertAllGithubFiles } from '../../core/shared/github/helpers'
import { Dialog, FormButton } from '../../uuiui'
import type { EditorDispatch } from '../editor/action-types'
import * as EditorActions from '../editor/actions/action-creators'
import { Substores, useEditorState } from '../editor/store/store-hook'

interface ConfirmRevertAllDialogProps {
  dispatch: EditorDispatch
}

export const ConfirmRevertAllDialogProps: React.FunctionComponent<
  React.PropsWithChildren<ConfirmRevertAllDialogProps>
> = (props) => {
  const hide = React.useCallback(() => {
    props.dispatch([EditorActions.hideModal()], 'everyone')
  }, [props])
  return (
    <Dialog
      title='Revert all changes'
      content={<DialogBody {...props} />}
      defaultButton={<AcceptButton {...props} />}
      secondaryButton={<CancelButton {...props} />}
      closeCallback={hide}
    />
  )
}

const DialogBody: React.FunctionComponent<
  React.PropsWithChildren<ConfirmRevertAllDialogProps>
> = () => (
  <React.Fragment>
    <p>
      Are you sure you want to revert <strong>all</strong> changes?
    </p>
    <p>Your local changes will be replaced with the original contents and cannot be recovered.</p>
  </React.Fragment>
)

const AcceptButton: React.FunctionComponent<
  React.PropsWithChildren<ConfirmRevertAllDialogProps>
> = (props) => {
  const branchContents = useEditorState(
    Substores.restOfEditor,
    (store) => store.editor.branchOriginContents,
    'branch contents',
  )
  const clickButton = React.useCallback(() => {
    const actions = revertAllGithubFiles(branchContents)
    props.dispatch([...actions, EditorActions.hideModal()], 'everyone')
  }, [props, branchContents])

  return (
    <FormButton primary danger onClick={clickButton}>
      Revert
    </FormButton>
  )
}

const CancelButton: React.FunctionComponent<
  React.PropsWithChildren<ConfirmRevertAllDialogProps>
> = (props) => {
  const clickButton = React.useCallback(() => {
    props.dispatch([EditorActions.hideModal()], 'everyone')
  }, [props])

  return <FormButton onClick={clickButton}>Cancel</FormButton>
}
