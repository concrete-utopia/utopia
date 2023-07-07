/** @jsxRuntime classic */
/** @jsx jsx */
import { jsx } from '@emotion/react'
import React from 'react'
import { disconnectGithubProjectActions } from '../../core/shared/github/helpers'
import { Dialog, FormButton } from '../../uuiui'
import type { EditorDispatch } from '../editor/action-types'
import * as EditorActions from '../editor/actions/action-creators'

interface ConfirmDisconnectBranchProps {
  dispatch: EditorDispatch
  branchName: string
}

export const ConfirmDisconnectBranchDialog: React.FunctionComponent<
  React.PropsWithChildren<ConfirmDisconnectBranchProps>
> = (props) => {
  const hide = React.useCallback(() => {
    props.dispatch([EditorActions.hideModal()], 'everyone')
  }, [props])
  return (
    <Dialog
      title='Disconnect project from branch'
      content={<DialogBody {...props} />}
      defaultButton={<AcceptButton {...props} />}
      secondaryButton={<CancelButton {...props} />}
      closeCallback={hide}
    />
  )
}

const DialogBody: React.FunctionComponent<React.PropsWithChildren<ConfirmDisconnectBranchProps>> = (
  props,
) => (
  <React.Fragment>
    <p>
      Are you sure you want to disconnect this project from the Github branch{' '}
      <strong>“{props.branchName}”</strong>?
    </p>
    <p>
      Your project will not be affected by this, but won't be able to save to or load from Github
      until you reconnect to a branch.
    </p>
  </React.Fragment>
)

const AcceptButton: React.FunctionComponent<
  React.PropsWithChildren<ConfirmDisconnectBranchProps>
> = (props) => {
  const clickButton = React.useCallback(() => {
    const actions = disconnectGithubProjectActions()
    props.dispatch(actions)
  }, [props])

  return (
    <FormButton primary danger onClick={clickButton}>
      Disconnect
    </FormButton>
  )
}

const CancelButton: React.FunctionComponent<
  React.PropsWithChildren<ConfirmDisconnectBranchProps>
> = (props) => {
  const clickButton = React.useCallback(() => {
    props.dispatch([EditorActions.hideModal()], 'everyone')
  }, [props])

  return <FormButton onClick={clickButton}>Cancel</FormButton>
}
