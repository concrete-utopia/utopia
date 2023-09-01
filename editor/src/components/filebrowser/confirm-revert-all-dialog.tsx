/** @jsxRuntime classic */
/** @jsx jsx */
import { jsx } from '@emotion/react'
import React from 'react'
import { revertAllGithubFiles } from '../../core/shared/github/helpers'
import { Dialog, FormButton } from '../../uuiui'
import type { EditorDispatch } from '../editor/action-types'
import * as EditorActions from '../editor/actions/action-creators'
import { Substores, useEditorState, useRefEditorState } from '../editor/store/store-hook'

interface ConfirmRevertAllDialogProps {
  dispatch: EditorDispatch
}

export const ConfirmRevertAllDialog: React.FunctionComponent<
  React.PropsWithChildren<ConfirmRevertAllDialogProps>
> = (props) => {
  const hide = React.useCallback(() => {
    props.dispatch([EditorActions.hideModal()], 'everyone')
  }, [props])
  const [dialogDisabled, setDialogDisabled] = React.useState(false)
  return (
    <Dialog
      title='Revert all changes'
      content={<DialogBody {...props} />}
      defaultButton={
        <AcceptButton
          {...props}
          dialogDisabled={dialogDisabled}
          setDialogDisabled={setDialogDisabled}
        />
      }
      secondaryButton={
        <CancelButton
          {...props}
          dialogDisabled={dialogDisabled}
          setDialogDisabled={setDialogDisabled}
        />
      }
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

interface ConfirmRevertAllAcceptButtonProps extends ConfirmRevertAllDialogProps {
  dialogDisabled: boolean
  setDialogDisabled: (disabled: boolean) => void
}

const AcceptButton: React.FunctionComponent<
  React.PropsWithChildren<ConfirmRevertAllAcceptButtonProps>
> = (props) => {
  const workersRef = useRefEditorState((state) => {
    return state.workers
  })
  const branchContents = useEditorState(
    Substores.restOfEditor,
    (store) => store.editor.branchOriginContents,
    'branch contents',
  )
  const clickButton = React.useCallback(() => {
    if (!props.dialogDisabled) {
      props.setDialogDisabled(true)
      void revertAllGithubFiles(workersRef.current, branchContents)
        .then((actions) => {
          props.dispatch([...actions, EditorActions.hideModal()], 'everyone')
        })
        .finally(() => {
          props.setDialogDisabled(false)
        })
    }
  }, [workersRef, props, branchContents])

  return (
    <FormButton primary danger onClick={clickButton} disabled={props.dialogDisabled}>
      Revert
    </FormButton>
  )
}

interface ConfirmRevertAllCancelButtonProps extends ConfirmRevertAllDialogProps {
  dialogDisabled: boolean
  setDialogDisabled: (disabled: boolean) => void
}

const CancelButton: React.FunctionComponent<
  React.PropsWithChildren<ConfirmRevertAllCancelButtonProps>
> = (props) => {
  const clickButton = React.useCallback(() => {
    if (!props.dialogDisabled) {
      props.dispatch([EditorActions.hideModal()], 'everyone')
    }
  }, [props])

  return (
    <FormButton onClick={clickButton} disabled={props.dialogDisabled}>
      Cancel
    </FormButton>
  )
}
