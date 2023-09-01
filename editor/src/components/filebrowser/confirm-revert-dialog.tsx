/** @jsxRuntime classic */
/** @jsx jsx */
import { jsx } from '@emotion/react'
import React, { useState } from 'react'
import type { GithubFileStatus } from '../../core/shared/github/helpers'
import { revertGithubFile } from '../../core/shared/github/helpers'
import { Dialog, FormButton } from '../../uuiui'
import type { EditorDispatch } from '../editor/action-types'
import * as EditorActions from '../editor/actions/action-creators'
import { Substores, useEditorState, useRefEditorState } from '../editor/store/store-hook'

interface ConfirmRevertDialogProps {
  dispatch: EditorDispatch
  filePath: string
  status: GithubFileStatus | null
}

export const ConfirmRevertDialog: React.FunctionComponent<
  React.PropsWithChildren<ConfirmRevertDialogProps>
> = (props) => {
  const hide = React.useCallback(() => {
    props.dispatch([EditorActions.hideModal()], 'everyone')
  }, [props])
  const [dialogDisabled, setDialogDisabled] = React.useState(false)
  return (
    <Dialog
      title='Revert changes'
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

const DialogBody: React.FunctionComponent<React.PropsWithChildren<ConfirmRevertDialogProps>> = (
  props,
) => (
  <React.Fragment>
    <p>
      Are you sure you want to revert <span>{props.filePath}</span>?
    </p>
    <p>Your local changes will be replaced with the original contents and cannot be recovered.</p>
  </React.Fragment>
)

interface ConfirmRevertAcceptButtonProps extends ConfirmRevertDialogProps {
  dialogDisabled: boolean
  setDialogDisabled: (disabled: boolean) => void
}

const AcceptButton: React.FunctionComponent<
  React.PropsWithChildren<ConfirmRevertAcceptButtonProps>
> = (props) => {
  const workersRef = useRefEditorState((state) => {
    return state.workers
  })
  const projectContents = useEditorState(
    Substores.projectContents,
    (store) => store.editor.projectContents,
    'project contents',
  )
  const branchContents = useEditorState(
    Substores.restOfEditor,
    (store) => store.editor.branchOriginContents,
    'branch contents',
  )
  const clickButton = React.useCallback(() => {
    if (props.status == null) {
      return
    }
    if (!props.dialogDisabled) {
      props.setDialogDisabled(true)
      void revertGithubFile(
        workersRef.current,
        props.status,
        props.filePath,
        projectContents,
        branchContents,
      )
        .then((actions) => {
          props.dispatch([...actions, EditorActions.hideModal()], 'everyone')
        })
        .finally(() => {
          props.setDialogDisabled(false)
        })
    }
  }, [workersRef, props, projectContents, branchContents])

  return (
    <FormButton primary danger onClick={clickButton} disabled={props.dialogDisabled}>
      Revert
    </FormButton>
  )
}

interface ConfirmRevertCancelButtonProps extends ConfirmRevertDialogProps {
  dialogDisabled: boolean
  setDialogDisabled: (disabled: boolean) => void
}

const CancelButton: React.FunctionComponent<
  React.PropsWithChildren<ConfirmRevertCancelButtonProps>
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
