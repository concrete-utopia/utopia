/** @jsxRuntime classic */
/** @jsx jsx */
import { jsx } from '@emotion/react'
import React from 'react'
import type { GithubFileStatus } from '../../core/shared/github/helpers'
import { revertGithubFile } from '../../core/shared/github/helpers'
import { Dialog, FormButton } from '../../uuiui'
import type { EditorDispatch } from '../editor/action-types'
import * as EditorActions from '../editor/actions/action-creators'
import { Substores, useEditorState } from '../editor/store/store-hook'

interface ConfirmRevertDialogProps {
  dispatch: EditorDispatch
  filePath: string
  status: GithubFileStatus | null
}

export const ConfirmRevertDialogProps: React.FunctionComponent<
  React.PropsWithChildren<ConfirmRevertDialogProps>
> = (props) => {
  const hide = React.useCallback(() => {
    props.dispatch([EditorActions.hideModal()], 'everyone')
  }, [props])
  return (
    <Dialog
      title='Revert changes'
      content={<DialogBody {...props} />}
      defaultButton={<AcceptButton {...props} />}
      secondaryButton={<CancelButton {...props} />}
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

const AcceptButton: React.FunctionComponent<React.PropsWithChildren<ConfirmRevertDialogProps>> = (
  props,
) => {
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
    const actions = revertGithubFile(props.status, props.filePath, projectContents, branchContents)
    props.dispatch([...actions, EditorActions.hideModal()], 'everyone')
  }, [props, projectContents, branchContents])

  return (
    <FormButton primary danger onClick={clickButton}>
      Revert
    </FormButton>
  )
}

const CancelButton: React.FunctionComponent<React.PropsWithChildren<ConfirmRevertDialogProps>> = (
  props,
) => {
  const clickButton = React.useCallback(() => {
    props.dispatch([EditorActions.hideModal()], 'everyone')
  }, [props])

  return <FormButton onClick={clickButton}>Cancel</FormButton>
}
