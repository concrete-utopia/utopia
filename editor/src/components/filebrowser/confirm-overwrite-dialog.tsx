/** @jsxRuntime classic */
/** @jsx jsx */
import { jsx } from '@emotion/react'
import React from 'react'
import { Dialog, FormButton } from '../../uuiui'
import type { EditorDispatch } from '../editor/action-types'
import * as EditorActions from '../editor/actions/action-creators'
import { fileResultUploadAction } from '../editor/image-insert'
import type { FileUploadInfo } from '../editor/store/editor-state'

interface ConfirmOverwriteDialogProps {
  dispatch: EditorDispatch
  files: Array<FileUploadInfo>
}

export const ConfirmOverwriteDialog: React.FunctionComponent<
  React.PropsWithChildren<ConfirmOverwriteDialogProps>
> = (props) => {
  const { dispatch, files } = props

  const [index, setIndex] = React.useState(0)

  const hide = React.useCallback(() => {
    dispatch([EditorActions.hideModal()], 'everyone')
  }, [dispatch])

  const switchToNextFile = React.useCallback(() => {
    if (index < files.length - 1) {
      setIndex(index + 1)
    } else {
      hide()
    }
  }, [index, files, hide])

  const onOverwriteClick = React.useCallback(() => {
    dispatch(
      [fileResultUploadAction(files[index].fileResult, files[index].targetPath, true)],
      'everyone',
    )
    switchToNextFile()
  }, [switchToNextFile, index, files, dispatch])

  return (
    <Dialog
      title='Replace file'
      content={<DialogBody targetPath={files[index].targetPath} />}
      defaultButton={
        <FormButton primary danger onClick={onOverwriteClick}>
          Replace
        </FormButton>
      }
      secondaryButton={<FormButton onClick={switchToNextFile}>Skip</FormButton>}
      subduedButton={<FormButton onClick={hide}>Cancel all</FormButton>}
      closeCallback={hide}
    />
  )
}

const DialogBody: React.FunctionComponent<React.PropsWithChildren<{ targetPath: string }>> = (
  props,
) => (
  <React.Fragment>
    <p>
      Are you sure you want to replace <span>{props.targetPath}</span>?
    </p>
    <p>Replaced files are not recoverable.</p>
  </React.Fragment>
)
