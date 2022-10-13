/** @jsxRuntime classic */
/** @jsx jsx */
import { jsx } from '@emotion/react'
import React from 'react'
import { FormButton } from '../../uuiui'

import { EditorDispatch } from '../editor/action-types'
import * as EditorActions from '../editor/actions/action-creators'

interface CancelButtonProps {
  dispatch: EditorDispatch
}

export const CancelButton: React.FunctionComponent<React.PropsWithChildren<CancelButtonProps>> = (
  props,
) => {
  const clickButton = React.useCallback(() => {
    props.dispatch([EditorActions.hideModal()], 'everyone')
  }, [props])

  return <FormButton onClick={clickButton}>Cancel</FormButton>
}
