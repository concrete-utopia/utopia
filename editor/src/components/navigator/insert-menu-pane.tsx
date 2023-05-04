/** @jsxRuntime classic */
/** @jsx jsx */
/** @jsxFrag React.Fragment */
import { jsx } from '@emotion/react'
import React from 'react'
import { NO_OP } from '../../core/shared/utils'
import {
  Button,
  FlexRow,
  Icons,
  Section,
  SectionBodyArea,
  SectionTitleRow,
  Title,
} from '../../uuiui'
import { setFocus } from '../common/actions'
import { EditorDispatch, LoginState } from '../editor/action-types'
import { InsertMenu } from '../editor/insertmenu'
import { useDispatch } from '../editor/store/dispatch-context'
import { DerivedState, EditorState, RightMenuTab } from '../editor/store/editor-state'
import { Substores, useEditorState } from '../editor/store/store-hook'
import { setRightMenuTab } from '../editor/actions/action-creators'

export interface LeftPaneProps {
  editorState: EditorState
  derivedState: DerivedState
  editorDispatch: EditorDispatch
  loginState: LoginState
}

export const InsertMenuPane = React.memo(() => {
  const dispatch = useDispatch()
  const { focusedPanel } = useEditorState(
    Substores.restOfEditor,
    (store) => {
      return {
        focusedPanel: store.editor.focusedPanel,
      }
    },
    'InsertMenuPane',
  )

  const onFocus = React.useCallback(
    (event: React.FocusEvent<HTMLElement>) => {
      if (focusedPanel !== 'insertmenu') {
        dispatch([setFocus('insertmenu')], 'everyone')
      }
    },
    [dispatch, focusedPanel],
  )

  const onClickClose = React.useCallback(() => {
    dispatch([setRightMenuTab(RightMenuTab.Inspector)])
  }, [dispatch])

  return (
    <Section data-name='InsertMenu' onFocus={onFocus} tabIndex={-1} style={{ width: '100%' }}>
      <SectionTitleRow minimised={false} toggleMinimised={NO_OP} hideButton={true}>
        <FlexRow flexGrow={1} style={{ position: 'relative' }}>
          <Title>Insert</Title>
        </FlexRow>
        <Button highlight style={{ width: 22, height: 22 }}>
          <Icons.Cross onMouseDown={onClickClose} onClick={onClickClose} />
        </Button>
      </SectionTitleRow>
      <SectionBodyArea
        minimised={false}
        style={{ paddingLeft: 8, paddingRight: 8, overflow: 'auto' }}
      >
        <InsertMenu />
      </SectionBodyArea>
    </Section>
  )
})
