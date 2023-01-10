/** @jsxRuntime classic */
/** @jsx jsx */
/** @jsxFrag React.Fragment */
import { jsx } from '@emotion/react'
import React from 'react'
import { NO_OP } from '../../core/shared/utils'
import { FlexRow, Section, SectionBodyArea, SectionTitleRow, Title } from '../../uuiui'
import { setFocus } from '../common/actions'
import { EditorDispatch, LoginState } from '../editor/action-types'
import { InsertMenu } from '../editor/insertmenu'
import { useDispatch } from '../editor/store/dispatch-context'
import { DerivedState, EditorState } from '../editor/store/editor-state'
import { useEditorState } from '../editor/store/store-hook'

export interface LeftPaneProps {
  editorState: EditorState
  derivedState: DerivedState
  editorDispatch: EditorDispatch
  loginState: LoginState
}

export const InsertMenuPane = React.memo(() => {
  const dispatch = useDispatch()
  const { focusedPanel } = useEditorState('restOfEditor')((store) => {
    return {
      focusedPanel: store.editor.focusedPanel,
    }
  }, 'InsertMenuPane')

  const onFocus = React.useCallback(
    (event: React.FocusEvent<HTMLElement>) => {
      if (focusedPanel !== 'insertmenu') {
        dispatch([setFocus('insertmenu')], 'everyone')
      }
    },
    [dispatch, focusedPanel],
  )

  return (
    <Section data-name='InsertMenu' onFocus={onFocus} tabIndex={-1} style={{ width: '100%' }}>
      <SectionTitleRow minimised={false} toggleMinimised={NO_OP}>
        <FlexRow flexGrow={1} style={{ position: 'relative' }}>
          <Title>Insert</Title>
        </FlexRow>
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
