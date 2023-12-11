/** @jsxRuntime classic */
/** @jsx jsx */
/** @jsxFrag React.Fragment */
import { jsx } from '@emotion/react'
import React from 'react'
import { Section, SectionBodyArea } from '../../uuiui'
import { setFocus } from '../common/actions'
import type { EditorDispatch, LoginState } from '../editor/action-types'
import { VariablesMenu } from '../editor/variablesmenu'
import { useDispatch } from '../editor/store/dispatch-context'
import type { DerivedState, EditorState } from '../editor/store/editor-state'
import { RightMenuTab } from '../editor/store/editor-state'
import { Substores, useEditorState } from '../editor/store/store-hook'
import { setRightMenuTab } from '../editor/actions/action-creators'

export interface LeftPaneProps {
  editorState: EditorState
  derivedState: DerivedState
  editorDispatch: EditorDispatch
  loginState: LoginState
}

export const VariablesMenuPane = React.memo(() => {
  const dispatch = useDispatch()
  const { focusedPanel } = useEditorState(
    Substores.restOfEditor,
    (store) => {
      return {
        focusedPanel: store.editor.focusedPanel,
      }
    },
    'VariablesMenuPane',
  )

  const onFocus = React.useCallback(
    (event: React.FocusEvent<HTMLElement>) => {
      if (focusedPanel !== 'variablesmenu') {
        dispatch([setFocus('variablesmenu')], 'everyone')
      }
    },
    [dispatch, focusedPanel],
  )

  const onClickClose = React.useCallback(() => {
    dispatch([setRightMenuTab(RightMenuTab.Inspector)])
  }, [dispatch])

  return (
    <Section
      data-name='VariablesMenu'
      onFocus={onFocus}
      tabIndex={-1}
      style={{ width: '100%', height: '100%' }}
    >
      <SectionBodyArea minimised={false} style={{ height: '100%' }}>
        <VariablesMenu />
      </SectionBodyArea>
    </Section>
  )
})
