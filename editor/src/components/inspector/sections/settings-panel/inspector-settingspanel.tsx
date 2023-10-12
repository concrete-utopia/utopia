/** @jsxRuntime classic */
/** @jsx jsx */
/** @jsxFrag */
import React from 'react'
import { jsx } from '@emotion/react'
import * as EditorActions from '../../../editor/actions/action-creators'
import styled from '@emotion/styled'
import { Substores, useEditorState } from '../../../editor/store/store-hook'
import { FlexRow, UtopiaTheme, H2, CheckboxInput, FlexColumn, Subdued } from '../../../../uuiui'
import { useDispatch } from '../../../editor/store/dispatch-context'
import { User } from '../../../../uuiui-deps'
import { LoggedOutPane } from '../../../../components/navigator/left-pane/logged-out-pane'

const StyledFlexRow = styled(FlexRow)({
  height: UtopiaTheme.layout.rowHeight.normal,
  paddingLeft: 12,
  paddingRight: 12,
})

export const SettingsPanel = React.memo(() => {
  const dispatch = useDispatch()
  const interfaceDesigner = useEditorState(
    Substores.restOfEditor,
    (store) => store.editor.interfaceDesigner,
    'SettingsPanel interfaceDesigner',
  )
  const navigatorMinimised = useEditorState(
    Substores.restOfEditor,
    (store) => !store.editor.leftMenu.visible,
    'SettingsPanel navigator.minimised',
  )

  const inspectorVisible = useEditorState(
    Substores.restOfEditor,
    (store) => store.editor.inspector.visible,
    'SettingsPanel inspector.visible',
  )

  const toggleCodeEditorVisible = React.useCallback(() => {
    dispatch([EditorActions.togglePanel('codeEditor')])
  }, [dispatch])

  const toggleNavigatorVisible = React.useCallback(() => {
    dispatch([EditorActions.togglePanel('leftmenu')])
  }, [dispatch])

  const toggleInspectorVisible = React.useCallback(() => {
    dispatch([EditorActions.togglePanel('rightmenu')])
  }, [dispatch])

  const toggleAdditionalControls = React.useCallback(() => {
    dispatch([EditorActions.toggleInterfaceDesignerAdditionalControls()])
  }, [dispatch])

  const loggedIn = useEditorState(
    Substores.restOfStore,
    (store) => User.isLoggedIn(store.userState.loginState),
    'LeftPaneComponent loggedIn',
  )

  return (
    <FlexColumn>
      {loggedIn ? null : <LoggedOutPane />}
      <FlexRow style={{ marginTop: 8, marginBottom: 12, paddingLeft: 8 }}>
        <H2>Interface</H2>
      </FlexRow>
      <StyledFlexRow>
        <CheckboxInput
          style={{ marginRight: 8 }}
          id='showCodeEditorLabel'
          checked={interfaceDesigner.codePaneVisible}
          onChange={toggleCodeEditorVisible}
        />
        <label htmlFor='showCodeEditorLabel'>Show Code Editor</label>
      </StyledFlexRow>
      <StyledFlexRow>
        <CheckboxInput
          style={{ marginRight: 8 }}
          id='showNavigatorLabel'
          checked={!navigatorMinimised}
          onChange={toggleNavigatorVisible}
        />
        <label htmlFor='showNavigatorLabel'>Show Navigator</label>
      </StyledFlexRow>
      <StyledFlexRow>
        <CheckboxInput
          style={{ marginRight: 8 }}
          id='showInspectorLabel'
          checked={inspectorVisible}
          onChange={toggleInspectorVisible}
        />
        <label htmlFor='showInspectorLabel'>Show Inspector</label>
      </StyledFlexRow>
      <div style={{ padding: '0px 34px', wordWrap: 'normal', whiteSpace: 'normal' }}>
        <Subdued>That's this panel! Get it back from the toolbar.</Subdued>
      </div>

      <StyledFlexRow>
        <CheckboxInput
          style={{ marginRight: 8 }}
          id='toggleInterfaceDesignerAdditionalCanvasControls'
          checked={interfaceDesigner.additionalControls}
          onChange={toggleAdditionalControls}
        />
        <label htmlFor='toggleInterfaceDesignerAdditionalCanvasControls'>Additional controls</label>
      </StyledFlexRow>
    </FlexColumn>
  )
})
