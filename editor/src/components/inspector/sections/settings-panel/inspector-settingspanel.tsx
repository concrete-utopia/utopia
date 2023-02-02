/** @jsxRuntime classic */
/** @jsx jsx */
/** @jsxFrag */
import React from 'react'
import { jsx } from '@emotion/react'
import * as EditorActions from '../../../editor/actions/action-creators'
import styled from '@emotion/styled'
import { Substores, useEditorState } from '../../../editor/store/store-hook'
import {
  FlexRow,
  UtopiaTheme,
  H2,
  CheckboxInput,
  FlexColumn,
  useColorTheme,
  Subdued,
} from '../../../../uuiui'

import { useDispatch } from '../../../editor/store/dispatch-context'

const StyledFlexRow = styled(FlexRow)({
  height: UtopiaTheme.layout.rowHeight.normal,
  paddingLeft: 12,
  paddingRight: 12,
})

export const SettingsPanel = React.memo(() => {
  const colorTheme = useColorTheme()
  const dispatch = useDispatch()
  const interfaceDesigner = useEditorState(
    Substores.restOfEditor,
    (store) => store.editor.interfaceDesigner,
    'SettingsPanel interfaceDesigner',
  )
  const navigatorMinimised = useEditorState(
    Substores.restOfEditor,
    (store) => store.editor.navigator.minimised,
    'SettingsPanel navigator.minimised',
  )

  const inspectorVisible = useEditorState(
    Substores.restOfEditor,
    (store) => store.editor.inspector.visible,
    'SettingsPanel inspector.visible',
  )

  const toggleCodeEditorVisible = React.useCallback(() => {
    dispatch([EditorActions.toggleInterfaceDesignerCodeEditor()])
  }, [dispatch])

  const toggleNavigatorVisible = React.useCallback(() => {
    dispatch([EditorActions.togglePanel('navigator')])
  }, [dispatch])

  const toggleInspectorVisible = React.useCallback(() => {
    dispatch([EditorActions.togglePanel('rightmenu')])
  }, [dispatch])

  const toggleAdditionalControls = React.useCallback(() => {
    dispatch([EditorActions.toggleInterfaceDesignerAdditionalControls()])
  }, [dispatch])

  return (
    <FlexColumn
      style={{
        marginBottom: UtopiaTheme.layout.rowHeight.normal,
        borderBottom: `1px solid ${colorTheme.subduedBorder.value}`,
      }}
    >
      <StyledFlexRow style={{ marginTop: 8, marginBottom: 12, paddingLeft: 8 }}>
        <H2>Interface</H2>
      </StyledFlexRow>
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
