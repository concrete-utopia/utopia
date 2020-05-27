import * as React from 'react'
import * as EditorActions from '../../../editor/actions/actions'
import { colorTheme, UtopiaTheme } from 'uuiui'
import { FlexRow } from 'uuiui'
import { H2 } from 'uuiui'
import { FlexColumn } from 'uuiui'
import styled from '@emotion/styled'
import { CheckboxInput } from 'uuiui'
import { useEditorState } from '../../../editor/store/store-hook'

const StyledFlexRow = styled(FlexRow)({
  height: UtopiaTheme.layout.rowHeight.medium,
  paddingLeft: 12,
  paddingRight: 12,
})

export const SettingsPanel = (props: any) => {
  const dispatch = useEditorState((store) => store.dispatch)
  const interfaceDesigner = useEditorState((store) => store.editor.interfaceDesigner)

  const toggleCodeEditorVisible = () => {
    dispatch([EditorActions.toggleInterfaceDesignerCodeEditor()])
  }

  const toggleLayoutReversed = () => {
    dispatch([EditorActions.toggleInterfaceDesignerLayoutReversed()])
  }

  const toggleAdditionalControls = () => {
    dispatch([EditorActions.toggleInterfaceDesignerAdditionalControls()])
  }

  return (
    <FlexColumn
      style={{
        marginBottom: UtopiaTheme.layout.rowHeight.medium,
        borderBottom: `1px solid ${colorTheme.subduedBorder.value}`,
      }}
    >
      <StyledFlexRow style={{ marginTop: 8, marginBottom: 12, paddingLeft: 8 }}>
        <H2>Interface</H2>
      </StyledFlexRow>
      <StyledFlexRow>
        <CheckboxInput
          style={{ display: 'flex', alignItems: 'center', marginRight: 4 }}
          id='showCodeEditorLabel'
          checked={interfaceDesigner.codePaneVisible}
          onChange={toggleCodeEditorVisible}
        />
        <label htmlFor='showCodeEditorLabel'>Show Code Editor</label>
      </StyledFlexRow>
      <StyledFlexRow>
        <CheckboxInput
          style={{ display: 'flex', alignItems: 'center', marginRight: 4 }}
          id='toggleInterfaceDesignerLayoutReversed'
          checked={interfaceDesigner.layoutReversed}
          onChange={toggleLayoutReversed}
        />
        <label htmlFor='toggleInterfaceDesignerLayoutReversed'>Code Editor Left of Canvas </label>
      </StyledFlexRow>
      <StyledFlexRow>
        <CheckboxInput
          style={{ display: 'flex', alignItems: 'center', marginRight: 4 }}
          id='toggleInterfaceDesignerAdditionalCanvasControls'
          checked={interfaceDesigner.additionalControls}
          onChange={toggleAdditionalControls}
        />
        <label htmlFor='toggleInterfaceDesignerAdditionalCanvasControls'>Additional controls</label>
      </StyledFlexRow>
    </FlexColumn>
  )
}
