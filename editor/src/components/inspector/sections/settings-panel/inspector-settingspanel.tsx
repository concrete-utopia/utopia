import * as React from 'react'
import * as EditorActions from '../../../editor/actions/actions'
import { Button, colorTheme, UtopiaTheme } from 'uuiui'
import { FlexRow } from 'uuiui'
import { H2 } from 'uuiui'
import { FlexColumn } from 'uuiui'
import styled from '@emotion/styled'
import { CheckboxInput } from 'uuiui'
import { useEditorState } from '../../../editor/store/store-hook'
import { betterReactMemo } from '../../../../uuiui-deps'
import {
  FeatureName,
  toggleFeatureEnabled,
  isFeatureEnabled,
  AllFeatureNames,
} from '../../../../utils/feature-switches'
import { getOpenUIJSFile } from '../../../editor/store/editor-state'

const StyledFlexRow = styled(FlexRow)({
  height: UtopiaTheme.layout.rowHeight.medium,
  paddingLeft: 12,
  paddingRight: 12,
})

const FeatureSwitchesSection = betterReactMemo('Feature Switches', () => {
  if (AllFeatureNames.length > 0) {
    return (
      <>
        <StyledFlexRow style={{ marginTop: 8, marginBottom: 12, paddingLeft: 8 }}>
          <H2>Experimental Feature Toggles</H2>
        </StyledFlexRow>
        {AllFeatureNames.map((name) => (
          <FeatureSwitchRow key={`feature-switch-${name}`} name={name} />
        ))}
      </>
    )
  } else {
    return null
  }
})

const FeatureSwitchRow = betterReactMemo('Feature Switch Row', (props: { name: FeatureName }) => {
  const name = props.name
  const id = `toggle-${name}`
  const [changeCount, setChangeCount] = React.useState(0)
  const forceRender = React.useCallback(() => setChangeCount(changeCount + 1), [changeCount])
  const onChange = React.useCallback(() => {
    toggleFeatureEnabled(name)
    forceRender()
  }, [forceRender, name])
  return (
    <StyledFlexRow>
      <CheckboxInput
        style={{ marginRight: 8 }}
        id={id}
        checked={isFeatureEnabled(name)}
        onChange={onChange}
      />
      <label htmlFor={id}>{name}</label>
    </StyledFlexRow>
  )
})

export const SettingsPanel = (props: any) => {
  const dispatch = useEditorState((store) => store.dispatch, 'SettingsPanel dispatch')
  const interfaceDesigner = useEditorState(
    (store) => store.editor.interfaceDesigner,
    'SettingsPanel interfaceDesigner',
  )

  const openUiJsFile = useEditorState((store) => {
    return getOpenUIJSFile(store.editor)
  }, 'SettingsPanel openUiJsFile')

  const jsxMetadata = useEditorState((store) => {
    return store.editor.jsxMetadataKILLME
  }, 'SettingsPanel jsxMetadata')

  const toggleCodeEditorVisible = React.useCallback(() => {
    dispatch([EditorActions.toggleInterfaceDesignerCodeEditor()])
  }, [dispatch])

  const toggleLayoutReversed = React.useCallback(() => {
    dispatch([EditorActions.toggleInterfaceDesignerLayoutReversed()])
  }, [dispatch])

  const toggleAdditionalControls = React.useCallback(() => {
    dispatch([EditorActions.toggleInterfaceDesignerAdditionalControls()])
  }, [dispatch])

  const printOpenUiJsFileModel = React.useCallback(() => {
    console.info('Open UIJSFile:', openUiJsFile)
  }, [openUiJsFile])

  const printCanvasMetadata = React.useCallback(() => {
    console.info('Latest metadata:', jsxMetadata)
  }, [jsxMetadata])

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
          id='toggleInterfaceDesignerLayoutReversed'
          checked={interfaceDesigner.layoutReversed}
          onChange={toggleLayoutReversed}
        />
        <label htmlFor='toggleInterfaceDesignerLayoutReversed'>Code Editor Left of Canvas </label>
      </StyledFlexRow>
      <StyledFlexRow>
        <CheckboxInput
          style={{ marginRight: 8 }}
          id='toggleInterfaceDesignerAdditionalCanvasControls'
          checked={interfaceDesigner.additionalControls}
          onChange={toggleAdditionalControls}
        />
        <label htmlFor='toggleInterfaceDesignerAdditionalCanvasControls'>Additional controls</label>
      </StyledFlexRow>
      <br />
      <Button outline spotlight onClick={printOpenUiJsFileModel}>
        Print Current Model to Console
      </Button>
      <Button outline spotlight onClick={printCanvasMetadata}>
        Print Latest Metadata / Measurements
      </Button>
      <FeatureSwitchesSection />
    </FlexColumn>
  )
}
