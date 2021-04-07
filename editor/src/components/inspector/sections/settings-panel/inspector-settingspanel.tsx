import * as React from 'react'
import * as EditorActions from '../../../editor/actions/action-creators'
import styled from '@emotion/styled'
import { useEditorState, useRefEditorState } from '../../../editor/store/store-hook'
import {
  FeatureName,
  toggleFeatureEnabled,
  isFeatureEnabled,
  AllFeatureNames,
} from '../../../../utils/feature-switches'
import { getOpenUIJSFile } from '../../../editor/store/editor-state'
import {
  FlexRow,
  UtopiaTheme,
  H2,
  CheckboxInput,
  FlexColumn,
  colorTheme,
  Button,
} from '../../../../uuiui'
import { betterReactMemo } from '../../../../uuiui-deps'

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

export const SettingsPanel = betterReactMemo('SettingsPanel', () => {
  const dispatch = useEditorState((store) => store.dispatch, 'SettingsPanel dispatch')
  const interfaceDesigner = useEditorState(
    (store) => store.editor.interfaceDesigner,
    'SettingsPanel interfaceDesigner',
  )

  const entireStateRef = useRefEditorState((store) => store)

  const openUiJsFile = useRefEditorState((store) => {
    return getOpenUIJSFile(store.editor)
  })

  const jsxMetadata = useRefEditorState((store) => {
    return store.editor.jsxMetadata
  })

  const toggleCodeEditorVisible = React.useCallback(() => {
    dispatch([EditorActions.toggleInterfaceDesignerCodeEditor()])
  }, [dispatch])

  const toggleAdditionalControls = React.useCallback(() => {
    dispatch([EditorActions.toggleInterfaceDesignerAdditionalControls()])
  }, [dispatch])

  const printEditorState = React.useCallback(() => {
    console.info('Current Editor State:', entireStateRef.current)
  }, [entireStateRef])

  const printOpenUiJsFileModel = React.useCallback(() => {
    console.info('Open UIJSFile:', openUiJsFile.current)
  }, [openUiJsFile])

  const printCanvasMetadata = React.useCallback(() => {
    console.info('Latest metadata:', jsxMetadata.current)
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
          id='toggleInterfaceDesignerAdditionalCanvasControls'
          checked={interfaceDesigner.additionalControls}
          onChange={toggleAdditionalControls}
        />
        <label htmlFor='toggleInterfaceDesignerAdditionalCanvasControls'>Additional controls</label>
      </StyledFlexRow>
      <br />
      <Button outline spotlight onClick={printEditorState}>
        Print Current Editor State to Console
      </Button>
      <Button outline spotlight onClick={printOpenUiJsFileModel}>
        Print Current Model to Console
      </Button>
      <Button outline spotlight onClick={printCanvasMetadata}>
        Print Latest Metadata / Measurements
      </Button>
      <FeatureSwitchesSection />
    </FlexColumn>
  )
})
