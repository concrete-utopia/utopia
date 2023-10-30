/** @jsxRuntime classic */
/** @jsx jsx */
/** @jsxFrag React.Fragment */
import { jsx } from '@emotion/react'
import React, { useState } from 'react'
import {
  Button,
  CheckboxInput,
  colorTheme,
  FlexColumn,
  FlexRow,
  H2,
  HeadlessStringInput,
  PopupList,
  Section,
  SectionBodyArea,
  StringInput,
  UtopiaTheme,
} from '../../../uuiui'
import { getControlStyles } from '../../../uuiui-deps'
import { InspectorInputEmotionStyle } from '../../../uuiui/inputs/base-input'

import type { SelectOption } from '../../../uuiui-deps'
import * as EditorActions from '../../editor/actions/action-creators'
import { setProjectDescription, setProjectName } from '../../editor/actions/action-creators'
import { useDispatch } from '../../editor/store/dispatch-context'
import { Substores, useEditorState, useRefEditorState } from '../../editor/store/store-hook'
import { UIGridRow } from '../../inspector/widgets/ui-grid-row'
import { ForksGiven } from './forks-given'
import type { FeatureName } from '../../../utils/feature-switches'
import {
  toggleFeatureEnabled,
  isFeatureEnabled,
  AllFeatureNames,
} from '../../../utils/feature-switches'
import json5 from 'json5'
import { load } from '../../../components/editor/actions/actions'
import { when } from '../../../utils/react-conditionals'
import { useTriggerForkProject } from '../../editor/persistence-hooks'
import { saveUserPreferencesDefaultLayout } from '../../common/user-preferences'
import { useGridPanelState } from '../../canvas/grid-panels-state'
import { notice } from '../../common/notice'
import { gridMenuDefaultPanels } from '../../canvas/stored-layout'

const themeOptions = [
  {
    label: 'System',
    value: 'system',
  },
  {
    label: 'Dark',
    value: 'dark',
  },
  {
    label: 'Light',
    value: 'light',
  },
]

const defaultTheme = themeOptions[0]

export const FeatureSwitchesSection = React.memo(() => {
  if (AllFeatureNames.length > 0) {
    return (
      <React.Fragment>
        <FlexRow style={{ marginTop: 8, marginBottom: 12, paddingLeft: 8 }}>
          <H2>Experimental Feature Toggles</H2>
        </FlexRow>
        {AllFeatureNames.map((name) => (
          <FeatureSwitchRow key={`feature-switch-${name}`} name={name} />
        ))}
      </React.Fragment>
    )
  } else {
    return null
  }
})

const FeatureSwitchRow = React.memo((props: { name: FeatureName }) => {
  const name = props.name
  const id = `toggle-${name}`
  const [changeCount, setChangeCount] = React.useState(0)
  const forceRender = React.useCallback(() => setChangeCount(changeCount + 1), [changeCount])
  const onChange = React.useCallback(() => {
    toggleFeatureEnabled(name)
    forceRender()
  }, [forceRender, name])
  return (
    <FlexRow
      style={{ paddingLeft: 12, paddingRight: 12, height: UtopiaTheme.layout.rowHeight.normal }}
    >
      <CheckboxInput
        style={{ marginRight: 8 }}
        id={id}
        checked={isFeatureEnabled(name)}
        onChange={onChange}
      />
      <label htmlFor={id}>{name}</label>
    </FlexRow>
  )
})

export const SettingsPane = React.memo(() => {
  const dispatch = useDispatch()
  const { projectName, projectDescription } = useEditorState(
    Substores.restOfEditor,
    (store) => {
      return {
        projectName: store.editor.projectName,
        projectDescription: store.editor.projectDescription,
      }
    },
    'SettingsPane',
  )

  const userState = useEditorState(
    Substores.userState,
    (store) => store.userState,
    'SettingsPane userState',
  )
  const themeConfig = userState.themeConfig

  const isMyProject = useEditorState(
    Substores.projectServerState,
    (store) => store.projectServerState.isMyProject,
    'SettingsPane isMyProject',
  )

  const [theme, setTheme] = React.useState<SelectOption>(
    themeOptions.find((option) => option.value === themeConfig) ?? defaultTheme,
  )

  const handleSubmitValueTheme = React.useCallback(
    (option: SelectOption) => {
      setTheme(option)
      dispatch([EditorActions.setCurrentTheme(option.value)])
    },
    [dispatch],
  )

  const onChangeProjectName = React.useCallback((event: React.ChangeEvent<HTMLInputElement>) => {
    changeProjectName(event.target.value)
  }, [])

  const onChangeProjectDescription = React.useCallback(
    (event: React.ChangeEvent<HTMLInputElement>) => {
      changeProjectDescription(event.target.value)
    },
    [],
  )

  const updateProjectName = React.useCallback(
    (newProjectName: string) => {
      dispatch([setProjectName(newProjectName)])
    },
    [dispatch],
  )

  const updateProjectDescription = React.useCallback(
    (newProjectDescription: string) => {
      dispatch([setProjectDescription(newProjectDescription)])
    },
    [dispatch],
  )

  const handleBlurProjectName = React.useCallback(
    (e: React.ChangeEvent<HTMLInputElement>) => {
      updateProjectName(e.target.value)
    },
    [updateProjectName],
  )

  const handleBlurProjectDescription = React.useCallback(
    (e: React.ChangeEvent<HTMLInputElement>) => {
      updateProjectDescription(e.target.value)
    },
    [updateProjectDescription],
  )

  const handleKeyPress = React.useCallback((e: React.KeyboardEvent<HTMLInputElement>) => {
    if (e.key === 'Enter') {
      let target = e.target as HTMLInputElement
      target.blur()
    }
  }, [])

  const [name, changeProjectName] = useState(projectName),
    [description, changeProjectDescription] = useState(projectDescription)

  const entireStateRef = useRefEditorState((store) => store)

  const loadProjectContentJson = React.useCallback(
    (value: string) => {
      const confirmed = window.confirm(
        'If you press OK, the inserted code will override the current project. Are you sure?',
      )
      if (confirmed) {
        const persistentModel = json5.parse(value)
        console.info('attempting to load new Project Contents JSON', persistentModel)
        void load(
          dispatch,
          persistentModel,
          entireStateRef.current.editor.projectName,
          entireStateRef.current.editor.id!,
          entireStateRef.current.builtInDependencies,
        )
      }
    },
    [dispatch, entireStateRef],
  )

  const onForkProjectClicked = useTriggerForkProject()

  const [panelState, setPanelState] = useGridPanelState()

  const onSavePanelsDefaultLayout = React.useCallback(() => {
    void saveUserPreferencesDefaultLayout(panelState)
    dispatch([
      EditorActions.addToast(
        notice('Saved current panels layout as default for new projects.', 'SUCCESS'),
      ),
    ])
  }, [panelState, dispatch])

  const onResetPanelsLayout = React.useCallback(() => {
    setPanelState(gridMenuDefaultPanels())
    dispatch([EditorActions.addToast(notice('Restored project panels layout.', 'SUCCESS'))])
  }, [dispatch, setPanelState])

  const onResetPanelsDefaultLayout = React.useCallback(async () => {
    await saveUserPreferencesDefaultLayout(gridMenuDefaultPanels())
    setPanelState(gridMenuDefaultPanels())
    dispatch([EditorActions.addToast(notice('Restored default panels layout.', 'SUCCESS'))])
  }, [dispatch, setPanelState])

  return (
    <FlexColumn
      id='leftPaneSettings'
      key='leftPaneSettings'
      style={{
        display: 'relative',
        alignItems: 'stretch',
        paddingBottom: 50,
        overflowY: 'scroll',
        alignSelf: 'stretch',
      }}
    >
      <Section>
        {isMyProject === 'yes' ? null : <ForksGiven />}
        <UIGridRow padded variant='<---1fr--->|------172px-------|'>
          <span style={{ color: colorTheme.fg2.value }}>Name</span>
          {userState.loginState.type !== 'LOGGED_IN' ? (
            <span>{name}</span>
          ) : (
            <StringInput
              testId='projectName'
              value={name}
              onChange={onChangeProjectName}
              onKeyDown={handleKeyPress}
              style={{ width: 150 }}
              onBlur={handleBlurProjectName}
            />
          )}
        </UIGridRow>
        <UIGridRow padded variant='<---1fr--->|------172px-------|'>
          <span style={{ color: colorTheme.fg2.value }}> Description </span>
          {userState.loginState.type !== 'LOGGED_IN' ? (
            <span>{description}</span>
          ) : (
            <StringInput
              testId='projectDescription'
              value={description}
              onChange={onChangeProjectDescription}
              onKeyDown={handleKeyPress}
              onBlur={handleBlurProjectDescription}
              style={{ width: 150 }}
            />
          )}
        </UIGridRow>
        {when(
          userState.loginState.type === 'LOGGED_IN',
          <div
            style={{
              height: UtopiaTheme.layout.rowHeight.normal,
              display: 'flex',
              justifyContent: 'center',
              alignItems: 'center',
              margin: '0 8px',
            }}
          >
            <Button
              outline={false}
              highlight
              onClick={onForkProjectClicked}
              style={{
                width: '100%',
                cursor: 'pointer',
                height: UtopiaTheme.layout.inputHeight.default,
                background: colorTheme.dynamicBlue.value,
                color: colorTheme.bg1.value,
              }}
            >
              Fork this project
            </Button>
          </div>,
        )}
        <SectionBodyArea minimised={false}>
          {/** Theme Toggle: */}
          <UIGridRow
            style={{ color: colorTheme.fg1.value, marginTop: 16 }}
            padded
            variant='<---1fr--->|------172px-------|'
          >
            <H2>Application</H2>
          </UIGridRow>
          <UIGridRow padded variant='<---1fr--->|------172px-------|'>
            <span style={{ color: colorTheme.fg2.value }}>Theme </span>
            <PopupList
              value={theme}
              options={themeOptions}
              onSubmitValue={handleSubmitValueTheme}
              style={{ width: 150 }}
            />
          </UIGridRow>
          <UIGridRow
            padded
            variant='<---1fr--->|------172px-------|'
            style={{ alignItems: 'flex-start' }}
          >
            <span style={{ color: colorTheme.fg2.value }}>Panels </span>
            <div style={{ display: 'flex', flexDirection: 'column', gap: 6 }}>
              <Button
                outline={false}
                highlight
                onClick={onSavePanelsDefaultLayout}
                style={{
                  cursor: 'pointer',
                  background: colorTheme.dynamicBlue.value,
                  color: colorTheme.bg1.value,
                }}
              >
                Set as default
              </Button>
              <Button outline={false} highlight spotlight onClick={onResetPanelsLayout}>
                Reset for this project
              </Button>
              <Button outline={false} highlight spotlight onClick={onResetPanelsDefaultLayout}>
                Restore defaults
              </Button>
            </div>
          </UIGridRow>
          <UIGridRow padded variant='<-------------1fr------------->'>
            <br />
            <HeadlessStringInput
              placeholder='Project Contents JSON'
              onSubmitValue={loadProjectContentJson}
              css={InspectorInputEmotionStyle({
                hasLabel: false,
                controlStyles: getControlStyles('simple'),
              })}
            />
          </UIGridRow>
        </SectionBodyArea>
        <FeatureSwitchesSection />
      </Section>
    </FlexColumn>
  )
})
