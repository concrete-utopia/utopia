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
  Icn,
  PopupList,
  Section,
  StringInput,
  UtopiaTheme,
} from '../../../uuiui'
import type { SelectOption } from '../../../uuiui-deps'
import * as EditorActions from '../../editor/actions/action-creators'
import { setProjectDescription, setProjectName } from '../../editor/actions/action-creators'
import { useDispatch } from '../../editor/store/dispatch-context'
import { Substores, useEditorState, useRefEditorState } from '../../editor/store/store-hook'
import { UIGridRow } from '../../inspector/widgets/ui-grid-row'
import type { FeatureName } from '../../../utils/feature-switches'
import {
  toggleFeatureEnabled,
  isFeatureEnabled,
  getFeaturesToDisplay,
} from '../../../utils/feature-switches'
import json5 from 'json5'
import { load } from '../../../components/editor/actions/actions'
import { when } from '../../../utils/react-conditionals'
import { useTriggerForkProject } from '../../editor/persistence-hooks'
import { saveUserPreferencesDefaultLayout } from '../../common/user-preferences'
import { useGridPanelState } from '../../canvas/grid-panels-state'
import { notice } from '../../common/notice'
import { gridMenuDefaultPanels } from '../../canvas/stored-layout'
import { usePermissions } from '../../editor/store/permissions'
import { DisableControlsInSubtree } from '../../../uuiui/utilities/disable-subtree'
import { useIsMyProject } from '../../editor/store/collaborative-editing'

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
  const [changeCount, setChangeCount] = React.useState(0)
  // this replaces the 'forceRender' in the original implementation
  const onFeatureChange = React.useCallback(() => setChangeCount(changeCount + 1), [changeCount])
  // eslint-disable-next-line react-hooks/exhaustive-deps
  const featuresToDisplay = React.useMemo(() => getFeaturesToDisplay(), [changeCount])
  if (featuresToDisplay.length > 0) {
    return (
      <Section>
        <UIGridRow padded variant='<---1fr--->|------172px-------|'>
          <H2>Experimental Toggle Features</H2>
        </UIGridRow>
        {featuresToDisplay.map((name) => (
          <FeatureSwitchRow
            key={`feature-switch-${name}`}
            name={name}
            onFeatureChange={onFeatureChange}
          />
        ))}
      </Section>
    )
  } else {
    return null
  }
})

const FeatureSwitchRow = React.memo((props: { name: FeatureName; onFeatureChange: () => void }) => {
  const name = props.name
  const onFeatureChange = props.onFeatureChange
  const id = `toggle-${name}`
  const onChange = React.useCallback(() => {
    toggleFeatureEnabled(name)
    onFeatureChange()
  }, [name, onFeatureChange])
  return (
    <FlexRow style={{ paddingLeft: 16, height: UtopiaTheme.layout.rowHeight.normal }}>
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

  const isMyProject = useIsMyProject()

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

  const canEditProject = usePermissions().edit

  const projectOwnerMetadata = useEditorState(
    Substores.projectServerState,
    (store) => store.projectServerState.projectData,
    'ForksGiven projectOwnerMetadata',
  )

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
        gap: 16,
      }}
    >
      <Section>
        <UIGridRow padded variant='<---1fr--->|------172px-------|'>
          <H2>Project</H2>
        </UIGridRow>
        <DisableControlsInSubtree disable={!canEditProject}>
          <UIGridRow padded variant='|--80px--|<--------1fr-------->'>
            <span style={{ fontWeight: 500 }}>Name</span>
            <StringInput
              testId='projectName'
              value={name}
              onChange={onChangeProjectName}
              onKeyDown={handleKeyPress}
              onBlur={handleBlurProjectName}
              pasteHandler={true}
            />
          </UIGridRow>
          <UIGridRow padded variant='|--80px--|<--------1fr-------->'>
            <span style={{ fontWeight: 500 }}>Description</span>
            <StringInput
              testId='projectDescription'
              value={description}
              onChange={onChangeProjectDescription}
              onKeyDown={handleKeyPress}
              onBlur={handleBlurProjectDescription}
              pasteHandler={true}
            />
          </UIGridRow>
          <UIGridRow padded variant='|--80px--|<--------1fr-------->' style={{ marginBottom: 10 }}>
            <span style={{ fontWeight: 500 }}>Owner</span>
            <FlexRow style={{ gap: 5 }}>
              {projectOwnerMetadata?.ownerName} {isMyProject ? <span>(you)</span> : ''}
            </FlexRow>
          </UIGridRow>
        </DisableControlsInSubtree>
        {when(
          userState.loginState.type === 'LOGGED_IN',
          <UIGridRow padded variant='<-------------1fr------------->'>
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
                borderRadius: UtopiaTheme.layout.inputHeight.default,
              }}
            >
              Fork Project
            </Button>
          </UIGridRow>,
        )}
        <UIGridRow padded variant='<-------------1fr------------->'>
          <a href='/projects' target='_blank' rel='noopener rofererrer'>
            <Button
              highlight
              spotlight
              outline={false}
              style={{
                width: '100%',
                cursor: 'pointer',
                height: UtopiaTheme.layout.inputHeight.default,
                color: colorTheme.dynamicBlue.value,
                gap: 6,
                borderRadius: UtopiaTheme.layout.inputHeight.default,
                background: 'transparent',
              }}
            >
              <Icn category='semantic' type='externallink' color='dynamic' width={18} height={18} />
              My Projects
            </Button>
          </a>
        </UIGridRow>
      </Section>

      <Section>
        <UIGridRow padded variant='<---1fr--->|------172px-------|'>
          <H2>Application</H2>
        </UIGridRow>
        <UIGridRow padded variant='|--80px--|<--------1fr-------->'>
          <span style={{ fontWeight: 500 }}>Theme</span>
          <PopupList
            value={theme}
            options={themeOptions}
            onSubmitValue={handleSubmitValueTheme}
            style={{ background: 'transparent' }}
          />
        </UIGridRow>
        <UIGridRow
          padded
          variant='|--80px--|<--------1fr-------->'
          style={{ alignItems: 'flex-start', paddingTop: 6 }}
        >
          <div style={{ height: 22, fontWeight: 500 }}>Panels</div>
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
        <UIGridRow padded variant='|--80px--|<--------1fr-------->' style={{ marginTop: 6 }}>
          <div style={{ fontWeight: 500 }}>Contents</div>
          <HeadlessStringInput
            placeholder='Project Contents JSON'
            testId='project-contents-json-input'
            onSubmitValue={loadProjectContentJson}
            css={{
              color: colorTheme.fg1.value,
              background: colorTheme.bg2.value,
              border: 'none',
              height: 22,
              borderRadius: 1,
            }}
          />
        </UIGridRow>
      </Section>
      <FeatureSwitchesSection />
    </FlexColumn>
  )
})
