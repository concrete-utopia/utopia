/** @jsxRuntime classic */
/** @jsx jsx */
/** @jsxFrag React.Fragment */
import { jsx } from '@emotion/react'
import React, { useState } from 'react'
import { NO_OP } from '../../../core/shared/utils'
import {
  colorTheme,
  FlexColumn,
  FlexRow,
  H2,
  PopupList,
  Section,
  SectionBodyArea,
  SectionTitleRow,
  StringInput,
  Subdued,
  Title,
  UtopiaTheme,
} from '../../../uuiui'
import { SelectOption } from '../../../uuiui-deps'
import { useIsMyProject } from '../../common/server-hooks'
import * as EditorActions from '../../editor/actions/action-creators'
import { setProjectDescription, setProjectName } from '../../editor/actions/action-creators'
import { useDispatch } from '../../editor/store/dispatch-context'
import { useEditorState } from '../../editor/store/store-hook'
import { UIGridRow } from '../../inspector/widgets/ui-grid-row'
import { ForksGiven } from './forks-given'

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

export const SettingsPane = React.memo(() => {
  const dispatch = useDispatch()
  const { userState, projectId, projectName, projectDescription, themeConfig } = useEditorState(
    'fullOldStore',
    (store) => {
      return {
        userState: store.userState,
        projectId: store.editor.id,
        projectName: store.editor.projectName,
        projectDescription: store.editor.projectDescription,
        themeConfig: store.userState.themeConfig,
      }
    },
    'SettingsPane',
  )

  const isMyProject = useIsMyProject(projectId)

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

  return (
    <FlexColumn
      id='leftPaneSettings'
      key='leftPaneSettings'
      style={{
        display: 'relative',
        alignItems: 'stretch',
        paddingBottom: 50,
      }}
    >
      <Section>
        <SectionTitleRow minimised={false} toggleMinimised={NO_OP}>
          <Title style={{ flexGrow: 1 }}>Settings</Title>
        </SectionTitleRow>
        <UIGridRow
          style={{ marginTop: 16, color: colorTheme.fg1.value }}
          padded
          variant='<---1fr--->|------172px-------|'
        >
          <H2> Project </H2>
        </UIGridRow>

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
        <SectionBodyArea minimised={false}>
          {/** Theme Toggle: */}
          <UIGridRow
            style={{ color: colorTheme.fg1.value, marginTop: 16 }}
            padded
            variant='<---1fr--->|------172px-------|'
          >
            <H2> Theme </H2>
          </UIGridRow>
          <UIGridRow padded variant='<---1fr--->|------172px-------|'>
            <span style={{ color: colorTheme.fg2.value }}>Application </span>
            <PopupList
              value={theme}
              options={themeOptions}
              onSubmitValue={handleSubmitValueTheme}
              style={{ width: 150 }}
            />
          </UIGridRow>
          <UIGridRow padded variant='<---1fr--->|------172px-------|'>
            <span style={{ color: colorTheme.fg2.value }}>VSCode </span>
            <Subdued>Change from code editor.</Subdued>
          </UIGridRow>
          <UIGridRow
            style={{ color: colorTheme.fg1.value, marginTop: 16 }}
            padded
            variant='<---1fr--->|------172px-------|'
          >
            <H2>VSCode </H2>
          </UIGridRow>

          <FlexRow>
            <div
              style={{
                height: 'initial',
                minHeight: UtopiaTheme.layout.rowHeight.normal,
                alignItems: 'flex-start',
                paddingTop: 8,
                paddingLeft: 8,
                paddingRight: 8,
                paddingBottom: 8,
                whiteSpace: 'pre-wrap',
                letterSpacing: 0.1,
                lineHeight: '17px',
                fontSize: '11px',
              }}
            >
              <Subdued>
                Settings can be changed in the code editor by opening the command palette and
                searching for Settings (CMD+P on Mac, CTRL+P on Windows / Linux). We store settings
                with each project.
              </Subdued>
            </div>
          </FlexRow>
        </SectionBodyArea>
      </Section>
    </FlexColumn>
  )
})
