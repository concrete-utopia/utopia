/** @jsxRuntime classic */
/** @jsx jsx */
/** @jsxFrag React.Fragment */
import { jsx } from '@emotion/react'
import React, { useState } from 'react'
import { NO_OP } from '../../../core/shared/utils'
import {
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
import * as EditorActions from '../../editor/actions/action-creators'
import { setProjectDescription, setProjectName } from '../../editor/actions/action-creators'
import { useEditorState } from '../../editor/store/store-hook'
import { UIGridRow } from '../../inspector/widgets/ui-grid-row'

const themeOptions = [
  {
    label: 'Dark',
    value: 'dark',
  },
  {
    label: 'Light',
    value: 'light',
  },
]

export const SettingsPane = React.memo(() => {
  const { dispatch, userState, projectName, projectDescription } = useEditorState((store) => {
    return {
      dispatch: store.dispatch,
      userState: store.userState,
      projectName: store.editor.projectName,
      projectDescription: store.editor.projectDescription,
    }
  }, 'SettingsPane')

  const [theme, setTheme] = React.useState<SelectOption>({
    label: 'Light',
    value: 'light',
  })

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

  const handleBlurProjecDescription = React.useCallback(
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
        <UIGridRow style={{ marginTop: 16 }} padded variant='<---1fr--->|------172px-------|'>
          <H2> Project </H2>
        </UIGridRow>
        <UIGridRow padded variant='<---1fr--->|------172px-------|'>
          <span>Name</span>
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
          <span> Description </span>
          {userState.loginState.type !== 'LOGGED_IN' ? (
            <span>{description}</span>
          ) : (
            <StringInput
              testId='projectDescription'
              value={description}
              onChange={onChangeProjectDescription}
              onKeyDown={handleKeyPress}
              onBlur={handleBlurProjecDescription}
              style={{ width: 150 }}
            />
          )}
        </UIGridRow>
        <SectionBodyArea minimised={false}>
          {/** Theme Toggle: */}
          <UIGridRow style={{ marginTop: 16 }} padded variant='<---1fr--->|------172px-------|'>
            <H2> Theme </H2>
          </UIGridRow>
          <UIGridRow padded variant='<---1fr--->|------172px-------|'>
            <span>Application </span>
            <PopupList
              value={theme}
              options={themeOptions}
              onSubmitValue={handleSubmitValueTheme}
              style={{ width: 150 }}
            />
          </UIGridRow>
          <UIGridRow padded variant='<---1fr--->|------172px-------|'>
            <span>VSCode </span>
            <Subdued>Change from code editor.</Subdued>
          </UIGridRow>
          <UIGridRow style={{ marginTop: 16 }} padded variant='<---1fr--->|------172px-------|'>
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
