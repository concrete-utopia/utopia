/** @jsx jsx */
import { jsx } from '@emotion/react'
import styled from '@emotion/styled'
import * as React from 'react'
import { thumbnailURL } from '../../common/server'
import { getAllUniqueUids } from '../../core/model/element-template-utils'
import { getUtopiaJSXComponentsFromSuccess } from '../../core/model/project-file-utils'
import { isParseSuccess, isTextFile, ProjectFile } from '../../core/shared/project-file-types'
import { NO_OP } from '../../core/shared/utils'
import { FLOATING_PREVIEW_BASE_URL } from '../../common/env-vars'
import { shareURLForProject } from '../../core/shared/utils'
import Utils from '../../utils/utils'
import {
  colorTheme,
  UtopiaTheme,
  FlexColumn,
  Section,
  SectionTitleRow,
  FlexRow,
  Title,
  SectionBodyArea,
  Button,
  MenuIcons,
  StringInput,
  Subdued,
  UIRow,
} from '../../uuiui'
import { betterReactMemo } from '../../uuiui-deps'
import { setFocus } from '../common/actions'
import { EditorDispatch, LoginState } from '../editor/action-types'
import * as EditorActions from '../editor/actions/action-creators'
import {
  clearSelection,
  regenerateThumbnail,
  setProjectName,
} from '../editor/actions/action-creators'
import { InsertMenu } from '../editor/insertmenu'
import { DerivedState, EditorState } from '../editor/store/editor-state'
import { useEditorState } from '../editor/store/store-hook'
import { closeTextEditorIfPresent } from '../editor/text-editor'
import { FileBrowser } from '../filebrowser/filebrowser'
import { UIGridRow } from '../inspector/widgets/ui-grid-row'
import { DependencyList } from './dependency-list'
import { GenericExternalResourcesList } from './external-resources/generic-external-resources-list'
import { GoogleFontsResourcesList } from './external-resources/google-fonts-resources-list'
import { StoryboardFilePath } from '../editor/store/editor-state'
import { getContentsTreeFileFromString } from '../assets'
export interface LeftPaneProps {
  editorState: EditorState
  derivedState: DerivedState
  editorDispatch: EditorDispatch
  loginState: LoginState
}

export const enum LeftMenuTab {
  UIInsert = 'ui-insert',
  Project = 'project',
  Storyboards = 'storyboards',
  Contents = 'contents',
  Settings = 'settings',
  Sharing = 'sharing',
  Github = 'github',
}

export function updateSelectedLeftMenuTab(editorState: EditorState, tab: LeftMenuTab): EditorState {
  return {
    ...editorState,
    leftMenu: {
      ...editorState.leftMenu,
      selectedTab: tab,
    },
  }
}

export function updateLeftMenuExpanded(editorState: EditorState, expanded: boolean): EditorState {
  return {
    ...editorState,
    leftMenu: {
      ...editorState.leftMenu,
      expanded: expanded,
    },
  }
}

export function setLeftMenuTabFromFocusedPanel(editorState: EditorState): EditorState {
  switch (editorState.focusedPanel) {
    case 'misccodeeditor':
      return updateSelectedLeftMenuTab(editorState, LeftMenuTab.Contents)
    case 'inspector':
    case 'canvas':
    case 'codeEditor':
    default:
      return editorState
  }
}

function getExistingUIDs(projectFile: ProjectFile | null): Array<string> {
  if (projectFile == null) {
    return []
  } else {
    if (isTextFile(projectFile)) {
      if (isParseSuccess(projectFile.fileContents.parsed)) {
        const components = getUtopiaJSXComponentsFromSuccess(projectFile.fileContents.parsed)
        return getAllUniqueUids(components)
      } else {
        return []
      }
    } else {
      return []
    }
  }
}

export const existingUIDs = Utils.memoize(getExistingUIDs)

export const LeftPaneMinimumWidth = 5

export const LeftPaneDefaultWidth = 260

export const LeftPaneComponentId = 'left-pane'

export const LeftPaneOverflowScrollId = 'left-pane-overflow-scroll'

export const LeftPaneComponent = betterReactMemo('LeftPaneComponent', () => {
  const selectedTab = useEditorState(
    (store) => store.editor.leftMenu.selectedTab,
    'LeftPaneComponent selectedTab',
  )
  const dispatch = useEditorState((store) => store.dispatch, 'LeftPaneComponent dispatch')

  return (
    <div
      id={LeftPaneComponentId}
      className='leftPane'
      style={{
        height: '100%',
        position: 'relative',
        backgroundColor: colorTheme.leftPaneBackground.value,
        borderRight: `1px solid ${colorTheme.subduedBorder.value}`,
        paddingLeft: 4,
      }}
      onMouseDown={() => closeTextEditorIfPresent()}
    >
      <div
        id={LeftPaneOverflowScrollId}
        className='overflow-y-scroll'
        style={{
          height: '100%',
          flexGrow: 1,
        }}
        onMouseDown={(mouseEvent: React.MouseEvent<HTMLDivElement>) => {
          if (mouseEvent.target instanceof HTMLDivElement) {
            if (mouseEvent.target.id === LeftPaneOverflowScrollId) {
              dispatch([clearSelection()])
            }
          }
        }}
      >
        {selectedTab === LeftMenuTab.Project ? <ProjectPane /> : null}
        {selectedTab === LeftMenuTab.Storyboards ? <StoryboardsPane /> : null}
        {selectedTab === LeftMenuTab.Contents ? <ContentsPane /> : null}
        {selectedTab === LeftMenuTab.Settings ? <SettingsPane /> : null}
        {selectedTab === LeftMenuTab.Sharing ? <SharingPane /> : null}
        {selectedTab === LeftMenuTab.Github ? <GithubPane /> : null}
      </div>
    </div>
  )
})

interface StoryboardListItemProps {
  selected: boolean
}

const StoryboardListItem = styled.div<StoryboardListItemProps>((props) => ({
  flex: '96px 0 1',
  borderRadius: 4,
  padding: 8,
  display: 'flex',
  flexDirection: 'column',
  justifyContent: 'flex-end',
  cursor: 'pointer',
  fontWeight: 500,
  backgroundColor: 'hsl(0,0%,90%)',
  boxShadow: props.selected ? 'inset 0px 0px 0px 2px #007AFF' : undefined,
  '&:hover': {
    boxShadow: 'inset 0px 0px 0px 2px  #007AFF',
  },
}))

const StoryboardsPane = betterReactMemo('StoryboardsPane', () => {
  const { dispatch, openFile, projectContents, isCanvasVisible } = useEditorState((store) => {
    return {
      dispatch: store.dispatch,
      openFile: store.editor.canvas.openFile?.filename,
      projectContents: store.editor.projectContents,
      isCanvasVisible: store.editor.canvas.visible,
    }
  }, 'StoryboardsPane')

  const handleStoryboardAdd = React.useCallback(() => {
    dispatch([EditorActions.addStoryboardFile()])
  }, [dispatch])

  const handleStoryboardListItemClick = React.useCallback(() => {
    dispatch([EditorActions.setPanelVisibility('canvas', true)])
  }, [dispatch])

  const storyboardList = [StoryboardFilePath]

  const noStoryboardFileAvailable =
    getContentsTreeFileFromString(projectContents, StoryboardFilePath) == null

  return (
    <FlexColumn
      id='leftPaneStoryboards'
      key='leftPaneStoryboards'
      style={{
        display: 'relative',
        alignItems: 'stretch',
        paddingBottom: 50,
      }}
    >
      <Section data-name='Storyboards' tabIndex={-1}>
        <SectionTitleRow minimised={false}>
          <FlexRow flexGrow={1} style={{ position: 'relative' }}>
            <Title>Storyboards</Title>
          </FlexRow>
        </SectionTitleRow>
        <SectionBodyArea minimised={false}>
          <FlexColumn style={{ paddingLeft: 8, paddingRight: 8, gap: 16 }}>
            <UIGridRow
              padded
              variant='|--32px--|<--------auto-------->'
              style={{
                height: 'inherit',
                wordWrap: 'normal',
                whiteSpace: 'normal',
                alignItems: 'flex-start',
                minHeight: 34,
                paddingTop: 8,
                paddingLeft: 8,
                paddingRight: 8,
                paddingBottom: 8,
                letterSpacing: 0.1,
                lineHeight: '17px',
                fontSize: '11px',
              }}
            >
              <MenuIcons.Pyramid style={{ marginTop: 2 }} />
              <span>Storyboards let you display and visually edit components.</span>
            </UIGridRow>

            {storyboardList.map((item) => (
              <StoryboardListItem
                selected={isCanvasVisible && openFile === item}
                style={{ background: UtopiaTheme.color.secondaryBackground.value }}
                key='mainStoryboard'
                onClick={handleStoryboardListItemClick}
              >
                <div>
                  Storyboard Label
                  <br />
                  <span style={{ opacity: 0.5, marginLeft: 4 }}>{item}</span>
                </div>
              </StoryboardListItem>
            ))}
            {noStoryboardFileAvailable ? (
              <Button
                spotlight
                highlight
                style={{ height: 34, alignSelf: 'stretch', border: '1px dashed lightgrey' }}
                onClick={handleStoryboardAdd}
              >
                Add Storyboard
              </Button>
            ) : null}
            <div
              style={{
                height: 'initial',
                minHeight: 34,
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
              <Subdued>The storyboard location and name are fixed for now.</Subdued>
            </div>
          </FlexColumn>
        </SectionBodyArea>
      </Section>
    </FlexColumn>
  )
})

const ContentsPane = betterReactMemo('ProjectStructurePane', () => {
  return (
    <FlexColumn
      id='leftPaneContents'
      key='leftPaneContents'
      style={{
        display: 'relative',
        alignItems: 'stretch',
        paddingBottom: 50,
      }}
    >
      <FileBrowser />
      <DependencyList />
      <GenericExternalResourcesList />
      <GoogleFontsResourcesList />
    </FlexColumn>
  )
})

const SettingsPane = betterReactMemo('SettingsPane', () => {
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
        <SectionBodyArea minimised={false}>
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
                Settings can be changed in the code editor by opening the comand palette and
                searching for Settings (CMD+P on Mac, CTRL+P on Mac / Linux). We store settings with
                each project.
              </Subdued>
            </div>
          </FlexRow>
        </SectionBodyArea>
      </Section>
    </FlexColumn>
  )
})

const SharingPane = betterReactMemo('SharingPane', () => {
  const [temporaryCopySuccess, setTemporaryCopySuccess] = React.useState(false)
  const { projectId, projectName } = useEditorState((store) => {
    return {
      projectId: store.editor.id,
      projectName: store.editor.projectName,
    }
  }, 'Menubar')
  const previewURL =
    projectId == null ? '' : shareURLForProject(FLOATING_PREVIEW_BASE_URL, projectId, projectName)

  const handleCopyProjectURL = () => {
    window.navigator.clipboard.writeText(previewURL)
    setTemporaryCopySuccess(true)
    setTimeout(() => {
      setTemporaryCopySuccess(false)
    }, 1500)
  }

  return (
    <FlexColumn
      id='leftPaneSharing'
      key='leftPaneSharing'
      css={{
        display: 'relative',
        alignItems: 'stretch',
        gap: 24,
        paddingBottom: 36,
      }}
    >
      <Section>
        <SectionTitleRow minimised={false} toggleMinimised={NO_OP}>
          <Title style={{ flexGrow: 1 }}>Sharing</Title>
        </SectionTitleRow>
        <SectionBodyArea minimised={false}>
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
            Share the URL to this project to let others view your code and fork it. Only you can
            make changes.
          </div>
        </SectionBodyArea>
      </Section>
      <Section>
        <SectionTitleRow minimised={false} toggleMinimised={NO_OP}>
          <Title style={{ flexGrow: 1 }}>Collaborate</Title>
        </SectionTitleRow>
        <SectionBodyArea minimised={false}>
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
            <p style={{ marginTop: 0, marginBottom: 12 }}>
              All Utopia projects are{' '}
              <span style={{ textDecoration: 'none', fontWeight: 600 }}>👁 public</span> by default.
              This means other people can discover and fork your project.
            </p>
            <p style={{ marginTop: 0, marginBottom: 12 }}>
              We'll introduce private and team projects soon.
            </p>
          </div>
        </SectionBodyArea>
      </Section>
      <Section>
        <SectionTitleRow minimised={false} toggleMinimised={NO_OP}>
          <Title style={{ flexGrow: 1 }}>Run and Embed</Title>
        </SectionTitleRow>
        <SectionBodyArea minimised={false}>
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
            You can share and embed a URL to the{' '}
            <a
              target='_blank'
              rel='noopener noreferrer'
              style={{ textDecoration: 'none', color: '#007AFF' }}
              href={previewURL}
            >
              running application
            </a>
            &nbsp;without the editor or design tool.
          </div>
          <UIGridRow variant='<--------auto-------->|--45px--|' padded>
            <StringInput testId='externalProjectURL' value={previewURL} readOnly />
            <Button
              spotlight
              highlight
              disabled={temporaryCopySuccess}
              onClick={handleCopyProjectURL}
            >
              {temporaryCopySuccess ? '✓' : 'Copy'}
            </Button>
          </UIGridRow>
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
            <Subdued>Old URLs will continue to work if you rename your project.</Subdued>
          </div>
        </SectionBodyArea>
      </Section>
    </FlexColumn>
  )
})

const GithubPane = betterReactMemo('GithubPane', () => {
  return (
    <FlexColumn
      id='leftPaneGithub'
      key='leftPaneGithub'
      style={{
        display: 'relative',
        alignItems: 'stretch',
        paddingBottom: 50,
      }}
    >
      <UIRow style={{ paddingLeft: 8, paddingRight: 8 }}>
        <Title>Github</Title>
      </UIRow>
      <div
        style={{
          height: 'initial',
          minHeight: 34,
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
        You can import a new project from Github. It might take a few minutes, and will show up in{' '}
        <a href='/projects'>your projects</a> (not here).
      </div>
      <UIGridRow padded variant='<--------auto-------->|--45px--|'>
        <StringInput testId='importProject' value='' />
        <Button spotlight highlight>
          Start
        </Button>
      </UIGridRow>
    </FlexColumn>
  )
})

export const InsertMenuPane = betterReactMemo('InsertMenuPane', () => {
  const { dispatch, focusedPanel } = useEditorState((store) => {
    return {
      dispatch: store.dispatch,
      focusedPanel: store.editor.focusedPanel,
    }
  }, 'InsertMenuPane')

  const onFocus = React.useCallback(
    (event: React.FocusEvent<HTMLElement>) => {
      if (focusedPanel !== 'insertmenu') {
        dispatch([setFocus('insertmenu')], 'everyone')
      }
    },
    [dispatch, focusedPanel],
  )

  return (
    <Section data-name='InsertMenu' onFocus={onFocus} tabIndex={-1} style={{ width: '100%' }}>
      <SectionTitleRow minimised={false} toggleMinimised={NO_OP}>
        <FlexRow flexGrow={1} style={{ position: 'relative' }}>
          <Title>Insert</Title>
        </FlexRow>
      </SectionTitleRow>
      <SectionBodyArea
        minimised={false}
        style={{ paddingLeft: 8, paddingRight: 8, overflow: 'auto' }}
      >
        <InsertMenu />
      </SectionBodyArea>
    </Section>
  )
})

const ProjectPane = betterReactMemo('ProjectSettingsPanel', () => {
  const {
    dispatch,
    projectName,
    projectId,
    thumbnailLastGenerated,
    userState,
    focusedPanel,
    minimised,
  } = useEditorState((store) => {
    return {
      dispatch: store.dispatch,
      projectName: store.editor.projectName,
      projectId: store.editor.id,
      thumbnailLastGenerated: store.editor.thumbnailLastGenerated,
      userState: store.userState,
      focusedPanel: store.editor.focusedPanel,
      minimised: store.editor.projectSettings.minimised,
    }
  }, 'ProjectSettingsPanel')

  const [name, changeProjectName] = React.useState(projectName)
  const [requestingPreviewImage, setRequestingPreviewImage] = React.useState(false)

  const toggleMinimised = React.useCallback(() => {
    dispatch([EditorActions.togglePanel('projectsettings')], 'leftpane')
  }, [dispatch])

  const updateProjectName = React.useCallback(
    (newProjectName: string) => {
      dispatch([setProjectName(newProjectName)])
    },
    [dispatch],
  )

  const triggerRegenerateThumbnail = React.useCallback(() => {
    setRequestingPreviewImage(true)
    dispatch([regenerateThumbnail()])
    setTimeout(() => setRequestingPreviewImage(false), 2000)
  }, [dispatch])

  const onFocus = React.useCallback(
    (event: React.FocusEvent<HTMLElement>) => {
      if (focusedPanel !== 'projectsettings') {
        dispatch([setFocus('projectsettings')], 'everyone')
      }
    },
    [dispatch, focusedPanel],
  )

  const handleBlur = React.useCallback(
    (e: React.ChangeEvent<HTMLInputElement>) => {
      updateProjectName(e.target.value)
    },
    [updateProjectName],
  )

  const handleKeyPress = React.useCallback((e: React.KeyboardEvent<HTMLInputElement>) => {
    if (e.key === 'Enter') {
      let target = e.target as HTMLInputElement
      target.blur()
    }
  }, [])

  const onChangeProjectName = React.useCallback((event: React.ChangeEvent<HTMLInputElement>) => {
    changeProjectName(event.target.value)
  }, [])

  const urlToRequest = `${thumbnailURL(projectId!)}?lastUpdated=${thumbnailLastGenerated}`

  return (
    <FlexColumn
      id='leftPaneContents'
      key='leftPaneContents'
      style={{
        display: 'relative',
        alignItems: 'stretch',
        paddingBottom: 50,
      }}
    >
      <FlexColumn
        id='leftPaneProject'
        key='leftPaneProjectTab'
        css={{
          display: 'relative',
          alignItems: 'stretch',
          gap: 24,
          paddingBottom: 36,
        }}
      >
        {projectId == null ? null : (
          <Section data-name='ProjectSettings' onFocus={onFocus} tabIndex={-1}>
            <SectionTitleRow minimised={minimised} toggleMinimised={toggleMinimised}>
              <FlexRow flexGrow={1} style={{ position: 'relative' }}>
                <Title style={{ flexGrow: 1 }}> Project </Title>
              </FlexRow>
            </SectionTitleRow>
            <SectionBodyArea minimised={minimised}>
              {userState.loginState.type === 'NOT_LOGGED_IN' ? (
                <span>Log in or sign up to see settings</span>
              ) : (
                <FlexColumn>
                  <SectionBodyArea minimised={false}>
                    <UIGridRow
                      padded
                      variant='<-------------1fr------------->'
                      style={{
                        height: 'inherit',
                        wordWrap: 'normal',
                        whiteSpace: 'normal',
                        alignItems: 'flex-start',
                        minHeight: 34,
                        paddingTop: 8,
                        paddingLeft: 8,
                        paddingRight: 8,
                        paddingBottom: 8,
                        letterSpacing: 0.1,
                        lineHeight: '17px',
                        fontSize: '11px',
                      }}
                    >
                      <Subdued>
                        These help you organise your projects. We also use them when you embed or
                        share your project on social media and chat apps.
                      </Subdued>
                    </UIGridRow>
                    <UIGridRow padded variant='<---1fr--->|------172px-------|'>
                      <span>Name</span>
                      <StringInput
                        testId='projectName'
                        value={name}
                        onChange={onChangeProjectName}
                        onKeyDown={handleKeyPress}
                        style={{ width: 150 }}
                        onBlur={handleBlur}
                      />
                    </UIGridRow>
                    <UIGridRow padded variant='<---1fr--->|------172px-------|'>
                      <span> Description </span>
                      <StringInput
                        testId='projectDescription'
                        value={projectName}
                        style={{ width: 150 }}
                      />
                    </UIGridRow>
                    <UIGridRow
                      padded
                      variant='<---1fr--->|------172px-------|'
                      style={{ alignItems: 'start', height: 'initial', paddingTop: 8 }}
                    >
                      <span> Preview </span>
                      <FlexColumn style={{ gap: 8 }}>
                        <div
                          css={{
                            boxShadow: `inset 0 0 0 1px ${colorTheme.secondaryBorder.value}`,
                            borderRadius: 1,
                            display: 'block',
                            justifySelf: 'stretch',
                            aspectRatio: '16 / 9',
                            backgroundImage: `url('${urlToRequest}')`,
                            backgroundSize: 'cover',
                            backgroundColor: colorTheme.canvasBackground.value,
                          }}
                        />
                        <Button
                          disabled={requestingPreviewImage}
                          spotlight
                          highlight
                          onClick={triggerRegenerateThumbnail}
                          css={{
                            position: 'relative',
                            textAlign: 'center',
                            '&:before': {
                              transition: requestingPreviewImage
                                ? 'right 2.5s ease-in-out'
                                : 'inherit',
                              position: 'absolute',
                              left: 0,
                              top: 0,
                              bottom: 0,
                              right: requestingPreviewImage ? 0 : '100%',
                              background: requestingPreviewImage
                                ? UtopiaTheme.color.primary.value
                                : 'transparent',
                              content: '""',
                            },
                          }}
                        >
                          {requestingPreviewImage ? 'Refreshing' : 'Refresh'}
                        </Button>
                      </FlexColumn>
                    </UIGridRow>
                  </SectionBodyArea>
                </FlexColumn>
              )}
            </SectionBodyArea>
          </Section>
        )}
      </FlexColumn>
    </FlexColumn>
  )
})
