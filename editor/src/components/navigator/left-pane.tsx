/** @jsxRuntime classic */
/** @jsx jsx */
/** @jsxFrag React.Fragment */
import { css, jsx, keyframes } from '@emotion/react'
import styled from '@emotion/styled'
import React, { ChangeEvent } from 'react'
import {
  fetchProjectMetadata,
  projectEditorURL,
  projectURL,
  thumbnailURL,
} from '../../common/server'
import { useGetProjectMetadata, useIsMyProject } from '../common/server-hooks'
import { getAllUniqueUids } from '../../core/model/element-template-utils'
import { getUtopiaJSXComponentsFromSuccess } from '../../core/model/project-file-utils'
import { isParseSuccess, isTextFile, ProjectFile } from '../../core/shared/project-file-types'
import { NO_OP } from '../../core/shared/utils'
import { auth0Url, BASE_URL, FLOATING_PREVIEW_BASE_URL } from '../../common/env-vars'
import { shareURLForProject } from '../../core/shared/utils'
import Utils from '../../utils/utils'
import {
  useColorTheme,
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
  H2,
  PopupList,
  Icons,
  Avatar,
} from '../../uuiui'
import { SelectOption, User } from '../../uuiui-deps'
import { setFocus } from '../common/actions'
import { EditorDispatch, LoginState } from '../editor/action-types'
import * as EditorActions from '../editor/actions/action-creators'
import {
  clearSelection,
  regenerateThumbnail,
  setProjectName,
  setProjectDescription,
} from '../editor/actions/action-creators'
import { InsertMenu } from '../editor/insertmenu'
import {
  DerivedState,
  EditorState,
  githubOperationPrettyName,
  isGithubCommishing,
  isGithubLoadingBranch,
  LeftMenuTab,
} from '../editor/store/editor-state'
import { useEditorState } from '../editor/store/store-hook'
import { FileBrowser } from '../filebrowser/filebrowser'
import { UIGridRow } from '../inspector/widgets/ui-grid-row'
import { DependencyList } from './dependency-list'
import { GenericExternalResourcesList } from './external-resources/generic-external-resources-list'
import { GoogleFontsResourcesList } from './external-resources/google-fonts-resources-list'
import { StoryboardFilePath } from '../editor/store/editor-state'
import { getContentsTreeFileFromString } from '../assets'
import { Link } from '../../uuiui/link'
import { useTriggerForkProject } from '../editor/persistence-hooks'
import urljoin from 'url-join'
import {
  getBranchContent,
  getBranchesForGithubRepository,
  GetBranchesResponse,
  parseGithubProjectString,
} from '../../core/shared/github'
import { startGithubAuthentication } from '../../utils/github-auth'
import { when } from '../../utils/react-conditionals'
import { forceNotNull } from '../../core/shared/optional-utils'

export interface LeftPaneProps {
  editorState: EditorState
  derivedState: DerivedState
  editorDispatch: EditorDispatch
  loginState: LoginState
}

export const LeftPaneComponentId = 'left-pane'

export const LeftPaneOverflowScrollId = 'left-pane-overflow-scroll'

export const LeftPaneComponent = React.memo(() => {
  const selectedTab = useEditorState(
    (store) => store.editor.leftMenu.selectedTab,
    'LeftPaneComponent selectedTab',
  )
  const projectId = useEditorState((store) => store.editor.id, 'LeftPaneComponent projectId')
  const dispatch = useEditorState((store) => store.dispatch, 'LeftPaneComponent dispatch')

  const isMyProject = useIsMyProject(projectId)

  const loggedIn = useEditorState(
    (store) => User.isLoggedIn(store.userState.loginState),
    'LeftPaneComponent loggedIn',
  )

  const colorTheme = useColorTheme()

  return (
    <div
      id={LeftPaneComponentId}
      className='leftPane'
      style={{
        height: '100%',
        position: 'relative',
        backgroundColor: colorTheme.leftPaneBackground.value,
        paddingLeft: 4,
      }}
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
        {isMyProject === 'yes' ? null : <ForksGiven />}
        {selectedTab === LeftMenuTab.Project && isMyProject === 'yes' ? <ProjectPane /> : null}
        {selectedTab === LeftMenuTab.Storyboards ? <StoryboardsPane /> : null}
        {selectedTab === LeftMenuTab.Contents ? <ContentsPane /> : null}
        {selectedTab === LeftMenuTab.Settings ? <SettingsPane /> : null}
        {selectedTab === LeftMenuTab.Sharing ? <SharingPane /> : null}
        {selectedTab === LeftMenuTab.Github ? <GithubPane /> : null}
        {loggedIn ? null : <LoggedOutPane />}
      </div>
    </div>
  )
})

const ForksGiven = React.memo(() => {
  const colorTheme = useColorTheme()

  const { id, projectName, description, isLoggedIn, forkedFrom } = useEditorState((store) => {
    return {
      dispatch: store.dispatch,
      id: store.editor.id,
      projectName: store.editor.projectName,
      description: store.editor.projectDescription,
      isLoggedIn: User.isLoggedIn(store.userState.loginState),
      forkedFrom: store.editor.forkedFromProjectId,
    }
  }, 'ForkPanel')

  const projectOwnerMetadata = useGetProjectMetadata(id)
  const forkedFromMetadata = useGetProjectMetadata(forkedFrom)

  const onClickLoginNewTab = React.useCallback(() => {
    window.open(auth0Url('auto-close'), '_blank')
  }, [])

  const forkedFromText =
    forkedFrom == null ? null : (
      <React.Fragment>
        Forked from <Link href={projectEditorURL(forkedFrom)}>{forkedFromMetadata?.title}</Link>
      </React.Fragment>
    )

  return (
    <Section data-name='Fork' tabIndex={-1}>
      <SectionTitleRow minimised={false}>
        <FlexRow flexGrow={1} style={{ position: 'relative' }}>
          <Title>Project</Title>
        </FlexRow>
      </SectionTitleRow>
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
          <div>
            <span
              style={{
                paddingLeft: 4,
                paddingRight: 4,
                paddingTop: 2,
                paddingBottom: 2,
                background: colorTheme.primary.value,
                color: colorTheme.neutralInvertedForeground.value,
                borderRadius: 2,
              }}
            >
              <b>{projectName}</b>&nbsp;
            </span>
            &nbsp;
            <Subdued>{id}</Subdued>
          </div>
          <p>{description}</p>
        </UIGridRow>
        <UIGridRow
          padded
          variant='|--32px--|<--------auto-------->'
          style={{ gap: 12, marginTop: 8 }}
        >
          <div
            role='avatar'
            style={{
              width: 28,
              height: 28,
              borderRadius: '50%',
              boxShadow: `inset 0px 0px 0px 1px ${colorTheme.subduedForeground.o(50).value}`,
              background: colorTheme.subtleBackground.value,
            }}
          >
            <Avatar
              isLoggedIn={isLoggedIn}
              userPicture={projectOwnerMetadata?.ownerPicture ?? null}
              size={28}
            />
          </div>

          <div style={{ whiteSpace: 'normal' }}>
            Created by <b>{projectOwnerMetadata?.ownerName ?? ''}</b>
            <br />
            {forkedFromText}
          </div>
        </UIGridRow>

        <UIGridRow style={{ gap: 8, marginTop: 8 }} padded variant='<--1fr--><--1fr-->'>
          <ForkButton />
          {isLoggedIn ? null : (
            <Button
              outline
              highlight
              style={{
                height: 24,
              }}
              onClick={onClickLoginNewTab}
            >
              <b>Sign in</b>&nbsp;to edit&nbsp;
              <Icons.ExternalLinkSmaller />
            </Button>
          )}
        </UIGridRow>
      </SectionBodyArea>
    </Section>
  )
})

const ForkButton = React.memo(() => {
  const onClickOnForkProject = useTriggerForkProject()

  return (
    <Button
      primary
      highlight
      style={{
        height: 24,
        backgroundImage: 'linear-gradient(3deg, #92ABFF 0%, #1FCCB7 99%)',
        boxShadow: 'inset 0 0 0 1px rgba(94,94,94,0.20)',
        borderRadius: 2,
      }}
      onClick={onClickOnForkProject}
    >
      <b>Fork</b>&nbsp;this project
    </Button>
  )
})

const LoggedOutPane = React.memo(() => {
  const colorTheme = useColorTheme()
  return (
    <Section data-name='Storyboards' tabIndex={-1}>
      <SectionTitleRow minimised={false}>
        <FlexRow flexGrow={1} style={{ position: 'relative' }}>
          <Title>Sign in to</Title>
        </FlexRow>
      </SectionTitleRow>
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
          <ul style={{ paddingLeft: 16 }}>
            <li>Design and code from anywhere</li>
            <li>Save and preview your projects</li>
            <li>Use custom assets, fonts, and more</li>
          </ul>
        </UIGridRow>
        <UIGridRow style={{ gap: 8 }} padded variant='<--1fr--><--1fr-->'>
          <Button primary highlight>
            <b>Sign In</b>&nbsp;
            <Icons.ExternalLinkSmaller color='on-highlight-main' />
          </Button>
          <Subdued>Free and Open Source</Subdued>
        </UIGridRow>
      </SectionBodyArea>
    </Section>
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
  // TOODO colortheme
  backgroundColor: 'hsl(0,0%,90%)',
  boxShadow: props.selected ? 'inset 0px 0px 0px 2px #007AFF' : undefined,
  '&:hover': {
    boxShadow: 'inset 0px 0px 0px 2px  #007AFF',
  },
}))

const StoryboardsPane = React.memo(() => {
  const { dispatch, openFile, projectContents, isCanvasVisible } = useEditorState((store) => {
    return {
      dispatch: store.dispatch,
      openFile: store.editor.canvas.openFile?.filename,
      projectContents: store.editor.projectContents,
      isCanvasVisible: store.editor.canvas.visible,
    }
  }, 'StoryboardsPane')

  const colorTheme = useColorTheme()
  const handleStoryboardAdd = React.useCallback(() => {
    dispatch([EditorActions.addStoryboardFile()])
  }, [dispatch])

  const handleStoryboardListItemClick = React.useCallback(() => {
    dispatch([EditorActions.setPanelVisibility('canvas', true)])
  }, [dispatch])

  const noStoryboardFileAvailable =
    getContentsTreeFileFromString(projectContents, StoryboardFilePath) == null

  const storyboardList = noStoryboardFileAvailable ? [] : [StoryboardFilePath]

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
                style={{ background: colorTheme.secondaryBackground.value }}
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

const ContentsPane = React.memo(() => {
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

const SettingsPane = React.memo(() => {
  const { dispatch } = useEditorState((store) => {
    return {
      dispatch: store.dispatch,
    }
  }, 'ProjectPane')
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

const SharingPane = React.memo(() => {
  const [temporaryCopySuccess, setTemporaryCopySuccess] = React.useState(false)
  const { projectId, projectName } = useEditorState((store) => {
    return {
      projectId: store.editor.id,
      projectName: store.editor.projectName,
    }
  }, 'Menubar')
  const previewURL =
    projectId == null ? '' : shareURLForProject(FLOATING_PREVIEW_BASE_URL, projectId, projectName)

  const handleCopyProjectURL = React.useCallback(() => {
    void window.navigator.clipboard.writeText(previewURL)
    setTemporaryCopySuccess(true)
    setTimeout(() => {
      setTemporaryCopySuccess(false)
    }, 1500)
  }, [previewURL])

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

const GithubSpinner = () => {
  const anim = keyframes`
    from {
      transform: rotate(0deg);
    }
    to {
      transform: rotate(360deg);
    }
  `

  return (
    <FlexColumn>
      <svg
        xmlns='http://www.w3.org/2000/svg'
        width='16'
        height='16'
        viewBox='0 0 24 24'
        fill='none'
        stroke='#999'
        strokeWidth='2'
        strokeLinecap='round'
        strokeLinejoin='round'
        css={css`
          animation: ${anim} 1s linear infinite;
        `}
      >
        <line x1='12' y1='2' x2='12' y2='6'></line>
        <line x1='12' y1='18' x2='12' y2='22'></line>
        <line x1='4.93' y1='4.93' x2='7.76' y2='7.76'></line>
        <line x1='16.24' y1='16.24' x2='19.07' y2='19.07'></line>
        <line x1='2' y1='12' x2='6' y2='12'></line>
        <line x1='18' y1='12' x2='22' y2='12'></line>
        <line x1='4.93' y1='19.07' x2='7.76' y2='16.24'></line>
        <line x1='16.24' y1='7.76' x2='19.07' y2='4.93'></line>
      </svg>
    </FlexColumn>
  )
}

const GithubPane = React.memo(() => {
  const [importGithubRepoStr, setImportGithubRepoStr] = React.useState('')
  const parsedImportRepo = parseGithubProjectString(importGithubRepoStr)
  const dispatch = useEditorState((store) => store.dispatch, 'GithubPane dispatch')
  const projectID = useEditorState((store) => store.editor.id, 'GithubPane projectID')
  const githubOperations = useEditorState(
    (store) => store.editor.githubOperations,
    'Github operations',
  )

  const githubWorking = React.useMemo(() => {
    return githubOperations.length > 0
  }, [githubOperations])

  const storedTargetGithubRepo = useEditorState((store) => {
    const repo = store.editor.githubSettings.targetRepository
    if (repo == null) {
      return undefined
    } else {
      return `https://github.com/${repo.owner}/${repo.repository}`
    }
  }, 'GithubPane storedTargetGithubRepo')

  const onStartImport = React.useCallback(() => {
    if (parsedImportRepo != null) {
      const { owner, repository } = parsedImportRepo

      const url = new URL(urljoin(BASE_URL, 'p'))
      url.searchParams.set('github_owner', owner)
      url.searchParams.set('github_repo', repository)

      window.open(url.toString())
    }
  }, [parsedImportRepo])

  const onChange = React.useCallback(
    (e: React.ChangeEvent<HTMLInputElement>) => {
      setImportGithubRepoStr(e.currentTarget.value)
    },
    [setImportGithubRepoStr],
  )

  const githubAuthenticated = useEditorState((store) => {
    return store.userState.githubState.authenticated
  }, 'GithubPane githubAuthenticated')

  const triggerAuthentication = React.useCallback(() => {
    void startGithubAuthentication(dispatch)
  }, [dispatch])

  const [urlToImportFrom, setURLToImportFrom] = React.useState<string | null>(null)

  const importFromURLChange = React.useCallback(
    (changeEvent: React.ChangeEvent<HTMLInputElement>) => {
      setURLToImportFrom(changeEvent.currentTarget.value)
    },
    [setURLToImportFrom],
  )

  const triggerImportFromURL = React.useCallback(() => {
    if (urlToImportFrom != null) {
      const url = new URL(urljoin(BASE_URL, 'p'))
      url.searchParams.set('import_url', urlToImportFrom)

      window.open(url.toString())
    }
  }, [urlToImportFrom])

  const onFocus = React.useCallback(
    (event: React.FocusEvent<HTMLElement>) => {
      dispatch([setFocus('githuboptions')], 'everyone')
    },
    [dispatch],
  )

  const [targetRepository, setTargetRepository] = React.useState<string | undefined>(
    storedTargetGithubRepo,
  )
  const parsedTargetRepository = React.useMemo(() => {
    if (targetRepository == null) {
      return null
    } else {
      return parseGithubProjectString(targetRepository)
    }
  }, [targetRepository])

  const onChangeTargetRepository = React.useCallback(
    (changeEvent: React.ChangeEvent<HTMLInputElement>) => {
      setTargetRepository(changeEvent.currentTarget.value)
    },
    [setTargetRepository],
  )

  const triggerSaveToGithub = React.useCallback(() => {
    if (parsedTargetRepository != null) {
      dispatch([EditorActions.saveToGithub(parsedTargetRepository)], 'everyone')
    }
  }, [dispatch, parsedTargetRepository])

  const [branchesForRepository, setBranchesForRepository] =
    React.useState<GetBranchesResponse | null>(null)

  React.useEffect(() => {
    if (parsedTargetRepository != null) {
      void getBranchesForGithubRepository(dispatch, parsedTargetRepository).then((result) => {
        setBranchesForRepository(result)
      })
    }
  }, [parsedTargetRepository, dispatch])

  const branchesUI = React.useMemo(() => {
    if (branchesForRepository == null) {
      return null
    } else {
      switch (branchesForRepository.type) {
        case 'FAILURE':
          return <span>{branchesForRepository.failureReason}</span>
        case 'SUCCESS':
          return (
            <>
              {when(
                branchesForRepository.branches.length > 0,
                <UIGridRow padded variant='<--------auto-------->|--45px--|'>
                  <span>NOTE: These will replace the current project contents.</span>
                </UIGridRow>,
              )}
              {when(
                branchesForRepository.branches.length > 0,
                <div
                  style={{ overflowY: 'auto', height: UtopiaTheme.layout.rowHeight.normal * 11.5 }}
                >
                  {branchesForRepository.branches.map((branch, index) => {
                    function loadContentForBranch() {
                      if (parsedTargetRepository != null) {
                        void getBranchContent(
                          dispatch,
                          parsedTargetRepository,
                          forceNotNull('Should have a project ID.', projectID),
                          branch.name,
                        )
                      }
                    }
                    return (
                      <UIGridRow key={index} padded variant='<--------auto-------->|--45px--|'>
                        <span>{branch.name}</span>
                        <Button
                          spotlight
                          highlight
                          onMouseUp={loadContentForBranch}
                          disabled={githubWorking}
                        >
                          {isGithubLoadingBranch(githubOperations, branch.name) ? (
                            <GithubSpinner />
                          ) : (
                            'Load'
                          )}
                        </Button>
                      </UIGridRow>
                    )
                  })}
                </div>,
              )}
            </>
          )

        default:
          const _exhaustiveCheck: never = branchesForRepository
          throw new Error(`Unhandled branches value ${JSON.stringify(branchesForRepository)}`)
      }
    }
  }, [
    branchesForRepository,
    parsedTargetRepository,
    dispatch,
    projectID,
    githubWorking,
    githubOperations,
  ])

  return (
    <FlexColumn
      id='leftPaneGithub'
      key='leftPaneGithub'
      style={{
        display: 'relative',
        alignItems: 'stretch',
        paddingBottom: 50,
      }}
      onFocus={onFocus}
    >
      <Section>
        <SectionTitleRow minimised={false}>
          <Title style={{ flexGrow: 1 }}>Github</Title>
          {githubWorking && (
            <FlexRow style={{ gap: 4 }}>
              <GithubSpinner />
              <span>{githubOperationPrettyName(githubOperations[0])}…</span>
            </FlexRow>
          )}
        </SectionTitleRow>
        <SectionBodyArea minimised={false}>
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
            {githubAuthenticated ? 'Authenticated With Github' : 'Not Authenticated With Github'}
          </div>
          <UIGridRow padded variant='<--------auto-------->|--45px--|'>
            <Button
              spotlight
              highlight
              disabled={githubAuthenticated}
              onMouseUp={triggerAuthentication}
            >
              Authenticate With Github
            </Button>
          </UIGridRow>
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
            You can import a new project from Github. It might take a few minutes, and will show up
            in a new tab.
          </div>
          <UIGridRow padded variant='<--------auto-------->|--45px--|'>
            <StringInput testId='importProject' value={importGithubRepoStr} onChange={onChange} />
            <Button
              spotlight
              highlight
              disabled={parsedImportRepo == null}
              onMouseUp={onStartImport}
            >
              Start
            </Button>
          </UIGridRow>

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
            Work with a Github repo if you have access to it.
          </div>
          <UIGridRow padded variant='<-------------1fr------------->'>
            <StringInput
              testId='saveToGithubInput'
              value={targetRepository}
              disabled={!githubAuthenticated}
              onChange={onChangeTargetRepository}
            />
          </UIGridRow>
          <UIGridRow padded variant='<-------------1fr------------->'>
            <Button
              spotlight
              highlight
              disabled={!githubAuthenticated || parsedTargetRepository == null || githubWorking}
              onMouseUp={triggerSaveToGithub}
            >
              {isGithubCommishing(githubOperations) ? <GithubSpinner /> : 'Save To Github'}
            </Button>
          </UIGridRow>
          {branchesUI}
        </SectionBodyArea>
      </Section>
      <Section>
        <SectionTitleRow minimised={false} toggleMinimised={NO_OP}>
          <Title style={{ flexGrow: 1 }}>Import From URL</Title>
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
              Import a project from an existing project based on its URL.
            </p>
          </div>

          <UIGridRow variant='<--------auto-------->|--45px--|' padded>
            <StringInput testId='import-from-url-input' onChange={importFromURLChange} />
            <Button spotlight highlight onClick={triggerImportFromURL}>
              Import
            </Button>
          </UIGridRow>
        </SectionBodyArea>
      </Section>
    </FlexColumn>
  )
})

export const InsertMenuPane = React.memo(() => {
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

const ProjectPane = React.memo(() => {
  const colorTheme = useColorTheme()
  const {
    dispatch,
    projectName,
    projectDescription,
    projectId,
    thumbnailLastGenerated,
    userState,
    focusedPanel,
    minimised,
    forkedFrom,
  } = useEditorState((store) => {
    return {
      dispatch: store.dispatch,
      projectName: store.editor.projectName,
      projectDescription: store.editor.projectDescription,
      projectId: store.editor.id,
      thumbnailLastGenerated: store.editor.thumbnailLastGenerated,
      userState: store.userState,
      focusedPanel: store.editor.focusedPanel,
      minimised: store.editor.projectSettings.minimised,
      forkedFrom: store.editor.forkedFromProjectId,
    }
  }, 'ProjectPane')

  const [name, changeProjectName] = React.useState(projectName)
  const [description, changeProjectDescription] = React.useState(projectDescription)
  const [requestingPreviewImage, setRequestingPreviewImage] = React.useState(false)

  const forkedFromMetadata = useGetProjectMetadata(forkedFrom)

  const forkedFromText =
    forkedFrom == null ? null : (
      <React.Fragment>
        Forked from <Link href={projectEditorURL(forkedFrom)}>{forkedFromMetadata?.title}</Link>
      </React.Fragment>
    )

  const toggleMinimised = React.useCallback(() => {
    dispatch([EditorActions.togglePanel('projectsettings')], 'leftpane')
  }, [dispatch])

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

  const onChangeProjectName = React.useCallback((event: React.ChangeEvent<HTMLInputElement>) => {
    changeProjectName(event.target.value)
  }, [])

  const onChangeProjectDescription = React.useCallback(
    (event: React.ChangeEvent<HTMLInputElement>) => {
      changeProjectDescription(event.target.value)
    },
    [],
  )

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
              <FlexColumn>
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
                    These help you organise your projects. We also use them when you embed or share
                    your project on social media and chat apps.
                  </Subdued>
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
                          transition: requestingPreviewImage ? 'right 2.5s ease-in-out' : 'inherit',
                          position: 'absolute',
                          left: 0,
                          top: 0,
                          bottom: 0,
                          right: requestingPreviewImage ? 0 : '100%',
                          background: requestingPreviewImage
                            ? colorTheme.primary.value
                            : 'transparent',
                          content: '""',
                        },
                      }}
                    >
                      {requestingPreviewImage ? 'Refreshing' : 'Refresh'}
                    </Button>
                  </FlexColumn>
                </UIGridRow>

                <UIGridRow
                  style={{ marginTop: 16 }}
                  padded
                  variant='<-------------1fr------------->'
                >
                  <Subdued>{forkedFromText}</Subdued>
                </UIGridRow>

                <UIGridRow
                  style={{ marginTop: 16 }}
                  padded
                  variant='<-------------1fr------------->'
                >
                  <ForkButton />
                </UIGridRow>
              </FlexColumn>
            </SectionBodyArea>
          </Section>
        )}
      </FlexColumn>
    </FlexColumn>
  )
})
