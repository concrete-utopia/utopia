/** @jsxRuntime classic */
/** @jsx jsx */
/** @jsxFrag React.Fragment */
import { css, jsx, keyframes } from '@emotion/react'
import React from 'react'
import urljoin from 'url-join'
import { BASE_URL } from '../../../common/env-vars'
import {
  getBranchContent,
  getBranchesForGithubRepository,
  GetBranchesResponse,
  getUsersPublicGithubRepositories,
  githubFileChangesSelector,
  parseGithubProjectString,
  RepositoryEntry,
} from '../../../core/shared/github'
import { forceNotNull } from '../../../core/shared/optional-utils'
import { capitalize } from '../../../core/shared/string-utils'
import { NO_OP } from '../../../core/shared/utils'
import { startGithubAuthentication } from '../../../utils/github-auth'
import { when } from '../../../utils/react-conditionals'
import {
  Button,
  FlexColumn,
  FlexRow,
  Section,
  SectionBodyArea,
  SectionTitleRow,
  StringInput,
  Title,
  UtopiaTheme,
} from '../../../uuiui'
import { setFocus } from '../../common/actions'
import * as EditorActions from '../../editor/actions/action-creators'
import {
  githubOperationPrettyName,
  isGithubCommishing,
  isGithubLoadingBranch,
} from '../../editor/store/editor-state'
import { useEditorState } from '../../editor/store/store-hook'
import { GithubFileStatus } from '../../filebrowser/filebrowser'
import { getGithubFileStatusColor } from '../../filebrowser/fileitem'
import { UIGridRow } from '../../inspector/widgets/ui-grid-row'

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

const FileChanges = ({ files, type }: { files: string[]; type: GithubFileStatus }) => {
  if (files.length === 0) {
    return null
  }
  return (
    <div style={{ color: getGithubFileStatusColor(type), marginTop: 4, marginBottom: 4 }}>
      <div>
        {capitalize(type)} files ({files.length})
      </div>
      {files.map((f) => (
        <div key={f} style={{ marginLeft: 8 }}>
          &rarr; {f}
        </div>
      ))}
    </div>
  )
}

export const GithubPane = React.memo(() => {
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
      return `${repo.owner}/${repo.repository}`
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

  const [usersRepositories, setUsersRepositories] = React.useState<Array<string> | null>(null)

  const setUsersRepositoriesCallback = React.useCallback(
    (repositories: Array<RepositoryEntry>) => {
      setUsersRepositories(
        repositories.map((repository) => {
          return repository.fullName
        }),
      )
    },
    [setUsersRepositories],
  )

  React.useEffect(() => {
    if (githubAuthenticated) {
      void getUsersPublicGithubRepositories(dispatch, setUsersRepositoriesCallback)
    }
  }, [githubAuthenticated, dispatch, setUsersRepositoriesCallback])

  const onInputChangeTargetRepository = React.useCallback(
    (event: React.ChangeEvent<HTMLInputElement>) => {
      setTargetRepository(event.currentTarget.value)
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

  const githubFileChanges = useEditorState(githubFileChangesSelector, 'Github file changes')

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
            Connect this project to a Github repository. You can then import and export to the repo
            if you have the correct permissions. Please note we don’t support connecting to private
            repositories at the moment.
          </div>
          <UIGridRow padded variant='<-------------1fr------------->'>
            <StringInput
              placeholder={
                usersRepositories == null ? 'Loading repositories...' : 'owner/repository'
              }
              onChange={onInputChangeTargetRepository}
              list={'repositories-list'}
              id={'repositories-input'}
              testId={'repositories-input'}
              name={'repositories-input'}
              value={targetRepository}
            />
            {usersRepositories == null ? null : (
              <datalist id={'repositories-list'}>
                {usersRepositories.map((repo, index) => {
                  return <option key={`repo-${index}`} value={repo} />
                })}
              </datalist>
            )}
          </UIGridRow>
          {githubFileChanges != null ? (
            // Note: this is completely temporary until we finalize the design
            <UIGridRow padded variant='<-------------1fr------------->'>
              <FileChanges type='untracked' files={githubFileChanges.untracked} />
              <FileChanges type='modified' files={githubFileChanges.modified} />
              <FileChanges type='deleted' files={githubFileChanges.deleted} />
            </UIGridRow>
          ) : null}
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
