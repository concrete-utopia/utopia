/** @jsxRuntime classic */
/** @jsx jsx */
/** @jsxFrag React.Fragment */
import { jsx } from '@emotion/react'
import React from 'react'
import urljoin from 'url-join'
import { BASE_URL } from '../../../../common/env-vars'
import {
  updateProjectWithBranchContent,
  getBranchesForGithubRepository,
  GetBranchesResponse,
  githubFileChangesSelector,
  parseGithubProjectString,
  updateProjectAgainstGithub,
} from '../../../../core/shared/github'
import { forceNotNull } from '../../../../core/shared/optional-utils'
import { NO_OP } from '../../../../core/shared/utils'
import { startGithubAuthentication } from '../../../../utils/github-auth'
import { when } from '../../../../utils/react-conditionals'
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
} from '../../../../uuiui'
import { setFocus } from '../../../common/actions'
import * as EditorActions from '../../../editor/actions/action-creators'
import {
  githubOperationPrettyName,
  isGithubCommishing,
  isGithubLoadingBranch,
  isGithubUpdating,
} from '../../../editor/store/editor-state'
import { useEditorState } from '../../../editor/store/store-hook'
import { UIGridRow } from '../../../inspector/widgets/ui-grid-row'
import { Ellipsis, GithubFileChangesList } from './github-file-changes-list'
import { GithubSpinner } from './github-spinner'
import { RefreshIcon } from './refresh-icon'
import { RepositoryListing } from './repository-listing'

const GitBranchIcon = () => {
  return (
    <svg
      xmlns='http://www.w3.org/2000/svg'
      width='11'
      height='11'
      viewBox='0 0 24 24'
      fill='none'
      stroke='currentColor'
      strokeWidth='2'
      strokeLinecap='round'
      strokeLinejoin='round'
    >
      <line x1='6' y1='3' x2='6' y2='15'></line>
      <circle cx='18' cy='6' r='3'></circle>
      <circle cx='6' cy='18' r='3'></circle>
      <path d='M18 9a9 9 0 0 1-9 9'></path>
    </svg>
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
    return store.editor.githubSettings.targetRepository
  }, 'GithubPane storedTargetGithubRepo')

  const currentBranch = useEditorState(
    (store) => store.editor.githubSettings.branchName,
    'Github current branch',
  )

  const originCommit = useEditorState((store) => {
    return store.editor.githubSettings.originCommit
  }, 'GithubPane currentBranch')

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

  const triggerSaveToGithub = React.useCallback(() => {
    if (storedTargetGithubRepo != null) {
      dispatch([EditorActions.saveToGithub(storedTargetGithubRepo)], 'everyone')
    }
  }, [dispatch, storedTargetGithubRepo])

  const triggerUpdateAgainstGithub = React.useCallback(() => {
    if (storedTargetGithubRepo != null && currentBranch != null && originCommit != null) {
      void updateProjectAgainstGithub(dispatch, storedTargetGithubRepo, currentBranch, originCommit)
    }
  }, [dispatch, storedTargetGithubRepo, currentBranch, originCommit])

  const branchesForRepository = useEditorState(
    (store) => store.editor.githubData.branches,
    'Github repo branches',
  )

  const isLoadingBranches = React.useMemo(
    () => githubOperations.some((op) => op.name === 'listBranches'),
    [githubOperations],
  )

  const refreshBranches = React.useCallback(() => {
    if (storedTargetGithubRepo != null) {
      void getBranchesForGithubRepository(dispatch, storedTargetGithubRepo)
    }
  }, [dispatch, storedTargetGithubRepo])

  const loadBranchesUI = React.useMemo(() => {
    return (
      <>
        <UIGridRow padded variant='<----------1fr---------><-auto->'>
          <span style={{ fontWeight: 500 }}>Branches</span>
          <Button
            spotlight
            highlight
            style={{ padding: '0 6px' }}
            onMouseUp={refreshBranches}
            disabled={githubWorking}
          >
            {isLoadingBranches ? <GithubSpinner /> : <RefreshIcon />}
          </Button>
        </UIGridRow>
        {when(
          branchesForRepository.length > 0,
          <UIGridRow padded variant='<--------auto-------->|--45px--|'>
            <span>NOTE: These will replace the current project contents.</span>
          </UIGridRow>,
        )}
        {when(
          branchesForRepository.length > 0,
          <div style={{ overflowY: 'auto', height: UtopiaTheme.layout.rowHeight.normal * 11.5 }}>
            {branchesForRepository.map((branch, index) => {
              function loadContentForBranch() {
                if (storedTargetGithubRepo != null) {
                  void updateProjectWithBranchContent(
                    dispatch,
                    storedTargetGithubRepo,
                    branch.name,
                    originCommit,
                    false,
                  )
                }
              }
              return (
                <UIGridRow key={index} padded variant='<--------auto-------->|--45px--|'>
                  <Ellipsis style={{ fontWeight: branch.name === currentBranch ? 600 : 400 }}>
                    {when(currentBranch === branch.name, <span>&rarr; </span>)}
                    <span title={branch.name}>{branch.name}</span>
                  </Ellipsis>
                  <Button
                    spotlight
                    highlight
                    onMouseUp={loadContentForBranch}
                    disabled={githubWorking}
                  >
                    {isGithubLoadingBranch(
                      githubOperations,
                      branch.name,
                      storedTargetGithubRepo,
                    ) ? (
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
  }, [
    branchesForRepository,
    storedTargetGithubRepo,
    dispatch,
    githubWorking,
    githubOperations,
    currentBranch,
    isLoadingBranches,
    refreshBranches,
    originCommit,
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
          <RepositoryListing
            githubAuthenticated={githubAuthenticated}
            storedTargetGithubRepo={storedTargetGithubRepo}
          />
          {when(
            currentBranch != null,
            <>
              <UIGridRow padded variant='<-------------1fr------------->'>
                <Ellipsis style={{ display: 'flex', alignItems: 'center', gap: 4 }}>
                  <GitBranchIcon />
                  <span style={{ fontWeight: 600 }} title={currentBranch || undefined}>
                    {currentBranch}
                  </span>
                </Ellipsis>
              </UIGridRow>
              <GithubFileChangesList changes={githubFileChanges} githubWorking={githubWorking} />
            </>,
          )}
          <UIGridRow padded variant='<-------------1fr------------->'>
            <Button
              spotlight
              highlight
              disabled={!githubAuthenticated || storedTargetGithubRepo == null || githubWorking}
              onMouseUp={triggerSaveToGithub}
            >
              {isGithubCommishing(githubOperations) ? <GithubSpinner /> : 'Save To Github'}
            </Button>
          </UIGridRow>
          <UIGridRow padded variant='<-------------1fr------------->'>
            <Button
              spotlight
              highlight
              disabled={!githubAuthenticated || storedTargetGithubRepo == null || githubWorking}
              onMouseUp={triggerUpdateAgainstGithub}
            >
              {isGithubUpdating(githubOperations) ? <GithubSpinner /> : 'Update Against Github'}
            </Button>
          </UIGridRow>
          {loadBranchesUI}
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
