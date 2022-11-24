/** @jsxRuntime classic */
/** @jsx jsx */
/** @jsxFrag React.Fragment */
import { jsx } from '@emotion/react'
import React, { useEffect } from 'react'
import TimeAgo from 'react-timeago'
import { projectDependenciesSelector } from '../../../../core/shared/dependencies'
import {
  dispatchPromiseActions,
  getBranchesForGithubRepository,
  getGithubFileChangesCount,
  githubFileChangesToList,
  updateProjectAgainstGithub,
  updateProjectWithBranchContent,
  useGithubFileChanges,
} from '../../../../core/shared/github'
import { startGithubAuthentication } from '../../../../utils/github-auth'
import { unless, when } from '../../../../utils/react-conditionals'
import {
  Button,
  colorTheme,
  FlexColumn,
  FlexRow,
  Icons,
  MenuIcons,
  Section,
  SectionTitleRow,
  StringInput,
  Title,
} from '../../../../uuiui'
import * as EditorActions from '../../../editor/actions/action-creators'
import {
  githubRepoFullName,
  isGithubCommishing,
  isGithubListingBranches,
  isGithubLoadingAnyBranch,
  isGithubLoadingBranch,
  isGithubUpdating,
} from '../../../editor/store/editor-state'
import { useEditorState } from '../../../editor/store/store-hook'
import { UIGridRow } from '../../../inspector/widgets/ui-grid-row'
import { Block } from './block'
import { Ellipsis, GithubFileChangesList } from './github-file-changes-list'
import { GithubSpinner } from './github-spinner'
import { cleanupBranchName } from './helpers'
import { PullRequestPane } from './pull-request-pane'
import { RefreshIcon } from './refresh-icon'
import { RepositoryListing } from './repository-listing'

const compactTimeagoFormatter = (value: number, unit: string) => {
  return `${value}${unit.charAt(0)}`
}

type IndicatorState = 'incomplete' | 'failed' | 'successful' | 'pending'

const AccountBlock = () => {
  const authenticated = useEditorState(
    (store) => store.userState.githubState.authenticated,
    'Github authenticated',
  )
  const state = React.useMemo(() => (authenticated ? 'successful' : 'incomplete'), [authenticated])
  const dispatch = useEditorState((store) => store.dispatch, 'dispatch')
  const triggerAuthentication = React.useCallback(() => {
    void startGithubAuthentication(dispatch)
  }, [dispatch])

  if (authenticated) {
    return null
  }

  return (
    <Block title='Account' status={state} first={true} last={true} expanded={true}>
      <Button
        spotlight
        highlight
        style={{
          padding: '1em',
          background: colorTheme.primary.value,
          borderRadius: 3,
          color: colorTheme.fg9.value,
        }}
        css={{
          '&:hover': {
            opacity: 0.7,
          },
        }}
        onMouseUp={triggerAuthentication}
      >
        Authenticate With Github
      </Button>
    </Block>
  )
}

const RepositoryBlock = () => {
  const repo = useEditorState(
    (store) => store.editor.githubSettings.targetRepository,
    'RepositoryBlock repo',
  )
  const githubAuthenticated = useEditorState(
    (store) => store.userState.githubState.authenticated,
    'RepositoryBlock authenticated',
  )
  const repoName = React.useMemo(() => githubRepoFullName(repo) || undefined, [repo])
  const hasRepo = React.useMemo(() => repo != null, [repo])
  const [expanded, setExpanded] = React.useState(false)
  React.useEffect(() => {
    setExpanded(repo == null)
  }, [repo])

  const toggleExpanded = React.useCallback(() => {
    if (!hasRepo) {
      return
    }
    setExpanded(!expanded)
  }, [expanded, hasRepo])

  if (!githubAuthenticated) {
    return null
  }

  return (
    <Block
      title={hasRepo ? 'Repository' : 'Select Repository'}
      subtitle={repoName}
      status={hasRepo ? 'successful' : 'pending'}
      first={true}
      last={!hasRepo}
      expanded={expanded}
      onClick={toggleExpanded}
    >
      <FlexColumn style={{ gap: 4 }}>
        <UIGridRow padded={false} variant='<-------------1fr------------->'>
          <div>
            We only support <strong>public</strong> repositories at this time.
          </div>
        </UIGridRow>
        <RepositoryListing
          githubAuthenticated={githubAuthenticated}
          storedTargetGithubRepo={repo}
        />
      </FlexColumn>
    </Block>
  )
}

const BranchBlock = () => {
  const { currentBranch, dispatch, githubOperations, targetRepository, branchesForRepository } =
    useEditorState(
      (store) => ({
        currentBranch: store.editor.githubSettings.branchName,
        dispatch: store.dispatch,
        githubOperations: store.editor.githubOperations,
        targetRepository: store.editor.githubSettings.targetRepository,
        branchesForRepository: store.editor.githubData.branches,
      }),
      'Github branch',
    )
  const repositoryData = useEditorState(
    (store) =>
      store.editor.githubData.publicRepositories.find(
        (r) => r.fullName === githubRepoFullName(store.editor.githubSettings.targetRepository),
      ) ?? null,
    'BranchBlock Repository data',
  )

  const isListingBranches = React.useMemo(
    () => isGithubListingBranches(githubOperations),
    [githubOperations],
  )

  const refreshBranches = React.useCallback(() => {
    if (targetRepository != null) {
      void dispatchPromiseActions(
        dispatch,
        getBranchesForGithubRepository(dispatch, targetRepository),
      )
    }
  }, [dispatch, targetRepository])

  React.useEffect(() => {
    refreshBranches()
  }, [refreshBranches])

  const [expandedFlag, setExpandedFlag] = React.useState(false)

  const expanded = React.useMemo(() => {
    return expandedFlag && branchesForRepository != null
  }, [expandedFlag, branchesForRepository])

  React.useEffect(() => {
    setExpandedFlag(currentBranch == null)
  }, [currentBranch])

  const toggleExpanded = React.useCallback(() => {
    if (currentBranch == null) {
      return
    }
    setExpandedFlag(!expanded)
  }, [expanded, currentBranch])

  const [branchFilter, setBranchFilter] = React.useState('')

  const updateBranchFilter = React.useCallback(
    (event: React.ChangeEvent<HTMLInputElement>) => {
      setBranchFilter(event.currentTarget.value)
    },
    [setBranchFilter],
  )

  const [branchesWereLoaded, setBranchesWereLoaded] = React.useState(false)
  React.useEffect(() => {
    setBranchesWereLoaded(false)
  }, [currentBranch, targetRepository])

  const filteredBranches = React.useMemo(() => {
    if (isListingBranches && !branchesWereLoaded) {
      return []
    }
    if (branchesForRepository == null || repositoryData == null) {
      return []
    }

    setBranchesWereLoaded(true)

    let filtered = branchesForRepository.filter(
      (b) => branchFilter.length === 0 || b.name.includes(branchFilter),
    )

    if (branchesForRepository.length === 0) {
      filtered.push({
        name: repositoryData.defaultBranch,
        new: true,
      })
    } else {
      const newBranchName = cleanupBranchName(branchFilter)
      if (newBranchName.length > 1 && !filtered.some((b) => b.name === newBranchName)) {
        filtered.push({
          name: newBranchName,
          new: true,
        })
      }
    }

    return filtered
  }, [branchesForRepository, branchesWereLoaded, repositoryData, branchFilter, isListingBranches])

  const clearBranch = React.useCallback(() => {
    dispatch(
      [EditorActions.updateGithubSettings({ branchName: null, branchLoaded: false })],
      'everyone',
    )
  }, [dispatch])

  const listBranchesUI = React.useMemo(() => {
    return (
      <UIGridRow padded={false} variant='<-------------1fr------------->' style={{ width: '100%' }}>
        <UIGridRow padded={false} variant='<-------------1fr------------->'>
          <StringInput
            testId='branches-input'
            placeholder='Filter…'
            value={branchFilter}
            onChange={updateBranchFilter}
          />
        </UIGridRow>
        <UIGridRow
          padded={false}
          variant='<-------------1fr------------->'
          style={{
            minHeight: 0,
            marginBottom: 6,
            color: colorTheme.fg4.value,
            fontSize: 10,
          }}
        >
          Or type in a new branch name.
        </UIGridRow>
        <FlexColumn
          style={{
            height: 220,
            overflowY: 'scroll',
            border: `1px solid ${colorTheme.githubBoxesBorder.value}`,
            borderRadius: 2,
          }}
        >
          {filteredBranches.map((branch, index) => {
            function selectBranch() {
              if (isListingBranches) {
                return
              }
              dispatch(
                [
                  EditorActions.updateGithubSettings({
                    branchName: branch.name,
                    branchLoaded: false,
                  }),
                ],
                'everyone',
              )
            }
            const loadingThisBranch = isGithubLoadingBranch(
              githubOperations,
              branch.name,
              targetRepository,
            )
            const isCurrent = currentBranch === branch.name
            return (
              <UIGridRow
                key={index}
                padded
                variant='<----------1fr---------><-auto->'
                css={{
                  cursor: loadingThisBranch
                    ? 'wait'
                    : isListingBranches
                    ? 'not-allowed'
                    : 'pointer',
                  opacity: isListingBranches && !loadingThisBranch ? 0.5 : 1,
                  '&:hover': {
                    background: colorTheme.primarySubdued.value,
                    color: colorTheme.white.value,
                    svg: { stroke: colorTheme.white.value },
                  },
                  fontWeight: isCurrent ? 'bold' : 'normal',
                  color: branch.new === true ? colorTheme.primary.value : 'inherit',
                }}
                onClick={selectBranch}
              >
                <Ellipsis>
                  {when(isCurrent, <span>&rarr; </span>)}
                  {branch.name}
                  {when(
                    repositoryData?.defaultBranch === branch.name,
                    <span style={{ color: colorTheme.fg7.value }}> (default)</span>,
                  )}
                </Ellipsis>
                {when(branch.new === true, <span>Create new</span>)}
                {when(loadingThisBranch, <GithubSpinner />)}
              </UIGridRow>
            )
          })}
        </FlexColumn>
        <UIGridRow padded={false} variant='<-------------1fr------------->'>
          <Button
            spotlight
            highlight
            style={{ padding: '0 6px', marginTop: 6 }}
            onMouseUp={refreshBranches}
            disabled={isListingBranches}
          >
            {isListingBranches ? (
              <GithubSpinner />
            ) : (
              <FlexRow style={{ gap: 4 }}>
                <RefreshIcon /> Refresh list
              </FlexRow>
            )}
          </Button>
        </UIGridRow>
        {when(
          currentBranch != null,
          <UIGridRow padded={false} variant='<-------------1fr------------->'>
            <Button
              spotlight
              highlight
              style={{ color: colorTheme.errorForeground.value }}
              onClick={clearBranch}
            >
              Clear branch
            </Button>
          </UIGridRow>,
        )}
      </UIGridRow>
    )
  }, [
    targetRepository,
    dispatch,
    githubOperations,
    currentBranch,
    isListingBranches,
    refreshBranches,
    branchFilter,
    updateBranchFilter,
    filteredBranches,
    clearBranch,
    repositoryData,
  ])

  const githubAuthenticated = useEditorState(
    (store) => store.userState.githubState.authenticated,
    'Github authenticated',
  )

  if (!githubAuthenticated) {
    return null
  }

  if (targetRepository == null) {
    return null
  }

  return (
    <Block
      expanded={expanded}
      onClick={toggleExpanded}
      title={currentBranch != null ? 'Branch' : 'Select Branch'}
      subtitle={currentBranch || undefined}
      status={!expanded && currentBranch != null ? 'successful' : 'incomplete'}
      last={currentBranch == null}
    >
      {listBranchesUI}
    </Block>
  )
}

const RemoteChangesBlock = () => {
  const upstreamChanges = useEditorState(
    (store) => store.editor.githubData.upstreamChanges,
    'Upstream changes',
  )
  const hasUpstreamChanges = React.useMemo(
    () => getGithubFileChangesCount(upstreamChanges) > 0,
    [upstreamChanges],
  )
  const githubFileChanges = useGithubFileChanges()
  const bothModified = React.useMemo(() => {
    const upstreamList = githubFileChangesToList(upstreamChanges)
    const localList = githubFileChangesToList(githubFileChanges)
    const intersection = upstreamList
      .filter((upstream) => localList.some((local) => local.filename === upstream.filename))
      .map((change) => change.filename)
    return intersection
  }, [upstreamChanges, githubFileChanges])

  const state = React.useMemo(
    (): IndicatorState =>
      hasUpstreamChanges ? (bothModified.length > 0 ? 'failed' : 'pending') : 'successful',
    [hasUpstreamChanges, bothModified],
  )
  const githubOperations = useEditorState(
    (store) => store.editor.githubOperations,
    'Github operations',
  )
  const githubLastUpdatedAt = useEditorState(
    (store) => store.editor.githubData.lastUpdatedAt,
    'Github last updated',
  )
  const repo = useEditorState(
    (store) => store.editor.githubSettings.targetRepository,
    'Github repo',
  )
  const dispatch = useEditorState((store) => store.dispatch, 'dispatch')
  const branch = useEditorState((store) => store.editor.githubSettings.branchName, 'Github branch')
  const branchLoaded = useEditorState(
    (store) => store.editor.githubSettings.branchLoaded,
    'Github branchLoaded',
  )
  const commit = useEditorState(
    (store) => store.editor.githubSettings.originCommit,
    'Github commit',
  )
  const triggerUpdateAgainstGithub = React.useCallback(() => {
    if (repo != null && branch != null && commit != null) {
      void updateProjectAgainstGithub(dispatch, repo, branch, commit)
    }
  }, [dispatch, repo, branch, commit])
  const githubAuthenticated = useEditorState(
    (store) => store.userState.githubState.authenticated,
    'Github authenticated',
  )
  if (!githubAuthenticated || branch == null || !branchLoaded) {
    return null
  }
  return (
    <Block
      expanded={hasUpstreamChanges}
      title={hasUpstreamChanges ? 'Remote Changes' : 'No Remote Changes'}
      subtitle={
        <TimeAgo
          style={{ color: colorTheme.fg7.value }}
          date={githubLastUpdatedAt ?? 0}
          formatter={compactTimeagoFormatter}
        />
      }
      status={state}
    >
      {when(
        hasUpstreamChanges,
        <FlexColumn style={{ gap: 10, width: '100%' }}>
          <GithubFileChangesList
            conflicts={bothModified}
            revertable={false}
            clickable={false}
            changes={upstreamChanges}
            showHeader={true}
          />
          <Button
            disabled={isGithubUpdating(githubOperations)}
            spotlight
            highlight
            style={{
              gap: 4,
              background: colorTheme.secondaryOrange.value,
              borderRadius: 3,
              color: colorTheme.bg0.value,
            }}
            onMouseUp={triggerUpdateAgainstGithub}
          >
            {isGithubUpdating(githubOperations) ? (
              <GithubSpinner />
            ) : (
              <>
                {<Icons.Download style={{ width: 19, height: 19 }} color={'on-light-main'} />}
                Pull Remote Changes
              </>
            )}
          </Button>
        </FlexColumn>,
      )}
    </Block>
  )
}

const LocalChangesBlock = () => {
  const githubFileChanges = useGithubFileChanges()
  const changesCount = React.useMemo(
    () => getGithubFileChangesCount(githubFileChanges),
    [githubFileChanges],
  )
  const hasLocalChanges = React.useMemo(() => changesCount > 0, [changesCount])
  const state = React.useMemo(
    (): IndicatorState => (hasLocalChanges ? 'incomplete' : 'successful'),
    [hasLocalChanges],
  )
  const githubOperations = useEditorState(
    (store) => store.editor.githubOperations,
    'Github operations',
  )
  const dispatch = useEditorState((store) => store.dispatch, 'dispatch')
  const repo = useEditorState(
    (store) => store.editor.githubSettings.targetRepository,
    'Github repo',
  )

  const branch = useEditorState((store) => store.editor.githubSettings.branchName, 'Github branch')

  const [pushToNewBranch, setPushToNewBranch] = React.useState(false)
  React.useEffect(() => {
    setPushToNewBranch(false)
  }, [branch])

  const togglePushToNewBranch = React.useCallback(() => {
    setPushToNewBranch(!pushToNewBranch)
  }, [pushToNewBranch])

  React.useEffect(() => {
    setRawCommitBranchName(pushToNewBranch ? null : branch)
  }, [pushToNewBranch, branch])

  const [rawCommitBranchName, setRawCommitBranchName] = React.useState<string | null>(null)
  const updateCommitBranchName = React.useCallback(
    (e: React.ChangeEvent<HTMLInputElement>) => setRawCommitBranchName(e.target.value),
    [],
  )
  const cleanedCommitBranchName = React.useMemo(() => {
    if (rawCommitBranchName == null) {
      return null
    }
    return cleanupBranchName(rawCommitBranchName)
  }, [rawCommitBranchName])

  const [commitMessage, setCommitMessage] = React.useState<string | null>(null)
  React.useEffect(() => {
    setCommitMessage(null)
  }, [branch])
  const updateCommitMessage = React.useCallback(
    (e: React.ChangeEvent<HTMLInputElement>) => setCommitMessage(e.target.value),
    [],
  )

  const triggerSaveToGithub = React.useCallback(() => {
    if (repo != null && cleanedCommitBranchName != null && commitMessage != null) {
      dispatch(
        [EditorActions.saveToGithub(repo, cleanedCommitBranchName, commitMessage)],
        'everyone',
      )
    }
  }, [dispatch, repo, commitMessage, cleanedCommitBranchName])

  const githubAuthenticated = useEditorState(
    (store) => store.userState.githubState.authenticated,
    'Github authenticated',
  )
  const pullRequests = useEditorState(
    (store) => store.editor.githubData.currentBranchPullRequests,
    'Branch PRs',
  )

  const branchLoaded = useEditorState(
    (store) => store.editor.githubSettings.branchLoaded,
    'Github branchLoaded',
  )

  if (!githubAuthenticated || branch == null || !branchLoaded) {
    return null
  }

  return (
    <Block
      expanded={hasLocalChanges}
      title={hasLocalChanges ? 'Local Changes' : 'No Local Changes'}
      status={state}
      last={pullRequests == null || pullRequests.length === 0}
    >
      {when(
        hasLocalChanges,
        <FlexColumn style={{ gap: 10, width: '100%', whiteSpace: 'pre-wrap' }}>
          <GithubFileChangesList
            showHeader={true}
            revertable={true}
            clickable={true}
            changes={githubFileChanges}
          />
          <div>Any unsaved files will be saved.</div>
          <StringInput
            testId='commit-message-input'
            placeholder='Commit message'
            value={commitMessage ?? ''}
            onChange={updateCommitMessage}
          />
          {when(
            pushToNewBranch,
            <div>
              <StringInput
                testId='commit-branch-input'
                placeholder='New branch name'
                value={rawCommitBranchName || ''}
                onChange={updateCommitBranchName}
              />
              {when(
                rawCommitBranchName !== cleanedCommitBranchName,
                <div
                  style={{
                    fontSize: 10,
                    background: colorTheme.bg1.value,
                    padding: '2px 6px',
                    color: colorTheme.fg4.value,
                  }}
                >
                  {cleanedCommitBranchName}
                </div>,
              )}
            </div>,
          )}
          <Button
            disabled={isGithubCommishing(githubOperations)}
            spotlight
            highlight
            style={{
              gap: 4,
              background: colorTheme.secondaryBlue.value,
              borderRadius: 3,
              color: colorTheme.bg0.value,
            }}
            onMouseUp={triggerSaveToGithub}
          >
            {isGithubCommishing(githubOperations) ? (
              <GithubSpinner />
            ) : (
              <>
                {<Icons.Upload style={{ width: 19, height: 19 }} color={'on-light-main'} />}
                Commit and Push
              </>
            )}
          </Button>
          <div style={{ textAlign: 'center' }}>or</div>
          <Button spotlight highlight onClick={togglePushToNewBranch}>
            {when(pushToNewBranch, <span>Push To Current Branch</span>)}
            {unless(pushToNewBranch, <span>Push To New Branch</span>)}
          </Button>
        </FlexColumn>,
      )}
    </Block>
  )
}

const PullRequestButton = () => {
  const branch = useEditorState(
    (store) => store.editor.githubSettings.branchName,
    'PullRequestButton branch',
  )
  const repo = useEditorState(
    (store) =>
      store.editor.githubData.publicRepositories.find(
        (r) => r.fullName === githubRepoFullName(store.editor.githubSettings.targetRepository),
      ) ?? null,
    'PullRequestButton repository',
  )

  const githubFileChanges = useGithubFileChanges()
  const changesCount = React.useMemo(
    () => getGithubFileChangesCount(githubFileChanges),
    [githubFileChanges],
  )
  const hasLocalChanges = React.useMemo(() => changesCount > 0, [changesCount])

  const openPR = React.useCallback(() => {
    if (repo != null && branch != null) {
      window.open(`https://github.com/${repo.fullName}/compare/${branch}?expand=1`, '_blank')
    }
  }, [repo, branch])

  const branchLoaded = useEditorState(
    (store) => store.editor.githubSettings.branchLoaded,
    'Github branch loaded',
  )

  if (repo == null || branch == null || !branchLoaded) {
    return null
  }
  if (hasLocalChanges || repo.defaultBranch === branch) {
    return null
  }
  return (
    <UIGridRow padded variant='<-------------1fr------------->'>
      <Button spotlight highlight onClick={openPR}>
        Open a Pull Request
      </Button>
    </UIGridRow>
  )
}

const BranchNotLoadedBlock = () => {
  const { branchName, branches, branchLoaded, dispatch, githubOperations, githubRepo } =
    useEditorState(
      (store) => ({
        branchName: store.editor.githubSettings.branchName,
        branchLoaded: store.editor.githubSettings.branchLoaded,
        dispatch: store.dispatch,
        branches: store.editor.githubData.branches,
        githubOperations: store.editor.githubOperations,
        githubRepo: store.editor.githubSettings.targetRepository,
      }),
      'BranchNotLoadedBlock data',
    )

  const builtInDependencies = useEditorState(
    (store) => store.builtInDependencies,
    'Built-in dependencies',
  )

  const currentDependencies = useEditorState(projectDependenciesSelector, 'Project dependencies')

  const loadFromBranch = React.useCallback(() => {
    if (githubRepo != null && branchName != null) {
      void updateProjectWithBranchContent(
        dispatch,
        githubRepo,
        branchName,
        false,
        currentDependencies,
        builtInDependencies,
      )
    }
  }, [dispatch, githubRepo, branchName, currentDependencies, builtInDependencies])

  const isANewBranch = React.useMemo(() => {
    if (branches == null) {
      return true
    }
    return !branches.some((b) => b.name === branchName)
  }, [branches, branchName])

  const [commitMessage, setCommitMessage] = React.useState<string | null>(null)
  React.useEffect(() => {
    setCommitMessage(null)
  }, [branchName])
  const updateCommitMessage = React.useCallback(
    (e: React.ChangeEvent<HTMLInputElement>) => setCommitMessage(e.target.value),
    [],
  )

  const pushToBranch = React.useCallback(() => {
    if (githubRepo == null || branchName == null) {
      return
    }
    dispatch(
      [
        EditorActions.saveToGithub(
          githubRepo,
          branchName,
          commitMessage ?? 'Committed automatically',
        ),
      ],
      'everyone',
    )
  }, [dispatch, githubRepo, branchName, commitMessage])

  type LoadFlow = 'loadFromBranch' | 'pushToBranch' | 'createBranch'

  const [flow, setFlow] = React.useState<LoadFlow | null>(null)
  const updateFlow = React.useCallback(
    (f: LoadFlow | null) => () => {
      setFlow(f)
    },
    [],
  )
  useEffect(() => {
    setFlow(isANewBranch ? 'createBranch' : null)
  }, [branchName, isANewBranch])

  if (!branchName || branchLoaded) {
    return null
  }
  return (
    <Block title='Contents' status='pending' expanded={true} last={true}>
      <FlexColumn style={{ gap: 8, width: '100%' }}>
        {flow == null ? (
          <>
            <Button
              spotlight
              highlight
              onClick={updateFlow('loadFromBranch')}
              style={{
                gap: 4,
                borderRadius: 3,
              }}
            >
              {<Icons.Download style={{ width: 19, height: 19 }} />}
              Load from Branch
            </Button>
            <div style={{ textAlign: 'center' }}>or</div>
            <FlexColumn style={{ gap: 2 }}>
              <Button
                spotlight
                highlight
                style={{ gap: 4, padding: '0 6px' }}
                onClick={updateFlow('pushToBranch')}
              >
                {<Icons.Upload style={{ width: 19, height: 19 }} />}
                Push to {isANewBranch ? 'New ' : ''}Branch
              </Button>
            </FlexColumn>
          </>
        ) : (
          <>
            {when(
              flow === 'loadFromBranch',
              <UIGridRow padded={false} variant='<-------------1fr------------->'>
                Loading from branch will replace your current project contents with the ones on
                Github.
                <Button
                  disabled={isGithubLoadingAnyBranch(githubOperations)}
                  spotlight
                  highlight
                  style={{
                    marginTop: 6,
                    gap: 4,
                    padding: '0 6px',
                    color: colorTheme.errorForeground.value,
                  }}
                  onClick={loadFromBranch}
                >
                  Yes, Load from this Branch.
                </Button>
              </UIGridRow>,
            )}
            {when(
              flow === 'pushToBranch',
              <UIGridRow padded={false} variant='<-------------1fr------------->'>
                Pushing to the branch will store your project on Github, replacing the current
                branch contents.
                <StringInput
                  testId='commit-message-input'
                  placeholder='Commit message'
                  value={commitMessage ?? ''}
                  onChange={updateCommitMessage}
                />
                <Button
                  disabled={isGithubCommishing(githubOperations)}
                  spotlight
                  highlight
                  style={{ marginTop: 6, gap: 4, padding: '0 6px' }}
                  onClick={pushToBranch}
                >
                  {<Icons.Upload style={{ width: 19, height: 19 }} color={'on-light-main'} />}
                  Push to Branch
                </Button>
              </UIGridRow>,
            )}
            {when(
              flow === 'createBranch',
              <UIGridRow padded={false} variant='<-------------1fr------------->'>
                <StringInput
                  testId='commit-message-input'
                  placeholder='Commit message'
                  value={commitMessage ?? ''}
                  onChange={updateCommitMessage}
                />
                <Button
                  disabled={isGithubCommishing(githubOperations)}
                  spotlight
                  highlight
                  style={{ marginTop: 6, gap: 4, padding: '0 6px' }}
                  onClick={pushToBranch}
                >
                  {<Icons.Upload style={{ width: 19, height: 19 }} />}
                  Create Branch and Push
                </Button>
              </UIGridRow>,
            )}
            {when(
              flow !== 'createBranch',
              <Button spotlight highlight onClick={updateFlow(null)} style={{ marginTop: 6 }}>
                Cancel
              </Button>,
            )}
          </>
        )}
      </FlexColumn>
    </Block>
  )
}

const PullRequestBlock = () => {
  const pullRequests = useEditorState(
    (store) => store.editor.githubData.currentBranchPullRequests,
    'Branch PRs',
  )
  if (pullRequests == null || pullRequests.length === 0) {
    return null
  }
  return (
    <Block
      title='Pull Requests'
      subtitle={`${pullRequests.length} open`}
      status={'pending'}
      expanded={true}
      last={true}
    >
      <PullRequestPane />
    </Block>
  )
}

export const GithubPane = React.memo(() => {
  const githubUser = useEditorState(
    (store) => store.editor.githubData.githubUserDetails,
    'Github user details',
  )
  const openGithubProfile = React.useCallback(() => {
    if (githubUser != null) {
      window.open(githubUser.htmlURL, '_blank')
    }
  }, [githubUser])
  return (
    <>
      <Section>
        <SectionTitleRow minimised={false} hideButton>
          <FlexRow flexGrow={1}>
            <Title style={{ flexGrow: 1 }}>Github</Title>
          </FlexRow>
          {when(
            githubUser != null,
            <Button
              style={{ gap: 4, padding: '0 6px' }}
              onClick={openGithubProfile}
              css={{
                '&:hover': {
                  opacity: 0.6,
                },
              }}
            >
              @{githubUser?.login}
              {<MenuIcons.Octocat style={{ width: 19, height: 19 }} />}
            </Button>,
          )}
        </SectionTitleRow>
      </Section>
      <Section style={{ padding: '10px' }}>
        <AccountBlock />
        <RepositoryBlock />
        <BranchBlock />
        <BranchNotLoadedBlock />
        <RemoteChangesBlock />
        <LocalChangesBlock />
        <PullRequestBlock />
        <PullRequestButton />
      </Section>
    </>
  )
})
