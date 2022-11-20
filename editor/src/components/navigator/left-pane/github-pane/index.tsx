/** @jsxRuntime classic */
/** @jsx jsx */
/** @jsxFrag React.Fragment */
import { jsx } from '@emotion/react'
import React from 'react'
import TimeAgo from 'react-timeago'
import { projectDependenciesSelector } from '../../../../core/shared/dependencies'
import {
  getBranchesForGithubRepository,
  getGithubFileChangesCount,
  githubFileChangesSelector,
  githubFileChangesToList,
  updateProjectAgainstGithub,
  updateProjectWithBranchContent,
} from '../../../../core/shared/github'
import { startGithubAuthentication } from '../../../../utils/github-auth'
import { unless, when } from '../../../../utils/react-conditionals'
import {
  Button,
  colorTheme,
  FlexColumn,
  FlexRow,
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
      <Button spotlight highlight style={{ padding: '0 6px' }} onMouseUp={triggerAuthentication}>
        Authenticate with Github
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
  const toggleExpanded = React.useCallback(() => setExpanded(!expanded), [expanded])
  React.useEffect(() => {
    setExpanded(repo == null)
  }, [repo])

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
  const currentBranch = useEditorState(
    (store) => store.editor.githubSettings.branchName,
    'Github branch',
  )
  const dispatch = useEditorState((store) => store.dispatch, 'GithubPane dispatch')
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
  const branchesForRepository = useEditorState(
    (store) => store.editor.githubData.branches,
    'BranchBlock branchesForRepository',
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

  const [expandedFlag, setExpandedFlag] = React.useState(false)
  const expanded = React.useMemo(() => {
    return expandedFlag && branchesForRepository != null
  }, [expandedFlag, branchesForRepository])
  const toggleExpanded = React.useCallback(() => setExpandedFlag(!expanded), [expanded])
  React.useEffect(() => {
    setExpandedFlag(currentBranch == null)
  }, [currentBranch])

  const [branchFilter, setBranchFilter] = React.useState('')
  const updateBranchFilter = React.useCallback(
    (event: React.ChangeEvent<HTMLInputElement>) => {
      setBranchFilter(event.currentTarget.value)
    },
    [setBranchFilter],
  )

  const repo = useEditorState(
    (store) =>
      store.editor.githubData.publicRepositories.find(
        (r) => r.fullName === githubRepoFullName(store.editor.githubSettings.targetRepository),
      ) || null,
    'GH repo',
  )

  const filteredBranches = React.useMemo(() => {
    if (branchesForRepository == null || repo == null) {
      return []
    }

    let filtered = branchesForRepository.filter(
      (b) => branchFilter.length === 0 || b.name.includes(branchFilter),
    )

    if (branchesForRepository.length === 0) {
      filtered.push({
        name: repo.defaultBranch,
        new: true,
      })
    } else {
      const newBranchName = cleanupBranchName(branchFilter)
      if (newBranchName.length > 1) {
        filtered.push({
          name: newBranchName,
          new: true,
        })
      }
    }

    return filtered
  }, [branchesForRepository, repo, branchFilter])

  const builtInDependencies = useEditorState(
    (store) => store.builtInDependencies,
    'Built-in dependencies',
  )

  const currentDependencies = useEditorState(projectDependenciesSelector, 'Project dependencies')

  const loadBranchesUI = React.useMemo(() => {
    return (
      <UIGridRow padded={false} variant='<-------------1fr------------->' style={{ width: '100%' }}>
        <UIGridRow padded={false} variant='<----------1fr---------><-auto->'>
          <StringInput
            testId='branches-input'
            placeholder='Filter…'
            value={branchFilter}
            onChange={updateBranchFilter}
          />
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
        <FlexColumn
          style={{
            height: 220,
            overflowY: 'scroll',
            border: `1px solid ${colorTheme.githubBoxesBorder.value}`,
            borderRadius: 2,
          }}
        >
          {filteredBranches.map((branch, index) => {
            function loadContentForBranch() {
              if (githubWorking) {
                return
              }
              if (storedTargetGithubRepo != null) {
                if (branch.new) {
                  dispatch(
                    [
                      EditorActions.saveToGithub(
                        storedTargetGithubRepo,
                        branch.name,
                        'First commit',
                      ),
                    ],
                    'everyone',
                  )
                } else {
                  void updateProjectWithBranchContent(
                    dispatch,
                    storedTargetGithubRepo,
                    branch.name,
                    false,
                    currentDependencies,
                    builtInDependencies,
                  ).then(() => setExpandedFlag(false))
                }
              }
            }
            const loadingThisBranch = isGithubLoadingBranch(
              githubOperations,
              branch.name,
              storedTargetGithubRepo,
            )
            const isCurrent = currentBranch === branch.name
            return (
              <UIGridRow
                key={index}
                padded
                variant='<----------1fr---------><-auto->'
                css={{
                  cursor: loadingThisBranch ? 'wait' : githubWorking ? 'not-allowed' : 'pointer',
                  opacity: githubWorking && !loadingThisBranch ? 0.5 : 1,
                  '&:hover': {
                    background: colorTheme.primarySubdued.value,
                    color: colorTheme.white.value,
                    svg: { stroke: colorTheme.white.value },
                  },
                  fontWeight: isCurrent ? 'bold' : 'normal',
                  color: branch.new === true ? colorTheme.primary.value : 'inherit',
                }}
                onClick={loadContentForBranch}
              >
                <Ellipsis>
                  {when(isCurrent, <span>&rarr; </span>)}
                  {branch.name}
                  {when(
                    repo?.defaultBranch === branch.name,
                    <span style={{ color: colorTheme.fg7.value }}> (default)</span>,
                  )}
                </Ellipsis>
                {when(branch.new === true, <span>Create new</span>)}
                {when(loadingThisBranch, <GithubSpinner />)}
              </UIGridRow>
            )
          })}
        </FlexColumn>
      </UIGridRow>
    )
  }, [
    storedTargetGithubRepo,
    dispatch,
    githubWorking,
    githubOperations,
    currentBranch,
    isLoadingBranches,
    refreshBranches,
    branchFilter,
    updateBranchFilter,
    setExpandedFlag,
    filteredBranches,
    builtInDependencies,
    currentDependencies,
    repo,
  ])

  const githubAuthenticated = useEditorState(
    (store) => store.userState.githubState.authenticated,
    'Github authenticated',
  )

  if (!githubAuthenticated) {
    return null
  }

  if (storedTargetGithubRepo == null) {
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
      {loadBranchesUI}
    </Block>
  )
}

const PullIcon = () => {
  return (
    <svg width='16' height='13' viewBox='0 0 16 13' fill='none' xmlns='http://www.w3.org/2000/svg'>
      <path
        d='M6.91409 1.05206C5.22083 1.05206 3.84816 2.38249 3.84816 4.02365C3.84816 4.16836 3.85879 4.31037 3.87927 4.44905L3.96426 5.02454L3.36455 5.01442L3.33717 5.01419C2.49054 5.01419 1.8042 5.6794 1.8042 6.49998C1.8042 7.32057 2.49054 7.98578 3.33717 7.98578H5.89212V8.97632H3.33717C1.92611 8.97632 0.782227 7.86763 0.782227 6.49998C0.782227 5.30185 1.66014 4.30245 2.8265 4.07313C2.82629 4.05666 2.82618 4.04017 2.82618 4.02365C2.82618 1.83543 4.6564 0.0615234 6.91409 0.0615234C8.48363 0.0615234 9.84558 0.918668 10.5306 2.17511C10.8431 2.08876 11.1728 2.04259 11.513 2.04259C13.4885 2.04259 15.0899 3.59476 15.0899 5.50945C15.0899 7.42415 13.4885 8.97632 11.513 8.97632H9.98003V7.98578H11.513C12.9241 7.98578 14.0679 6.87709 14.0679 5.50945C14.0679 4.14181 12.9241 3.03312 11.513 3.03312C11.1402 3.03312 10.7874 3.11019 10.4694 3.24831L9.97454 3.46326L9.78253 2.97172C9.34396 1.84897 8.22441 1.05206 6.91409 1.05206Z'
        fill={colorTheme.fg9.value}
      />
      <path
        d='M8.44706 6.00472C8.44706 5.73119 8.21828 5.50945 7.93607 5.50945C7.65386 5.50945 7.42508 5.73119 7.42508 6.00472V11.2475L6.25344 10.1119C6.05389 9.9185 5.73035 9.9185 5.53079 10.1119C5.33124 10.3053 5.33124 10.6189 5.53079 10.8123L7.57475 12.7934C7.67058 12.8863 7.80055 12.9384 7.93607 12.9384C8.0716 12.9384 8.20157 12.8863 8.2974 12.7934L10.3414 10.8123C10.5409 10.6189 10.5409 10.3053 10.3414 10.1119C10.1418 9.9185 9.81826 9.9185 9.6187 10.1119L8.44706 11.2475V6.00472Z'
        fill={colorTheme.fg9.value}
      />
    </svg>
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
  const githubFileChanges = useEditorState(githubFileChangesSelector, 'Github file changes')
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
  const githubWorking = React.useMemo(() => {
    return githubOperations.length > 0
  }, [githubOperations])
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
  if (!githubAuthenticated || branch == null) {
    return null
  }
  return (
    <Block
      expanded={hasUpstreamChanges}
      title={hasUpstreamChanges ? 'Remote Changes' : 'No Remote Changes'}
      subtitle={
        <TimeAgo
          style={{ color: colorTheme.fg7.value }}
          date={githubLastUpdatedAt || 0}
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
            githubWorking={githubWorking}
          />
          <Button
            disabled={githubWorking}
            spotlight
            highlight
            style={{
              gap: 4,
              background: colorTheme.secondaryOrange.value,
              borderRadius: 3,
              color: colorTheme.fg9.value,
            }}
            onMouseUp={triggerUpdateAgainstGithub}
          >
            {isGithubUpdating(githubOperations) ? (
              <GithubSpinner />
            ) : (
              <>
                <PullIcon />
                Pull Remote Changes
              </>
            )}
          </Button>
        </FlexColumn>,
      )}
    </Block>
  )
}

const PushIcon = () => {
  return (
    <svg width='15' height='11' viewBox='0 0 15 11' fill='none' xmlns='http://www.w3.org/2000/svg'>
      <path
        d='M6.41409 1.05206C4.72083 1.05206 3.34816 2.38249 3.34816 4.02365C3.34816 4.16836 3.35879 4.31037 3.37927 4.44905L3.46426 5.02454L2.86455 5.01442L2.83717 5.01419C1.99054 5.01419 1.3042 5.6794 1.3042 6.49998C1.3042 7.32057 1.99054 7.98578 2.83717 7.98578H5.39212V8.97632H2.83717C1.42611 8.97632 0.282227 7.86763 0.282227 6.49998C0.282227 5.30185 1.16014 4.30245 2.3265 4.07313C2.32629 4.05666 2.32618 4.04017 2.32618 4.02365C2.32618 1.83543 4.1564 0.0615234 6.41409 0.0615234C7.98363 0.0615234 9.34558 0.918668 10.0306 2.17511C10.3431 2.08876 10.6728 2.04259 11.013 2.04259C12.9885 2.04259 14.5899 3.59476 14.5899 5.50945C14.5899 7.42415 12.9885 8.97632 11.013 8.97632H9.48003V7.98578H11.013C12.4241 7.98578 13.5679 6.87709 13.5679 5.50945C13.5679 4.14181 12.4241 3.03312 11.013 3.03312C10.6402 3.03312 10.2874 3.11019 9.9694 3.24831L9.47454 3.46326L9.28253 2.97172C8.84396 1.84897 7.72441 1.05206 6.41409 1.05206Z'
        fill={colorTheme.fg9.value}
      />
      <path
        d='M7.94706 10.4432C7.94706 10.7167 7.71828 10.9384 7.43607 10.9384C7.15386 10.9384 6.92508 10.7167 6.92508 10.4432V7.82179V5.2004L5.75344 6.33599C5.55389 6.5294 5.23035 6.5294 5.03079 6.33599C4.83124 6.14258 4.83124 5.82899 5.03079 5.63558L7.07475 3.65451C7.17058 3.56163 7.30055 3.50945 7.43607 3.50945C7.5716 3.50945 7.70157 3.56163 7.7974 3.65451L9.84135 5.63558C10.0409 5.82899 10.0409 6.14258 9.84135 6.33599C9.6418 6.5294 9.31826 6.5294 9.1187 6.33599L7.94706 5.2004V10.4432Z'
        fill={colorTheme.fg9.value}
      />
    </svg>
  )
}

const LocalChangesBlock = () => {
  const githubFileChanges = useEditorState(githubFileChangesSelector, 'Github file changes')
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

  const githubWorking = React.useMemo(() => {
    return githubOperations.length > 0
  }, [githubOperations])

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

  if (!githubAuthenticated || branch == null) {
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
            githubWorking={githubWorking}
          />
          <StringInput
            testId='commit-message-input'
            placeholder='Commit message'
            value={commitMessage || ''}
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
            disabled={githubWorking}
            spotlight
            highlight
            style={{
              gap: 4,
              background: colorTheme.secondaryBlue.value,
              borderRadius: 3,
              color: colorTheme.fg9.value,
            }}
            onMouseUp={triggerSaveToGithub}
          >
            {isGithubCommishing(githubOperations) ? (
              <GithubSpinner />
            ) : (
              <>
                <PushIcon />
                Commit and Push
              </>
            )}
          </Button>
          <Button onClick={togglePushToNewBranch}>
            {when(pushToNewBranch, <span>Or Push To Current Branch</span>)}
            {unless(pushToNewBranch, <span>Or Push To New Branch</span>)}
          </Button>
        </FlexColumn>,
      )}
    </Block>
  )
}

const PullRequestButton = () => {
  const { repo, branch } = useEditorState(
    (store) => ({
      repo:
        store.editor.githubData.publicRepositories.find(
          (r) => r.fullName === githubRepoFullName(store.editor.githubSettings.targetRepository),
        ) || null,
      branch: store.editor.githubSettings.branchName,
    }),
    'GH repo and branch',
  )
  const githubFileChanges = useEditorState(githubFileChangesSelector, 'Github file changes')
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

  if (repo == null || branch == null) {
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
        <RemoteChangesBlock />
        <LocalChangesBlock />
        <PullRequestBlock />
        <PullRequestButton />
      </Section>
    </>
  )
})
