/** @jsxRuntime classic */
/** @jsx jsx */
/** @jsxFrag React.Fragment */
import { jsx } from '@emotion/react'
import React, { useEffect } from 'react'
import TimeAgo from 'react-timeago'
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
import { Button, FlexColumn, FlexRow, StringInput, useColorTheme } from '../../../../uuiui'
import * as EditorActions from '../../../editor/actions/action-creators'
import {
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

const compactTimeagoFormatter = (value: number, unit: string) => {
  return `${value}${unit.charAt(0)}`
}

type IndicatorState = 'pending' | 'stopped' | 'ready' | 'warning'

function getIndicatorColor(state: IndicatorState): string {
  switch (state) {
    case 'pending':
      return '#FFFFFF'
    case 'ready':
      return '#1FCCB7'
    case 'stopped':
      return '#FF7759'
    case 'warning':
      return '#F1D972'
    default:
      const _exhaustiveCheck: never = state
      throw new Error(`invalid state ${state}`)
  }
}

const IndicatorLight = ({
  state,
  hasBlockBefore,
}: {
  state: IndicatorState
  hasBlockBefore: boolean
}) => {
  const color = getIndicatorColor(state)
  return (
    <FlexColumn
      style={{
        height: 13,
        position: 'relative',
        justifyContent: 'flex-end',
      }}
    >
      {when(hasBlockBefore === true, <IndicatorLightConnector up />)}
      <svg
        style={{ zIndex: 1 }}
        width='9px'
        height='9px'
        viewBox='0 0 11 11'
        fill='none'
        xmlns='http://www.w3.org/2000/svg'
      >
        <rect x='1' y='1' width='9' height='9' rx='4.5' fill={color} />
        <rect x='1' y='1' width='9' height='9' rx='4.5' stroke='#2D2E33' />
      </svg>
    </FlexColumn>
  )
}

const IndicatorLightConnector = ({ up, down }: { up?: boolean; down?: boolean }) => {
  return (
    <div
      style={{
        flex: 1,
        width: 1,
        background: '#2D2E33',
        left: 4,
        position: 'absolute',
        top: down ? 6 : 0,
        bottom: up ? 1 : 0,
      }}
    />
  )
}

const ChevronDownIcon = () => {
  return (
    <svg
      xmlns='http://www.w3.org/2000/svg'
      width='11px'
      height='11px'
      viewBox='11 0 2 22'
      fill='none'
      stroke='currentColor'
      strokeWidth='2'
      strokeLinecap='round'
      strokeLinejoin='round'
    >
      <polyline points='6 9 12 15 18 9'></polyline>
    </svg>
  )
}

const ChevronUpIcon = () => {
  return (
    <svg
      xmlns='http://www.w3.org/2000/svg'
      width='12px'
      height='12px'
      viewBox='0 0 24 24'
      fill='none'
      stroke='currentColor'
      strokeWidth='2'
      strokeLinecap='round'
      strokeLinejoin='round'
    >
      <polyline points='18 15 12 9 6 15'></polyline>
    </svg>
  )
}

const Block = (props: {
  title: string
  subtitle?: string | JSX.Element
  state: IndicatorState
  hasBlockBefore: boolean
  hasBlockAfter: boolean
  onClick?: (e: React.MouseEvent) => void
  active?: boolean
  children: any
}) => {
  useColorTheme()
  const { title, subtitle, state, hasBlockAfter, onClick, children, active, hasBlockBefore } = props
  return (
    <UIGridRow
      variant='|--16px--|<--------auto-------->'
      padded={false}
      alignItems='stretch'
      style={{ position: 'relative' }}
    >
      <div>
        <IndicatorLight state={state} hasBlockBefore={hasBlockBefore} />
        {when(hasBlockAfter, <IndicatorLightConnector down />)}
      </div>
      <FlexColumn style={{ gap: 8 }}>
        <UIGridRow
          variant='<----------1fr---------><-auto->'
          padded={false}
          alignItems='start'
          style={{ minHeight: 0 }}
        >
          <div style={{ fontWeight: 700 }}>{title}</div>
          {when(
            subtitle != undefined,
            <FlexRow
              onClick={onClick}
              css={{
                gap: 2,
                borderRadius: 2,
                '&:hover': onClick != undefined && {
                  cursor: 'pointer',
                  background: '#eee',
                },
                background: active ? '#eee' : undefined,
                alignItems: 'center',
                justifyContent: 'center',
                paddingLeft: 4,
                paddingRight: 2,
                paddingTop: 1,
                paddingBottom: 1,
              }}
            >
              <Ellipsis style={{ maxWidth: 120 }}>{subtitle}</Ellipsis>
              {onClick && (active ? <ChevronUpIcon /> : <ChevronDownIcon />)}
            </FlexRow>,
          )}
        </UIGridRow>
        {children && (
          <UIGridRow padded={false} variant='<-------------1fr------------->'>
            {children}
          </UIGridRow>
        )}
      </FlexColumn>
    </UIGridRow>
  )
}

const AccountBlock = () => {
  const authenticated = useEditorState(
    (store) => store.userState.githubState.authenticated,
    'Github authenticated',
  )
  const email = useEditorState(
    (store) =>
      (store.userState.loginState.type === 'LOGGED_IN' && store.userState.loginState.user.email) ||
      undefined,
    'User email',
  )
  const state = React.useMemo(() => (authenticated ? 'ready' : 'pending'), [authenticated])
  const dispatch = useEditorState((store) => store.dispatch, 'dispatch')
  const triggerAuthentication = React.useCallback(() => {
    void startGithubAuthentication(dispatch)
  }, [dispatch])

  return (
    <Block
      title='Account'
      state={state}
      subtitle={authenticated ? email : undefined}
      hasBlockAfter={authenticated}
      hasBlockBefore={false}
    >
      {unless(
        authenticated,
        <div>
          <Button spotlight highlight onMouseUp={triggerAuthentication}>
            Authenticate with Github
          </Button>
        </div>,
      )}
    </Block>
  )
}

const RepositoryBlock = () => {
  const repo = useEditorState(
    (store) => store.editor.githubSettings.targetRepository,
    'Github repo',
  )
  const githubAuthenticated = useEditorState(
    (store) => store.userState.githubState.authenticated,
    'Github authenticated',
  )
  const storedTargetGithubRepo = useEditorState(
    (store) => store.editor.githubSettings.targetRepository,
    'Github repo',
  )
  const repoName = React.useMemo(
    () => (repo != null ? `${repo.owner}/${repo.repository}` : undefined),
    [repo],
  )
  const hasRepo = React.useMemo(() => repo != null, [repo])
  const [expanded, setExpanded] = React.useState(false)
  useEffect(() => {
    setExpanded(false)
  }, [repo])
  const expand = React.useCallback(() => {
    setExpanded((v) => !v)
  }, [setExpanded])

  if (!githubAuthenticated) {
    return null
  }

  return (
    <Block
      title={hasRepo ? 'Repository' : 'Select Repository'}
      state={!expanded && hasRepo != null ? 'ready' : 'pending'}
      subtitle={repoName}
      hasBlockBefore={true}
      hasBlockAfter={hasRepo}
      active={expanded}
      onClick={expand}
    >
      {(expanded || repo == null) && (
        <FlexColumn style={{ gap: 10 }}>
          <div style={{ fontSize: 10, whiteSpace: 'pre-wrap' }}>
            We only support <strong>public</strong> repositories at this time.
          </div>
          <RepositoryListing
            githubAuthenticated={githubAuthenticated}
            storedTargetGithubRepo={storedTargetGithubRepo}
          />
        </FlexColumn>
      )}
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

  const [expanded, setExpanded] = React.useState(false)
  useEffect(() => {
    setExpanded(false)
  }, [currentBranch])
  const expand = React.useCallback(() => {
    setExpanded((v) => !v)
  }, [setExpanded])

  const [branchFilter, setBranchFilter] = React.useState('')
  const updateBranchFilter = React.useCallback(
    (event: React.ChangeEvent<HTMLInputElement>) => {
      setBranchFilter(event.currentTarget.value)
    },
    [setBranchFilter],
  )
  const filteredBranches = React.useMemo(() => {
    return branchesForRepository.filter((b) => {
      if (branchFilter.length === 0) {
        return true
      }
      return b.name.includes(branchFilter)
    })
  }, [branchesForRepository, branchFilter])

  const loadBranchesUI = React.useMemo(() => {
    return (
      <>
        <UIGridRow
          padded={false}
          variant='<-------------1fr------------->'
          style={{ paddingBottom: 10 }}
        >
          <FlexColumn style={{ gap: 8 }}>
            <FlexRow style={{ gap: 4 }}>
              <StringInput
                testId='branches-input'
                placeholder='Filter…'
                style={{ flex: 1 }}
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
            </FlexRow>
            <FlexColumn
              style={{
                height: 220,
                overflowY: 'scroll',
                padding: 8,
                border: '1px solid #2D2E33',
                borderRadius: 3,
                gap: 8,
              }}
            >
              {filteredBranches.map((branch, index) => {
                function loadContentForBranch() {
                  if (storedTargetGithubRepo != null) {
                    void updateProjectWithBranchContent(
                      dispatch,
                      storedTargetGithubRepo,
                      branch.name,
                      false,
                    ).then(() => setExpanded(false))
                  }
                }
                return (
                  <FlexRow key={index} style={{ justifyContent: 'space-between' }}>
                    <Ellipsis
                      style={{
                        maxWidth: 150,
                        fontWeight: branch.name === currentBranch ? 600 : 400,
                      }}
                    >
                      {when(currentBranch === branch.name, <span>&rarr; </span>)}
                      <span title={branch.name}>{branch.name}</span>
                    </Ellipsis>
                    <Button
                      spotlight
                      highlight
                      onMouseUp={loadContentForBranch}
                      disabled={githubWorking}
                      style={{ padding: '0 6px' }}
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
                  </FlexRow>
                )
              })}
            </FlexColumn>
          </FlexColumn>
        </UIGridRow>
      </>
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
    setExpanded,
    filteredBranches,
  ])

  const githubAuthenticated = useEditorState(
    (store) => store.userState.githubState.authenticated,
    'Github authenticated',
  )

  if (!expanded && (!githubAuthenticated || storedTargetGithubRepo == null)) {
    return null
  }

  return (
    <Block
      title={currentBranch != null ? 'Branch' : 'Select Branch'}
      state={!expanded && currentBranch != null ? 'ready' : 'pending'}
      subtitle={currentBranch || undefined}
      hasBlockBefore={true}
      hasBlockAfter={currentBranch != null}
      onClick={expand}
      active={expanded}
    >
      {when(expanded || currentBranch == null, loadBranchesUI)}
    </Block>
  )
}

const PullIcon = () => {
  return (
    <svg width='16' height='13' viewBox='0 0 16 13' fill='none' xmlns='http://www.w3.org/2000/svg'>
      <path
        d='M6.91409 1.05206C5.22083 1.05206 3.84816 2.38249 3.84816 4.02365C3.84816 4.16836 3.85879 4.31037 3.87927 4.44905L3.96426 5.02454L3.36455 5.01442L3.33717 5.01419C2.49054 5.01419 1.8042 5.6794 1.8042 6.49998C1.8042 7.32057 2.49054 7.98578 3.33717 7.98578H5.89212V8.97632H3.33717C1.92611 8.97632 0.782227 7.86763 0.782227 6.49998C0.782227 5.30185 1.66014 4.30245 2.8265 4.07313C2.82629 4.05666 2.82618 4.04017 2.82618 4.02365C2.82618 1.83543 4.6564 0.0615234 6.91409 0.0615234C8.48363 0.0615234 9.84558 0.918668 10.5306 2.17511C10.8431 2.08876 11.1728 2.04259 11.513 2.04259C13.4885 2.04259 15.0899 3.59476 15.0899 5.50945C15.0899 7.42415 13.4885 8.97632 11.513 8.97632H9.98003V7.98578H11.513C12.9241 7.98578 14.0679 6.87709 14.0679 5.50945C14.0679 4.14181 12.9241 3.03312 11.513 3.03312C11.1402 3.03312 10.7874 3.11019 10.4694 3.24831L9.97454 3.46326L9.78253 2.97172C9.34396 1.84897 8.22441 1.05206 6.91409 1.05206Z'
        fill='#2D2E33'
      />
      <path
        d='M8.44706 6.00472C8.44706 5.73119 8.21828 5.50945 7.93607 5.50945C7.65386 5.50945 7.42508 5.73119 7.42508 6.00472V11.2475L6.25344 10.1119C6.05389 9.9185 5.73035 9.9185 5.53079 10.1119C5.33124 10.3053 5.33124 10.6189 5.53079 10.8123L7.57475 12.7934C7.67058 12.8863 7.80055 12.9384 7.93607 12.9384C8.0716 12.9384 8.20157 12.8863 8.2974 12.7934L10.3414 10.8123C10.5409 10.6189 10.5409 10.3053 10.3414 10.1119C10.1418 9.9185 9.81826 9.9185 9.6187 10.1119L8.44706 11.2475V6.00472Z'
        fill='#2D2E33'
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
      hasUpstreamChanges ? (bothModified.length > 0 ? 'stopped' : 'warning') : 'ready',
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
      title={hasUpstreamChanges ? 'Remote Changes' : 'No Remote Changes'}
      state={state}
      hasBlockBefore={true}
      hasBlockAfter={true}
      subtitle={
        <TimeAgo
          style={{ color: '#aaa' }}
          date={githubLastUpdatedAt || 0}
          formatter={compactTimeagoFormatter}
        />
      }
    >
      {when(
        hasUpstreamChanges,
        <FlexColumn style={{ gap: 10, paddingBottom: 10 }}>
          <GithubFileChangesList
            conflicts={bothModified}
            revertable={false}
            changes={upstreamChanges}
            showHeader={true}
            githubWorking={githubWorking}
          />
          <Button
            disabled={githubWorking}
            spotlight
            highlight
            style={{ gap: 4 }}
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
        fill='black'
      />
      <path
        d='M7.94706 10.4432C7.94706 10.7167 7.71828 10.9384 7.43607 10.9384C7.15386 10.9384 6.92508 10.7167 6.92508 10.4432V7.82179V5.2004L5.75344 6.33599C5.55389 6.5294 5.23035 6.5294 5.03079 6.33599C4.83124 6.14258 4.83124 5.82899 5.03079 5.63558L7.07475 3.65451C7.17058 3.56163 7.30055 3.50945 7.43607 3.50945C7.5716 3.50945 7.70157 3.56163 7.7974 3.65451L9.84135 5.63558C10.0409 5.82899 10.0409 6.14258 9.84135 6.33599C9.6418 6.5294 9.31826 6.5294 9.1187 6.33599L7.94706 5.2004V10.4432Z'
        fill='black'
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
    (): IndicatorState => (hasLocalChanges ? 'pending' : 'ready'),
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
  const triggerSaveToGithub = React.useCallback(() => {
    if (repo != null) {
      dispatch([EditorActions.saveToGithub(repo)], 'everyone')
    }
  }, [dispatch, repo])
  const githubAuthenticated = useEditorState(
    (store) => store.userState.githubState.authenticated,
    'Github authenticated',
  )
  const branch = useEditorState((store) => store.editor.githubSettings.branchName, 'Github branch')
  if (!githubAuthenticated || branch == null) {
    return null
  }
  return (
    <Block
      title={hasLocalChanges ? 'Local Changes' : 'No Local Changes'}
      state={state}
      hasBlockBefore={true}
      hasBlockAfter={false}
    >
      {when(
        hasLocalChanges,
        <FlexColumn style={{ gap: 10 }}>
          <GithubFileChangesList
            showHeader={true}
            revertable={true}
            changes={githubFileChanges}
            githubWorking={githubWorking}
          />
          <Button
            disabled={githubWorking}
            spotlight
            highlight
            style={{ gap: 4 }}
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
        </FlexColumn>,
      )}
    </Block>
  )
}

export const GithubPane = React.memo(() => {
  return (
    <FlexColumn style={{ padding: 10, gap: 0 }}>
      <AccountBlock />
      <RepositoryBlock />
      <BranchBlock />
      <RemoteChangesBlock />
      <LocalChangesBlock />
    </FlexColumn>
  )
})
