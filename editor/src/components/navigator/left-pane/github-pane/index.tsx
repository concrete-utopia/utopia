/** @jsxRuntime classic */
/** @jsx jsx */
/** @jsxFrag React.Fragment */
import { jsx } from '@emotion/react'
import React, { ChangeEventHandler, useCallback, useEffect } from 'react'
import TimeAgo from 'react-timeago'
import urljoin from 'url-join'
import { BASE_URL } from '../../../../common/env-vars'
import {
  getBranchesForGithubRepository,
  getGithubFileChangesCount,
  githubFileChangesSelector,
  githubFileChangesToList,
  parseGithubProjectString,
  updateProjectAgainstGithub,
  updateProjectWithBranchContent,
} from '../../../../core/shared/github'
import { NO_OP } from '../../../../core/shared/utils'
import { startGithubAuthentication } from '../../../../utils/github-auth'
import { unless, when } from '../../../../utils/react-conditionals'
import {
  Button,
  FlexColumn,
  FlexRow,
  Section,
  SectionBodyArea,
  SectionTitleRow,
  StringInput,
  Title,
  useColorTheme,
  UtopiaTheme,
} from '../../../../uuiui'
import { WarningIcon } from '../../../../uuiui/warning-icon'
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

const compactTimeagoFormatter = (value: number, unit: string) => {
  return `${value}${unit.charAt(0)}`
}

type SemaphoreState = 'pending' | 'stopped' | 'ready' | 'warning'

function getSemaphoreColor(state: SemaphoreState): string {
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

const Semaphore = ({ state }: { state: SemaphoreState }) => {
  const color = getSemaphoreColor(state)
  return (
    <FlexColumn
      style={{
        alignItems: 'center',
        justifyContent: 'center',
        height: 20,
      }}
    >
      <svg
        width='10px'
        height='10px'
        viewBox='0 0 10 10'
        fill='none'
        xmlns='http://www.w3.org/2000/svg'
      >
        <rect x='0.5' y='0.5' width='8' height='8' rx='4.5' fill={color} />
        <rect x='0.5' y='0.5' width='8' height='8' rx='4.5' stroke='#2D2E33' />
      </svg>
    </FlexColumn>
  )
}

const Connector = () => {
  return (
    <div
      style={{
        width: 1,
        flex: 1,
        background: '#2D2E33',
        marginLeft: 4,
        marginTop: -6,
        marginBottom: -5,
      }}
    />
  )
}

const Block = (props: {
  title: string
  subtitle?: string | JSX.Element
  state: SemaphoreState
  hasMore: boolean
  onClick?: (e: React.MouseEvent) => void
  active?: boolean
  children: any
}) => {
  useColorTheme()
  const { title, subtitle, state, hasMore, onClick, children, active } = props
  return (
    <FlexRow
      style={{
        alignItems: 'stretch',
      }}
    >
      <FlexColumn>
        <Semaphore state={state} />
        {when(hasMore, <Connector />)}
      </FlexColumn>
      <FlexColumn
        style={{
          gap: 4,
          paddingBottom: 10,
          flexGrow: 1,
          boxSizing: 'border-box',
          flex: '1 1 100%',
        }}
      >
        <FlexRow
          style={{
            justifyContent: 'space-between',
            height: 20,
            borderRadius: 2,
            padding: '0 5px',
            margin: '0 5px',
          }}
          css={{
            background: (active && '#ddd') || undefined,
            '&:hover':
              (onClick != undefined && {
                cursor: 'pointer',
                background: '#ddd',
              }) ||
              undefined,
          }}
          onClick={onClick}
        >
          <span style={{ fontWeight: 700 }}>{title}</span>
          {when(subtitle != undefined, <Ellipsis style={{ maxWidth: 120 }}>{subtitle}</Ellipsis>)}
        </FlexRow>
        {children}
      </FlexColumn>
    </FlexRow>
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
      hasMore={authenticated}
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
      state={hasRepo ? 'ready' : 'pending'}
      subtitle={repoName}
      hasMore={hasRepo}
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

  const loadBranchesUI = React.useMemo(() => {
    return (
      <>
        <FlexColumn style={{ gap: 10, padding: 4 }}>
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
            {branchesForRepository
              .filter((b) => {
                if (branchFilter.length === 0) {
                  return true
                }
                return b.name.includes(branchFilter)
              })
              .map((branch, index) => {
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
    branchFilter,
    updateBranchFilter,
    setExpanded,
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
      hasMore={currentBranch != null}
      onClick={expand}
      active={expanded}
    >
      {when(
        expanded || currentBranch == null,
        <FlexColumn style={{ gap: 10 }}>{loadBranchesUI}</FlexColumn>,
      )}
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
    (): SemaphoreState =>
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
      hasMore={true}
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
        <FlexColumn style={{ gap: 10 }}>
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
    (): SemaphoreState => (hasLocalChanges ? 'pending' : 'ready'),
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
      hasMore={false}
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
    <FlexColumn style={{ padding: 10 }}>
      <AccountBlock />
      <RepositoryBlock />
      <BranchBlock />
      <RemoteChangesBlock />
      <LocalChangesBlock />
    </FlexColumn>
  )
})

export const GithubPane2 = React.memo(() => {
  const [importGithubRepoStr, setImportGithubRepoStr] = React.useState('')
  const parsedImportRepo = parseGithubProjectString(importGithubRepoStr)
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
        {when(
          storedTargetGithubRepo != null,
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
  ])

  const githubFileChanges = useEditorState(githubFileChangesSelector, 'Github file changes')
  const githubLastUpdatedAt = useEditorState(
    (store) => store.editor.githubData.lastUpdatedAt,
    'Github last updated',
  )
  const upstreamChanges = useEditorState(
    (store) => store.editor.githubData.upstreamChanges,
    'Github upstream changes',
  )
  const upstreamChangesCount = React.useMemo(
    () => getGithubFileChangesCount(upstreamChanges),
    [upstreamChanges],
  )
  const hasUpstreamChanges = React.useMemo(() => {
    return upstreamChangesCount > 0
  }, [upstreamChangesCount])

  const bothModified = React.useMemo(() => {
    const upstreamList = githubFileChangesToList(upstreamChanges)
    const localList = githubFileChangesToList(githubFileChanges)
    const intersection = upstreamList
      .filter((upstream) => localList.some((local) => local.filename === upstream.filename))
      .map((change) => change.filename)
    return intersection
  }, [upstreamChanges, githubFileChanges])

  const disconnectFromGithub = useCallback(() => {
    if (currentBranch != null) {
      dispatch(
        [
          EditorActions.showModal({
            type: 'disconnect-github-project',
          }),
        ],
        'everyone',
      )
    }
  }, [dispatch, currentBranch])

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
              <GithubFileChangesList
                showHeader={true}
                revertable={true}
                changes={githubFileChanges}
                githubWorking={githubWorking}
              />
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
                <div
                  style={{
                    padding: '10px 0',
                    display: 'flex',
                    justifyContent: 'space-between',
                  }}
                >
                  {when(
                    hasUpstreamChanges,
                    <div>
                      <div
                        style={{
                          display: 'flex',
                          gap: 4,
                          alignItems: 'center',
                        }}
                      >
                        <FlexRow style={{ gap: 2, color: '#f90' }}>
                          Upstream:
                          <FlexRow>
                            {upstreamChangesCount} file{upstreamChangesCount !== 1 ? 's' : ''}{' '}
                            changed
                          </FlexRow>
                        </FlexRow>
                      </div>
                      <GithubFileChangesList
                        showHeader={false}
                        revertable={false}
                        conflicts={bothModified}
                        changes={upstreamChanges}
                        githubWorking={githubWorking}
                      />
                    </div>,
                  )}
                  {unless(hasUpstreamChanges, <span>Upstream: up-to-date.</span>)}
                  <div style={{ color: '#aaa' }}>
                    <TimeAgo date={githubLastUpdatedAt || 0} formatter={compactTimeagoFormatter} />
                  </div>
                </div>
                <Button
                  spotlight
                  highlight
                  disabled={!githubAuthenticated || storedTargetGithubRepo == null || githubWorking}
                  onMouseUp={triggerUpdateAgainstGithub}
                >
                  {isGithubUpdating(githubOperations) ? (
                    <GithubSpinner />
                  ) : (
                    <>
                      {bothModified.length > 0 && <WarningIcon />}
                      Update Against Github
                    </>
                  )}
                </Button>
              </UIGridRow>
            </>,
          )}
          {loadBranchesUI}
          {when(
            currentBranch != null,
            <UIGridRow
              padded
              variant='<-------------1fr------------->'
              style={{ margin: '10px 0' }}
            >
              <Button spotlight highlight onClick={disconnectFromGithub} disabled={githubWorking}>
                Disconnect from branch
              </Button>
            </UIGridRow>,
          )}
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
