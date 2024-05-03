/** @jsxRuntime classic */
/** @jsx jsx */
/** @jsxFrag React.Fragment */
import { jsx } from '@emotion/react'
import React, { useMemo } from 'react'
import TimeAgo from 'react-timeago'
import { notice } from '../../../../components/common/notice'
import {
  showToast,
  updateGithubSettings,
} from '../../../../components/editor/actions/action-creators'
import type { GithubRepo } from '../../../../components/editor/store/editor-state'
import {
  emptyGithubSettings,
  githubRepoEquals,
  githubRepoFullName,
  isGithubLoadingBranch,
  isGithubLoadingRepositories,
} from '../../../../components/editor/store/editor-state'
import { UIGridRow } from '../../../../components/inspector/widgets/ui-grid-row'
import type { RepositoryEntry } from '../../../../core/shared/github/helpers'
import { connectRepo, parseGithubProjectString } from '../../../../core/shared/github/helpers'
import { unless, when } from '../../../../utils/react-conditionals'
import { Button, colorTheme, FlexColumn, FlexRow, StringInput } from '../../../../uuiui'
import { useDispatch } from '../../../editor/store/dispatch-context'
import { Substores, useEditorState } from '../../../editor/store/store-hook'
import { Ellipsis } from './github-file-changes-list'
import { GithubSpinner } from './github-spinner'
import { RefreshIcon } from './refresh-icon'
import { GithubOperations } from '../../../../core/shared/github/operations'
import type { EditorDispatch } from '../../../editor/action-types'
import { useGridPanelState } from '../../../canvas/grid-panels-state'
import { GridMenuMinWidth } from '../../../canvas/stored-layout'

interface RepositoryRowProps extends RepositoryEntry {
  importPermitted: boolean
  searchable: boolean
}

const RepositoryRow = (props: RepositoryRowProps) => {
  const dispatch = useDispatch()

  const [importing, setImporting] = React.useState(false)

  const githubOperations = useEditorState(
    Substores.github,
    (store) => store.editor.githubOperations,
    'RepositoryRow githubOperations',
  )

  const currentRepo = useEditorState(
    Substores.github,
    (store) => store.editor.githubSettings.targetRepository,
    'RepositoryRow currentRepo',
  )

  const loadingRepos = useMemo(() => {
    return isGithubLoadingRepositories(githubOperations)
  }, [githubOperations])

  const importingThisBranch = useMemo(() => {
    if (props.defaultBranch == null) {
      return false
    } else {
      return isGithubLoadingBranch(githubOperations, props.defaultBranch, currentRepo)
    }
  }, [props.defaultBranch, githubOperations, currentRepo])

  const [previousImportingThisBranch, setPreviousImportingThisBranch] =
    React.useState(importingThisBranch)

  // Should reset the spinner which is tied to a specific branch and repository.
  if (importingThisBranch !== previousImportingThisBranch) {
    setPreviousImportingThisBranch(importingThisBranch)
    if (!importingThisBranch) {
      setImporting(false)
    }
  }

  const importRepository = React.useCallback(() => {
    if (loadingRepos) {
      return
    }
    const parsedTargetRepository = parseGithubProjectString(props.fullName)
    if (parsedTargetRepository == null || props.defaultBranch == null) {
      dispatch(
        [
          showToast(
            notice(
              `Error when attempting to import a repository with repo: ${props.fullName}`,
              'ERROR',
            ),
          ),
        ],
        'everyone',
      )
    } else {
      const isAnotherRepo = !githubRepoEquals(parsedTargetRepository, currentRepo)
      dispatch(connectRepo(isAnotherRepo, parsedTargetRepository, null, null, isAnotherRepo))
    }
  }, [dispatch, props.fullName, props.defaultBranch, loadingRepos, currentRepo])

  const githubUserDetails = useEditorState(
    Substores.github,
    (store) => store.editor.githubData.githubUserDetails,
    'RepositoryRow githubUserDetails',
  )

  const searchPublicRepo = React.useCallback(
    (e: React.MouseEvent) => {
      e.stopPropagation()
      if (githubUserDetails != null) {
        void searchPublicRepoFromString(dispatch, githubUserDetails.login, props.fullName)
      }
    },
    [dispatch, githubUserDetails, props],
  )

  const [panelState] = useGridPanelState()
  const panelWidth = React.useMemo(() => {
    return (
      panelState.find((panel) => panel.panels.some(({ name }) => name === 'navigator'))
        ?.menuWidth ?? GridMenuMinWidth
    )
  }, [panelState])

  return (
    <UIGridRow
      padded
      variant='<----------1fr---------><-auto->'
      tall={true}
      css={{
        cursor: importing
          ? 'wait'
          : loadingRepos || !props.importPermitted
          ? 'not-allowed'
          : 'pointer',
        opacity: loadingRepos || (!props.importPermitted && !props.searchable) ? 0.5 : 1,
        '&:hover': {
          background: colorTheme.dynamicBlue.value,
          color: colorTheme.bg1.value,
          svg: { stroke: colorTheme.bg1.value },
          '.search-button': {
            color: colorTheme.fg0.value,
          },
        },
        maxWidth: panelWidth,
      }}
      onClick={importRepository}
    >
      <div>
        <Ellipsis style={{ maxWidth: panelWidth - 150 }} title={props.fullName}>
          {props.fullName}
        </Ellipsis>
        <span style={{ fontSize: 10, opacity: 0.5 }}>
          {unless(
            props.searchable,
            <React.Fragment>
              {props.isPrivate ? 'private' : 'public'}
              {props.updatedAt == null ? null : (
                <>
                  {' '}
                  &middot; <TimeAgo date={props.updatedAt} />
                </>
              )}
            </React.Fragment>,
          )}
        </span>
      </div>
      {when(
        props.searchable,
        <Button
          className='search-button'
          highlight
          spotlight
          style={{ padding: '0 6px' }}
          onClick={searchPublicRepo}
        >
          Search
        </Button>,
      )}
      {when(importing, <GithubSpinner />)}
    </UIGridRow>
  )
}

interface RepositoryListingProps {
  githubAuthenticated: boolean
  storedTargetGithubRepo: GithubRepo | null
}

export const RepositoryListing = React.memo(
  ({ githubAuthenticated, storedTargetGithubRepo }: RepositoryListingProps) => {
    const storedTargetGithubRepoAsText = React.useMemo(() => {
      if (storedTargetGithubRepo == null) {
        return undefined
      } else {
        return `${storedTargetGithubRepo.owner}/${storedTargetGithubRepo.repository}`
      }
    }, [storedTargetGithubRepo])
    const [previousStoredTarget, setPreviousStoredTarget] = React.useState<string | undefined>(
      undefined,
    )
    const [targetRepository, setTargetRepository] = React.useState<string | undefined>(
      storedTargetGithubRepoAsText,
    )
    if (storedTargetGithubRepoAsText !== previousStoredTarget) {
      // Since the storedTargetGithubRepoAsText value changed, update targetRepository.
      setTargetRepository(storedTargetGithubRepoAsText)
      setPreviousStoredTarget(storedTargetGithubRepoAsText)
    }

    const onInputChangeTargetRepository = React.useCallback(
      (event: React.ChangeEvent<HTMLInputElement>) => {
        setTargetRepository(event.currentTarget.value)
      },
      [setTargetRepository],
    )

    const githubUserDetails = useEditorState(
      Substores.github,
      (store) => store.editor.githubData.githubUserDetails,
      'RepositoryListing githubUserDetails',
    )

    const repositories = useEditorState(
      Substores.github,
      (store) => [
        ...store.editor.githubData.userRepositories,
        ...store.editor.githubData.publicRepositories,
      ],
      'Github repositories',
    )

    const filteredRepositories = React.useMemo(() => {
      let filteredResult: Array<RepositoryRowProps> = []
      for (const repository of repositories) {
        // Only include a repository if the user can pull it.
        if (repository.permissions.pull) {
          // TODO make sure to disable commit/push blocks if the repo does not have push permissions
          filteredResult.push({
            ...repository,
            importPermitted: true,
            searchable: false,
          })
        }
      }
      if (targetRepository != null) {
        filteredResult = filteredResult.filter((repository) => {
          return repository.fullName.includes(targetRepository)
        })
      }
      return filteredResult
    }, [repositories, targetRepository])

    const filteredRepositoriesWithSpecialCases = React.useMemo(() => {
      if (filteredRepositories == null) {
        return null
      } else {
        const parsedRepo =
          targetRepository == null ? null : parseGithubProjectString(targetRepository)
        if (parsedRepo == null) {
          return filteredRepositories
        } else {
          const ownerRepo = githubRepoFullName(parsedRepo)
          const alreadyIncludesEntry =
            filteredRepositories?.some((repo) => repo.fullName === ownerRepo) ?? false
          if (alreadyIncludesEntry) {
            return filteredRepositories
          } else {
            const fullName = `${parsedRepo.owner}/${parsedRepo.repository}`
            const isSearchable =
              githubUserDetails != null &&
              isSearchableRepository(
                lookupSearchableRepositoryDetails(githubUserDetails.login, fullName),
              )

            const additionalEntry: RepositoryRowProps = {
              fullName: fullName,
              name: parsedRepo.repository,
              avatarUrl: null,
              isPrivate: true,
              description: null,
              updatedAt: null,
              defaultBranch: 'main',
              importPermitted: false,
              permissions: {
                admin: false,
                push: false,
                pull: false,
              },
              searchable: isSearchable,
            }
            return [...filteredRepositories, additionalEntry]
          }
        }
      }
    }, [filteredRepositories, targetRepository, githubUserDetails])

    const githubOperations = useEditorState(
      Substores.github,
      (store) => store.editor.githubOperations,
      'Github operations',
    )
    const isLoadingRepositories = React.useMemo(
      () => githubOperations.some((op) => op.name === 'loadRepositories'),
      [githubOperations],
    )

    const currentRepo = useEditorState(
      Substores.github,
      (store) => store.editor.githubSettings.targetRepository,
      'Github targetRepository',
    )

    const dispatch = useDispatch()

    const refreshReposOnClick = React.useCallback(() => {
      void GithubOperations.getUsersPublicGithubRepositories(
        dispatch,
        'user-initiated',
        currentRepo,
      ).then((actions) => {
        dispatch(actions, 'everyone')
      })
    }, [dispatch, currentRepo])

    const clearRepository = React.useCallback(() => {
      dispatch([updateGithubSettings(emptyGithubSettings())], 'everyone')
    }, [dispatch])

    const onKeyDown = React.useCallback(
      (e: React.KeyboardEvent) => {
        if (e.key === 'Enter' && targetRepository != null && githubUserDetails != null) {
          void searchPublicRepoFromString(dispatch, githubUserDetails.login, targetRepository)
          return
        }
      },
      [targetRepository, dispatch, githubUserDetails],
    )

    if (!githubAuthenticated) {
      return null
    }

    return (
      <FlexColumn style={{ gap: 4 }}>
        <UIGridRow padded={false} variant='<-------------1fr------------->'>
          <StringInput
            placeholder={
              filteredRepositoriesWithSpecialCases == null
                ? 'Loading repositories...'
                : 'owner/repository'
            }
            onChange={onInputChangeTargetRepository}
            onKeyDown={onKeyDown}
            list={'repositories-list'}
            id={'repositories-input'}
            testId={'repositories-input'}
            name={'repositories-input'}
            value={targetRepository}
          />
        </UIGridRow>
        <FlexColumn
          style={{
            height: 220,
            overflowY: 'scroll',
            border: `1px solid ${colorTheme.githubBoxesBorder.value}`,
            borderRadius: 2,
          }}
        >
          {filteredRepositoriesWithSpecialCases == null ? (
            <div style={{ display: 'flex', height: '100%' }}>
              <div style={{ margin: 'auto', position: 'relative' }}>
                <GithubSpinner />
              </div>
            </div>
          ) : (
            filteredRepositoriesWithSpecialCases.map((repository, index) => {
              return <RepositoryRow key={`repo-${index}`} {...repository} />
            })
          )}
        </FlexColumn>
        <Button
          spotlight
          highlight
          style={{ padding: '0 6px' }}
          disabled={isLoadingRepositories}
          onMouseDown={refreshReposOnClick}
        >
          {isLoadingRepositories ? (
            <GithubSpinner />
          ) : (
            <FlexRow style={{ gap: 4 }}>
              <RefreshIcon /> Refresh list
            </FlexRow>
          )}
        </Button>
        <UIGridRow padded={false} variant='<-------------1fr------------->'>
          <a
            href='https://github.com/new'
            target='_blank'
            rel='noopener noreferrer'
            style={{ color: colorTheme.dynamicBlue.value }}
          >
            Create new repository on Github.
          </a>
        </UIGridRow>
        {when(
          targetRepository != null,
          <Button
            spotlight
            highlight
            style={{ color: colorTheme.errorForeground.value }}
            onClick={clearRepository}
          >
            Clear repository
          </Button>,
        )}
      </FlexColumn>
    )
  },
)

type NotSearchableRepository = { type: 'not-searchable' }
type SearchableRepository = { type: 'searchable'; owner: string; repo: string }

type SearchableRepositoryDetails = NotSearchableRepository | SearchableRepository

function isSearchableRepository(
  details: SearchableRepositoryDetails,
): details is SearchableRepository {
  return details.type === 'searchable'
}

function lookupSearchableRepositoryDetails(
  login: string,
  fullName: string,
): SearchableRepositoryDetails {
  const parts = fullName.trim().toLowerCase().split('/')
  if (parts.length !== 2) {
    return { type: 'not-searchable' }
  }
  const [owner, repo] = parts
  if (owner === login) {
    return { type: 'not-searchable' }
  }
  if (owner.length < 1 || repo.length < 1) {
    return { type: 'not-searchable' }
  }

  return { type: 'searchable', owner: owner, repo: repo }
}

async function searchPublicRepoFromString(
  dispatch: EditorDispatch,
  login: string,
  fullName: string,
) {
  const details = lookupSearchableRepositoryDetails(login, fullName)
  if (!isSearchableRepository(details)) {
    return
  }

  const actions = await GithubOperations.searchPublicGithubRepository(dispatch, 'user-initiated', {
    owner: details.owner,
    repo: details.repo,
  })

  dispatch(actions, 'everyone')
}
