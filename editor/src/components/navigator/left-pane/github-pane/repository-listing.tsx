/** @jsxRuntime classic */
/** @jsx jsx */
/** @jsxFrag React.Fragment */
import { jsx } from '@emotion/react'
import React from 'react'
import TimeAgo from 'react-timeago'
import { notice } from '../../../../components/common/notice'
import { showToast } from '../../../../components/editor/actions/action-creators'
import {
  GithubRepo,
  githubRepoEquals,
  githubRepoFullName,
  isGithubLoadingBranch,
} from '../../../../components/editor/store/editor-state'
import { UIGridRow } from '../../../../components/inspector/widgets/ui-grid-row'
import {
  connectRepo,
  getUsersPublicGithubRepositories,
  parseGithubProjectString,
  RepositoryEntry,
} from '../../../../core/shared/github'
import { when } from '../../../../utils/react-conditionals'
import { Button, colorTheme, FlexColumn, FlexRow, StringInput } from '../../../../uuiui'
import { useEditorState } from '../../../editor/store/store-hook'
import { Ellipsis } from './github-file-changes-list'
import { GithubSpinner } from './github-spinner'
import { RefreshIcon } from './refresh-icon'

interface RepositoryRowProps extends RepositoryEntry {
  importPermitted: boolean
}

const RepositoryRow = (props: RepositoryRowProps) => {
  const dispatch = useEditorState((store) => store.dispatch, 'RepositoryRow dispatch')

  const githubWorking = useEditorState(
    (store) => store.editor.githubOperations.length > 0,
    'RepositoryRow githubWorking',
  )

  const [importing, setImporting] = React.useState(false)

  const importingThisBranch = useEditorState((store) => {
    if (props.defaultBranch == null) {
      return false
    } else {
      return isGithubLoadingBranch(
        store.editor.githubOperations,
        props.defaultBranch,
        store.editor.githubSettings.targetRepository,
      )
    }
  }, 'RepositoryRow importingThisBranch')

  const [previousImportingThisBranch, setPreviousImportingThisBranch] =
    React.useState(importingThisBranch)

  // Should reset the spinner which is tied to a specific branch and repository.
  if (importingThisBranch !== previousImportingThisBranch) {
    setPreviousImportingThisBranch(importingThisBranch)
    if (!importingThisBranch) {
      setImporting(false)
    }
  }

  const currentRepo = useEditorState(
    (store) => store.editor.githubSettings.targetRepository,
    'Current Github repository',
  )

  const importRepository = React.useCallback(() => {
    if (githubWorking) {
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
      dispatch(connectRepo(isAnotherRepo, parsedTargetRepository, null, null))
    }
  }, [dispatch, props.fullName, props.defaultBranch, currentRepo, githubWorking])

  return (
    <UIGridRow
      padded
      variant='<----------1fr---------><-auto->'
      tall={true}
      css={{
        cursor: importing
          ? 'wait'
          : githubWorking || !props.importPermitted
          ? 'not-allowed'
          : 'pointer',
        opacity: githubWorking || !props.importPermitted ? 0.5 : 1,
        '&:hover': {
          background: colorTheme.primarySubdued.value,
          color: colorTheme.white.value,
          svg: { stroke: colorTheme.white.value },
        },
      }}
      onClick={importRepository}
    >
      <div>
        <Ellipsis style={{ maxWidth: 140 }}>{props.fullName}</Ellipsis>
        <span style={{ fontSize: 10, opacity: 0.5 }}>
          {props.isPrivate ? 'private' : 'public'}
          {props.updatedAt == null ? null : (
            <>
              {' '}
              &middot; <TimeAgo date={props.updatedAt} />
            </>
          )}
        </span>
      </div>
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

    const usersRepositories = useEditorState(
      (store) => store.editor.githubData.publicRepositories,
      'Github repositories',
    )

    const filteredRepositories = React.useMemo(() => {
      let filteredResult: Array<RepositoryRowProps> = []
      for (const repository of usersRepositories) {
        // Only include a repository if the user can push to it.
        if (repository.permissions.push) {
          filteredResult.push({
            ...repository,
            importPermitted: true,
          })
        }
      }
      if (targetRepository != null) {
        filteredResult = filteredResult.filter((repository) => {
          return repository.fullName.includes(targetRepository)
        })
      }
      return filteredResult
    }, [usersRepositories, targetRepository])

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
            const additionalEntry: RepositoryRowProps = {
              fullName: parsedRepo.repository,
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
            }
            return [...filteredRepositories, additionalEntry]
          }
        }
      }
    }, [filteredRepositories, targetRepository])

    const githubOperations = useEditorState(
      (store) => store.editor.githubOperations,
      'Github operations',
    )
    const githubWorking = React.useMemo(() => githubOperations.length > 0, [githubOperations])
    const isLoadingRepositories = React.useMemo(
      () => githubOperations.some((op) => op.name === 'loadRepositories'),
      [githubOperations],
    )
    const dispatch = useEditorState((store) => store.dispatch, 'dispatch')

    const refreshRepos = React.useCallback(() => {
      void getUsersPublicGithubRepositories(dispatch)
    }, [dispatch])

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
          disabled={githubWorking}
          onMouseDown={refreshRepos}
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
          <a href='https://github.com/new' target='_blank' rel='noopener noreferrer'>
            Create new repository on Github.
          </a>
        </UIGridRow>
      </FlexColumn>
    )
  },
)
