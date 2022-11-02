/** @jsxRuntime classic */
/** @jsx jsx */
/** @jsxFrag React.Fragment */
import { jsx } from '@emotion/react'
import React from 'react'
import urljoin from 'url-join'
import { BASE_URL } from '../../../../common/env-vars'
import {
  getBranchContent,
  getBranchesForGithubRepository,
  GetBranchesResponse,
  githubFileChangesSelector,
  parseGithubProjectString,
} from '../../../../core/shared/github'
import { forceNotNull } from '../../../../core/shared/optional-utils'
import { capitalize } from '../../../../core/shared/string-utils'
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
} from '../../../editor/store/editor-state'
import { useEditorState } from '../../../editor/store/store-hook'
import { GithubFileStatus } from '../../../filebrowser/filebrowser'
import { getGithubFileStatusColor } from '../../../filebrowser/fileitem'
import { UIGridRow } from '../../../inspector/widgets/ui-grid-row'
import { GithubSpinner } from './github-spinner'
import { RepositoryListing } from './repository-listing'

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
    return store.editor.githubSettings.targetRepository
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

  const triggerSaveToGithub = React.useCallback(() => {
    if (storedTargetGithubRepo != null) {
      dispatch([EditorActions.saveToGithub(storedTargetGithubRepo)], 'everyone')
    }
  }, [dispatch, storedTargetGithubRepo])

  const [branchesForRepository, setBranchesForRepository] =
    React.useState<GetBranchesResponse | null>(null)

  React.useEffect(() => {
    if (storedTargetGithubRepo != null) {
      void getBranchesForGithubRepository(dispatch, storedTargetGithubRepo).then((result) => {
        setBranchesForRepository(result)
      })
    }
  }, [storedTargetGithubRepo, dispatch])

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
                      if (storedTargetGithubRepo != null) {
                        void getBranchContent(
                          dispatch,
                          storedTargetGithubRepo,
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

        default:
          const _exhaustiveCheck: never = branchesForRepository
          throw new Error(`Unhandled branches value ${JSON.stringify(branchesForRepository)}`)
      }
    }
  }, [
    branchesForRepository,
    storedTargetGithubRepo,
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
          <RepositoryListing
            githubAuthenticated={githubAuthenticated}
            storedTargetGithubRepo={storedTargetGithubRepo}
          />
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
              disabled={!githubAuthenticated || storedTargetGithubRepo == null || githubWorking}
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
