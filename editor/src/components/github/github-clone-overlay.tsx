/** @jsxRuntime classic */
/** @jsx jsx */
/** @jsxFrag React.Fragment */
import { css, jsx, keyframes } from '@emotion/react'
import React from 'react'
import {
  AlwaysTrue,
  atomWithPubSub,
  updatePubSubAtomFromOutsideReact,
  usePubSubAtomReadOnly,
} from '../../core/shared/atom-with-pub-sub'
import { GithubOperations } from '../../core/shared/github/operations'
import { forceNotNull } from '../../core/shared/optional-utils'
import { NO_OP } from '../../core/shared/utils'
import { totallyEmptyDefaultProject } from '../../sample-projects/sample-project-utils'
import invariant from '../../third-party/remix/invariant'
import { Dialog, FormButton, UtopiaStyles, colorTheme } from '../../uuiui'
import type { EditorDispatch } from '../editor/action-types'
import { useDispatch } from '../editor/store/dispatch-context'
import { type EditorStorePatched, type GithubRepoWithBranch } from '../editor/store/editor-state'
import { Substores, useEditorState, useRefEditorState } from '../editor/store/store-hook'
import { AuthenticateWithGithubButton } from '../navigator/left-pane/github-pane'
import { onClickSignIn } from '../titlebar/title-bar'

export const LoadActionsDispatched = 'loadActionDispatched'

export const GitRepoToLoadAtom = atomWithPubSub<GithubRepoWithBranch | null>({
  key: 'GitRepoToLoadAtom',
  defaultValue: null,
})

export const GithubRepositoryCloneFlow = React.memo(() => {
  const githubRepo = usePubSubAtomReadOnly(GitRepoToLoadAtom, AlwaysTrue)
  const userLoggedIn = useEditorState(
    Substores.userState,
    (store) => store.userState.loginState.type === 'LOGGED_IN',
    'GithubRepositoryCloneFlow userLoggedIn',
  )

  const githubAuthenticated = useEditorState(
    Substores.userState,
    (store) => store.userState.githubState.authenticated,
    'GithubRepositoryCloneFlow githubAuthenticated',
  )

  if (githubRepo == null) {
    // we don't want to load anything, so just return null to hide this overlay
    return null
  }
  if (!userLoggedIn) {
    // we want to prompt the user to log in
    return (
      <FullScreenOverlay
        style={{
          cursor: 'auto',
        }}
      >
        <Dialog
          title='Not Signed In'
          content={<>You need to be signed in to be able to clone a repository from GitHub</>}
          closeCallback={NO_OP}
          defaultButton={
            <FormButton primary onClick={onClickSignIn}>
              Sign In To Utopia
            </FormButton>
          }
        />
      </FullScreenOverlay>
    )
  }
  if (!githubAuthenticated) {
    // we want to prompt the user to log authenticate their github
    return (
      <FullScreenOverlay
        style={{
          cursor: 'auto',
        }}
      >
        <Dialog
          title='Connect to Github'
          content={
            <>
              You need to connect Utopia with your Github account to be able to access Github
              repositories
            </>
          }
          closeCallback={NO_OP}
          defaultButton={<AuthenticateWithGithubButton />}
        />
      </FullScreenOverlay>
    )
  }

  // The GitClonePseudoElement triggers the actual repo cloning
  return <GitClonePseudoElement githubRepo={githubRepo} />
})

let githubRepoAlreadyCloning = false
async function cloneGithubRepo(
  dispatch: EditorDispatch,
  storeRef: { current: EditorStorePatched },
  githubRepo: GithubRepoWithBranch,
) {
  if (githubRepoAlreadyCloning) {
    return
  }
  githubRepoAlreadyCloning = true
  const projectName = `${githubRepo.owner}-${githubRepo.repository}`

  const githubBranch = githubRepo.branch
  // Obtain a projectID from the server, and save an empty initial project
  storeRef.current.persistence.createNew(projectName, totallyEmptyDefaultProject())
  const loadActionDispatchedByPersistenceMachine =
    await awaitLoadActionDispatchedByPersistenceMachine()
  const createdProjectID = loadActionDispatchedByPersistenceMachine.projectId
  await GithubOperations.updateProjectWithBranchContent(
    storeRef.current.workers,
    dispatch,
    forceNotNull('Should have a project ID by now.', createdProjectID),
    githubRepo,
    githubBranch,
    false,
    [],
    storeRef.current.builtInDependencies,
    {}, // Assuming a totally empty project (that is being saved probably parallel to this operation, hopefully not causing any race conditions)
  )

  // at this point we can assume the repo is loaded and we can finally hide the overlay
  updatePubSubAtomFromOutsideReact(GitRepoToLoadAtom, 'async', null)

  // TODO make sure the EditorState knows we have a github repo connected!!!
}

const GitClonePseudoElement = React.memo((props: { githubRepo: GithubRepoWithBranch }) => {
  const { githubRepo } = props
  const dispatch = useDispatch()

  const editorStoreRef = useRefEditorState((store) => store)

  React.useEffect(() => {
    void cloneGithubRepo(dispatch, editorStoreRef, githubRepo)
    // eslint-disable-next-line react-hooks/exhaustive-deps
  }, [])

  // The GitClonePseudoElement's sole job is to call cloneGithubRepo in a useEffect.
  // I pulled it to a dedicated component so it's purpose remains clear and this useEffect doesn't get lost in the noise
  return null
})

const awaitLoadActionDispatchedByPersistenceMachine = (): Promise<{ projectId: string }> => {
  // TODO move this WAY out of Editor.tsx
  invariant(
    PubSub.countSubscriptions(LoadActionsDispatched) === 0,
    'At this point, awaitLoadActionDispatchedByPersistenceMachine should have zero listeners',
  )
  return new Promise((resolve, reject) => {
    const listener = (message: string, data: { projectId: string }) => {
      PubSub.unsubscribe(listener)
      resolve(data)
    }
    PubSub.subscribe(LoadActionsDispatched, listener)
  })
}

const GithubCloneDialogBox = (props: React.PropsWithChildren<unknown>) => {
  return (
    <div
      style={{
        display: 'flex',
        flexDirection: 'column',
        width: 300,
        opacity: 1,
        fontSize: 12,
        fontWeight: 500,
        background: colorTheme.contextMenuBackground.value,
        border: `1px solid ${colorTheme.neutralBorder.value}`,
        padding: 30,
        borderRadius: 2,
        boxShadow: UtopiaStyles.shadowStyles.high.boxShadow,
        whiteSpace: 'initial',
      }}
    >
      {props.children}
    </div>
  )
}

function handleEventNoop(e: React.MouseEvent | React.KeyboardEvent) {
  e.stopPropagation()
  e.preventDefault()
}

export const FullScreenOverlay = (
  props: React.PropsWithChildren<{ style?: React.CSSProperties }>,
) => {
  const anim = keyframes`
    from {
      opacity: 0;
    }
    to {
      opacity: 0.2;
    }
  `

  return (
    <div
      onMouseDown={handleEventNoop}
      onMouseUp={handleEventNoop}
      onClick={handleEventNoop}
      onKeyDown={handleEventNoop}
      onKeyUp={handleEventNoop}
      style={{
        position: 'fixed',
        top: 0,
        left: 0,
        right: 0,
        bottom: 0,
        backgroundColor: '#00000033',
        zIndex: 30,
        transition: 'all .1s ease-in-out',
        display: 'flex',
        flexDirection: 'column',
        alignItems: 'center',
        justifyContent: 'center',
        cursor: 'wait',
        ...props.style,
      }}
      css={css`
        animation: ${anim} 0.3s ease-in-out;
      `}
    >
      {props.children}
    </div>
  )
}
