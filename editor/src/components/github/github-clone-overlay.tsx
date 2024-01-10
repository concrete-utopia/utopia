/** @jsxRuntime classic */
/** @jsx jsx */
/** @jsxFrag React.Fragment */
import { css, jsx, keyframes } from '@emotion/react'
import React from 'react'
import {
  AlwaysTrue,
  atomWithPubSub,
  usePubSubAtomReadOnly,
} from '../../core/shared/atom-with-pub-sub'
import type { GithubRepo } from '../editor/store/editor-state'
import { Substores, useEditorState } from '../editor/store/store-hook'
import { UtopiaStyles, colorTheme } from '../../uuiui'
import { SignInButton } from '../titlebar/title-bar'
import { AuthenticateWithGithubButton } from '../navigator/left-pane/github-pane'

export const GitRepoToLoadAtom = atomWithPubSub<GithubRepo | null>({
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
    Substores.github,
    (store) => store.editor.githubData.githubUserDetails != null,
    'GithubRepositoryCloneFlow githubUserDetails',
  )

  // const projectName = `${githubRepoObj.owner}-${githubRepoObj.repository}`
  // const githubBranch = urlParams.get('github_branch')
  // // Obtain a projectID from the server, and save an empty initial project
  // this.storedState.persistence.createNew(projectName, totallyEmptyDefaultProject())

  // const loadActionDispatchedByPersistenceMachine =
  //   await this.awaitLoadActionDispatchedByPersistenceMachine()

  // const createdProjectID = loadActionDispatchedByPersistenceMachine.projectId

  // await GithubOperations.updateProjectWithBranchContent(
  //   this.storedState.workers,
  //   this.boundDispatch,
  //   forceNotNull('Should have a project ID by now.', createdProjectID),
  //   githubRepoObj,
  //   githubBranch,
  //   false,
  //   [],
  //   builtInDependencies,
  //   {}, // Assuming a totally empty project (that is being saved probably parallel to this operation, hopefully not causing any race conditions)
  // )

  // TODO make sure the EditorState knows we have a github repo connected!!

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
        <GithubCloneDialogBox>
          <div style={{}}>
            You need to log in!
            <br />
            <SignInButton />
          </div>
        </GithubCloneDialogBox>
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
        <GithubCloneDialogBox>
          <div style={{}}>
            You have to authenticate with Github in order to clone the repository
            <br />
            <AuthenticateWithGithubButton />
          </div>
        </GithubCloneDialogBox>
      </FullScreenOverlay>
    )
  }

  return (
    <FullScreenOverlay
      style={{
        cursor: 'auto',
      }}
    >
      <GithubCloneDialogBox>
        <div style={{}}>Cloning repository RepoName...</div>
      </GithubCloneDialogBox>
    </FullScreenOverlay>
  )
})

const GithubCloneDialogBox = (props: React.PropsWithChildren<unknown>) => {
  return (
    <div
      style={{
        opacity: 1,
        fontSize: 12,
        fontWeight: 500,
        background: colorTheme.contextMenuBackground.value,
        border: `1px solid ${colorTheme.neutralBorder.value}`,
        padding: 30,
        borderRadius: 2,
        boxShadow: UtopiaStyles.shadowStyles.high.boxShadow,
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
