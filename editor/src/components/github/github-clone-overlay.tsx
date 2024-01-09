/** @jsxRuntime classic */
/** @jsx jsx */
/** @jsxFrag React.Fragment */
import { css, jsx, keyframes } from '@emotion/react'
import React from 'react'

export const GithubRepositoryCloneFlow = React.memo(() => {
  // const projectName = `${githubRepoObj.owner}-${githubRepoObj.repository}`

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

  return (
    <FullScreenOverlay
      style={{
        backgroundColor: '#00000033',
        cursor: 'auto',
      }}
    >
      <div style={{ color: 'white' }}>hello</div>
    </FullScreenOverlay>
  )
})

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
