import React, { useCallback } from 'react'
import { createSelector } from 'reselect'
import { auth0Url } from '../../common/env-vars'
import { getUserPicture } from '../../common/user'
import { getGithubFileChangesCount, useGithubFileChanges } from '../../core/shared/github/helpers'
import { unless, when } from '../../utils/react-conditionals'
import {
  Avatar,
  Button,
  colorTheme,
  Icons,
  LargerIcons,
  SimpleFlexRow,
  UNSAFE_getIconURL,
} from '../../uuiui'
import type { LoginState } from '../../uuiui-deps'
import type { EditorAction } from '../editor/action-types'
import {
  openCodeEditorFile,
  setLeftMenuTab,
  setPanelVisibility,
  togglePanel,
} from '../editor/actions/action-creators'
import { useDispatch } from '../editor/store/dispatch-context'
import type { EditorStoreShared } from '../editor/store/editor-state'
import { EditorStorePatched, githubRepoFullName, LeftMenuTab } from '../editor/store/editor-state'
import { Substores, useEditorState } from '../editor/store/store-hook'
import { RoundButton } from './buttons'
import { TestMenu } from './test-menu'

interface ProjectTitleProps {}

const ProjectTitle: React.FC<React.PropsWithChildren<ProjectTitleProps>> = ({ children }) => {
  return (
    <div
      style={{
        fontWeight: 400,
        fontSize: 12,
        padding: '0 10px',
        color: colorTheme.fg0.value,
      }}
    >
      {children}
    </div>
  )
}

const TitleBar = React.memo(() => {
  const dispatch = useDispatch()
  const { loginState } = useEditorState(
    Substores.restOfStore,
    (store) => ({
      loginState: store.userState.loginState,
    }),
    'TitleBar loginState',
  )
  const projectName = useEditorState(
    Substores.restOfEditor,
    (store) => {
      return store.editor.projectName
    },
    'TitleBar projectName',
  )

  const { upstreamChanges, currentBranch, treeConflicts, repoName } = useEditorState(
    Substores.github,
    (store) => {
      return {
        upstreamChanges: store.editor.githubData.upstreamChanges,
        currentBranch: store.editor.githubSettings.branchName,
        treeConflicts: store.editor.githubData.treeConflicts,
        repoName: githubRepoFullName(store.editor.githubSettings.targetRepository),
      }
    },
    'TitleBar github',
  )

  const userPicture = useGetUserPicture()

  const hasUpstreamChanges = React.useMemo(
    () => getGithubFileChangesCount(upstreamChanges) > 0,
    [upstreamChanges],
  )

  const hasMergeConflicts = React.useMemo(() => {
    if (treeConflicts == null) {
      return false
    }
    return Object.keys(treeConflicts).length > 0
  }, [treeConflicts])

  const githubFileChanges = useGithubFileChanges()
  const hasDownstreamChanges = React.useMemo(
    () => getGithubFileChangesCount(githubFileChanges) > 0,
    [githubFileChanges],
  )

  const onClickLoginNewTab = useCallback(() => {
    window.open(auth0Url('auto-close'), '_blank')
  }, [])

  const toggleLeftPanel = useCallback(() => {
    let actions: Array<EditorAction> = []
    actions.push(togglePanel('leftmenu'))
    dispatch(actions)
  }, [dispatch])

  const openLeftPaneltoGithubTab = useCallback(() => {
    dispatch([setPanelVisibility('leftmenu', true), setLeftMenuTab(LeftMenuTab.Github)])
  }, [dispatch])

  const loggedIn = React.useMemo(() => loginState.type === 'LOGGED_IN', [loginState])

  const openFile = React.useCallback(
    (filename: string) => {
      dispatch([openCodeEditorFile(filename, true)], 'everyone')
    },
    [dispatch],
  )
  const showMergeConflict = React.useCallback(() => {
    if (Object.keys(treeConflicts).length < 1) {
      return
    }
    const firstConflictFilename = Object.keys(treeConflicts)[0]
    openLeftPaneltoGithubTab()
    openFile(firstConflictFilename)
  }, [openLeftPaneltoGithubTab, openFile, treeConflicts])

  return (
    <SimpleFlexRow
      style={{
        backgroundColor: colorTheme.bg0.value,
        borderBottom: `1px solid ${colorTheme.subduedBorder.value}`,
        padding: 0,
        flex: '0 0 40px',
        fontWeight: 600,
        letterSpacing: 0.2,
        justifyContent: 'space-between',
      }}
    >
      <SimpleFlexRow
        style={{
          display: 'flex',
          height: '100%',
          alignItems: 'center',
          flex: '1 1 0px',
          gap: 10,
          paddingLeft: 8,
        }}
      >
        <RoundButton onClick={toggleLeftPanel}>
          <img
            style={{
              userSelect: 'none',
              display: 'block',
            }}
            width={30}
            src={UNSAFE_getIconURL('utopia-logo', 'black', 'special', 60, 47)}
          />
        </RoundButton>
        <TestMenu />
        {when(
          loggedIn,
          <>
            {when(
              hasUpstreamChanges,
              <RoundButton
                color={colorTheme.secondaryOrange.value}
                onClick={openLeftPaneltoGithubTab}
              >
                {<Icons.Download style={{ width: 19, height: 19 }} color={'on-light-main'} />}
                <>Pull Remote</>
              </RoundButton>,
            )}
            {when(
              hasMergeConflicts,
              <RoundButton color={colorTheme.error.value} onClick={showMergeConflict}>
                {
                  <Icons.WarningTriangle
                    style={{ width: 19, height: 19 }}
                    color={'on-light-main'}
                  />
                }
                <>Merge Conflicts</>
              </RoundButton>,
            )}
            {when(
              hasDownstreamChanges,
              <RoundButton
                color={colorTheme.secondaryBlue.value}
                onClick={openLeftPaneltoGithubTab}
              >
                {<Icons.Upload style={{ width: 19, height: 19 }} color={'on-light-main'} />}
                <>Push Local</>
              </RoundButton>,
            )}
          </>,
        )}
      </SimpleFlexRow>
      <SimpleFlexRow
        style={{
          paddingLeft: 16,
          paddingRight: 16,
          borderRadius: 16,
          background: colorTheme.bg1.value,
          height: 27,
        }}
      >
        {currentBranch != null ? (
          <SimpleFlexRow style={{ gap: 5 }}>
            {repoName}
            {<Icons.Branch style={{ width: 19, height: 19 }} />}
            {currentBranch}
          </SimpleFlexRow>
        ) : (
          <ProjectTitle>{projectName}</ProjectTitle>
        )}
      </SimpleFlexRow>
      <div style={{ flexGrow: 1 }} />
      <div style={{ flex: '0 0 0px', paddingRight: 8 }}>
        {unless(
          loggedIn,
          <Button
            primary
            highlight
            style={{ paddingLeft: 8, paddingRight: 8 }}
            onClick={onClickLoginNewTab}
          >
            Sign In To Save
          </Button>,
        )}
        {when(
          loggedIn,
          <a href='/projects' target='_blank'>
            <Avatar userPicture={userPicture} isLoggedIn={loggedIn} />
          </a>,
        )}
      </div>
    </SimpleFlexRow>
  )
})

export default TitleBar

const loginStateSelector = createSelector(
  (store: EditorStoreShared) => store.userState.loginState,
  (loginState: LoginState) => getUserPicture(loginState),
)
function useGetUserPicture(): string | null {
  return useEditorState(Substores.restOfStore, loginStateSelector, 'useGetUserPicture')
}
