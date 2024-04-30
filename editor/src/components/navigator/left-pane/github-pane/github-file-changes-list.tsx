/** @jsxRuntime classic */
/** @jsx jsx */
/** @jsxFrag React.Fragment */
import { jsx } from '@emotion/react'
import React from 'react'
import { WarningIcon } from '../../../../uuiui/warning-icon'
import type {
  Conflict,
  GithubFileChanges,
  GithubFileChangesListItem,
} from '../../../../core/shared/github/helpers'
import {
  getGithubFileChangesCount,
  githubFileChangesToList,
} from '../../../../core/shared/github/helpers'
import { Button, colorTheme, FlexColumn, FlexRow } from '../../../../uuiui'
import * as EditorActions from '../../../editor/actions/action-creators'
import { Substores, useEditorState } from '../../../editor/store/store-hook'
import { GithubFileStatusLetter } from '../../../filebrowser/fileitem'
import { when } from '../../../../utils/react-conditionals'
import { MenuProvider, MomentumContextMenu } from '../../../../components/context-menu-wrapper'
import { NO_OP } from '../../../../core/shared/utils'
import { useContextMenu } from 'react-contexify'
import { getConflictMenuItems } from '../../../../core/shared/github-ui'
import { UIGridRow } from '../../../../components/inspector/widgets/ui-grid-row'
import {
  isGithubCommitting,
  isGithubLoadingAnyBranch,
} from '../../../../components/editor/store/editor-state'
import { useDispatch } from '../../../editor/store/dispatch-context'

export const Ellipsis: React.FC<{
  children: any
  title?: string
  style?: React.CSSProperties
}> = ({ children, title, style }) => {
  return (
    <div
      style={{
        whiteSpace: 'nowrap',
        overflow: 'hidden',
        textOverflow: 'ellipsis',
        ...style,
      }}
      title={title}
    >
      {children}
    </div>
  )
}

const RevertButton = ({
  disabled,
  text,
  onMouseUp,
}: {
  disabled: boolean
  text?: string
  onMouseUp: (e: React.MouseEvent) => void
}) => {
  return (
    <Button
      style={{ padding: '0 6px', gap: 4 }}
      spotlight
      highlight
      disabled={disabled}
      onMouseUp={onMouseUp}
    >
      <RevertIcon />
      {text}
    </Button>
  )
}

const RevertIcon = () => {
  return (
    <svg width='9' height='7' viewBox='0 0 9 7' fill='none' xmlns='http://www.w3.org/2000/svg'>
      <path
        d='M7.93601 6.35693V2.72081H0.845703M0.845703 2.72081L2.90299 4.77809M0.845703 2.72081L2.90299 0.643075'
        stroke={colorTheme.fg1.value}
        strokeWidth='0.7'
      />
    </svg>
  )
}

interface ConflictButtonProps {
  fullPath: string
  conflict: Conflict
  disabled: boolean
}

const ConflictButton = React.memo((props: ConflictButtonProps) => {
  const menuId = `conflict-context-menu-${props.fullPath}`
  const dispatch = useDispatch()
  const githubRepo = useEditorState(
    Substores.github,
    (store) => {
      return store.editor.githubSettings.targetRepository
    },
    'ConflictButton githubRepo',
  )
  const projectID = useEditorState(
    Substores.restOfEditor,
    (store) => {
      return store.editor.id
    },
    'ConflictButton projectID',
  )
  const githubUserDetails = useEditorState(
    Substores.github,
    (store) => store.editor.githubData.githubUserDetails,
    'ConflictButton githubUserDetails',
  )

  const menuItems = React.useMemo(() => {
    if (githubRepo != null && projectID != null && githubUserDetails != null) {
      return getConflictMenuItems(
        githubRepo,
        projectID,
        dispatch,
        props.fullPath,
        props.conflict,
        undefined,
      )
    } else {
      return []
    }
  }, [props.fullPath, props.conflict, dispatch, githubRepo, projectID, githubUserDetails])
  const { show } = useContextMenu({
    id: menuId,
  })
  const openContextMenu = React.useCallback(
    (event: React.MouseEvent<HTMLDivElement>) => {
      event.preventDefault()
      show(event)
    },
    [show],
  )
  return (
    <MenuProvider id={menuId} itemsLength={menuItems.length}>
      <Button
        style={{ padding: '0 6px' }}
        spotlight
        highlight
        disabled={props.disabled}
        onClick={openContextMenu}
      >
        Action...
      </Button>
      <MomentumContextMenu id={menuId} items={menuItems} getData={NO_OP} />
    </MenuProvider>
  )
})

export const GithubFileChangesList: React.FC<{
  changes: GithubFileChanges | null
  revertable: boolean
  clickable: boolean
  showHeader: boolean
  conflicts?: string[]
}> = ({ changes, revertable, showHeader, conflicts, clickable }) => {
  const count = React.useMemo(() => getGithubFileChangesCount(changes), [changes])
  const dispatch = useDispatch()
  const list = React.useMemo(() => githubFileChangesToList(changes), [changes])
  const treeConflicts = useEditorState(
    Substores.github,
    (store) => store.editor.githubData.treeConflicts,
    'GithubFileChangesList treeConflicts',
  )

  const handleClickRevertAllFiles = React.useCallback(
    (e: React.MouseEvent) => {
      if (!revertable) {
        return
      }
      e.preventDefault()
      dispatch([EditorActions.showModal({ type: 'file-revert-all' })], 'everyone')
    },
    [dispatch, revertable],
  )

  const handleClickRevertFile = React.useCallback(
    (item: GithubFileChangesListItem) => (e: React.MouseEvent) => {
      if (!revertable) {
        return
      }
      e.preventDefault()
      dispatch(
        [
          EditorActions.showModal({
            type: 'file-revert',
            filePath: item.filename,
            status: item.status,
          }),
        ],
        'everyone',
      )
    },
    [dispatch, revertable],
  )

  const openFile = React.useCallback(
    (filename: string) => () => {
      dispatch([EditorActions.openCodeEditorFile(filename, true)], 'everyone')
    },
    [dispatch],
  )

  const githubOperations = useEditorState(
    Substores.github,
    (store) => store.editor.githubOperations,
    'Github operations',
  )

  const disableButtons = React.useMemo(() => {
    return isGithubLoadingAnyBranch(githubOperations) || isGithubCommitting(githubOperations)
  }, [githubOperations])

  if (count === 0) {
    return null
  }

  return (
    <FlexColumn style={{ gap: 10 }}>
      {when(
        showHeader,
        <Header
          count={count}
          revertable={revertable}
          disabled={disableButtons}
          onClickRevertAll={handleClickRevertAllFiles}
        />,
      )}
      <FlexColumn
        style={{
          border: `1px solid ${colorTheme.githubBoxesBorder.value}`,
          borderRadius: 2,
        }}
      >
        {list.map((i) => {
          const conflicting = conflicts?.includes(i.filename) ?? false
          const isTreeConflict = i.filename in treeConflicts
          return (
            <UIGridRow
              key={i.filename}
              padded={false}
              variant='<----------1fr---------><-auto->'
              title={conflicting ? 'Potential conflicts' : i.filename}
              css={{
                paddingRight: 6,
                color: conflicting ? colorTheme.errorForeground.value : 'inherit',
                cursor: conflicting ? 'help' : 'default',
                '&:hover': {
                  cursor: !conflicting && clickable ? 'pointer' : undefined,
                  background: clickable ? colorTheme.bg2.value : undefined,
                },
              }}
            >
              <UIGridRow
                padded
                variant='|--16px--|<--------auto-------->'
                onClick={clickable ? openFile(i.filename) : undefined}
              >
                <GithubFileStatusLetter status={i.status} />
                <FlexRow
                  style={{
                    gap: 2,
                    textDecoration: i.status === 'deleted' ? 'line-through' : 'none',
                  }}
                >
                  <Ellipsis>{i.filename}</Ellipsis>
                </FlexRow>
              </UIGridRow>
              {when(conflicting, <WarningIcon color='error' />)}
              {when(
                revertable && !isTreeConflict,
                <RevertButton disabled={disableButtons} onMouseUp={handleClickRevertFile(i)} />,
              )}
              {when(
                isTreeConflict,
                <ConflictButton
                  fullPath={i.filename}
                  conflict={treeConflicts[i.filename]}
                  disabled={disableButtons}
                />,
              )}
            </UIGridRow>
          )
        })}
      </FlexColumn>
    </FlexColumn>
  )
}

const Header: React.FC<{
  count: number
  revertable: boolean
  disabled: boolean
  onClickRevertAll: (e: React.MouseEvent) => void
}> = ({ count, revertable, disabled, onClickRevertAll }) => {
  return (
    <UIGridRow padded={false} variant='<----------1fr---------><-auto->' style={{ minHeight: 0 }}>
      <div>
        {count} file{count !== 1 ? 's' : ''} changed
      </div>
      {when(
        revertable,
        <RevertButton disabled={disabled} text='Revert all' onMouseUp={onClickRevertAll} />,
      )}
    </UIGridRow>
  )
}
