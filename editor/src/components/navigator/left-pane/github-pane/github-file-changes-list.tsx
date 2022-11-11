/** @jsxRuntime classic */
/** @jsx jsx */
/** @jsxFrag React.Fragment */
import { jsx } from '@emotion/react'
import React from 'react'
import { WarningIcon } from '../../../../uuiui/warning-icon'
import {
  Conflict,
  getGithubFileChangesCount,
  GithubFileChanges,
  GithubFileChangesListItem,
  githubFileChangesToList,
  resolveConflict,
} from '../../../../core/shared/github'
import { Button, FlexRow } from '../../../../uuiui'
import * as EditorActions from '../../../editor/actions/action-creators'
import { useEditorState } from '../../../editor/store/store-hook'
import { GithubFileStatusLetter } from '../../../filebrowser/fileitem'
import { UIGridRow } from '../../../inspector/widgets/ui-grid-row'
import { when } from '../../../../utils/react-conditionals'
import { ContextMenuItem } from '../../../../components/context-menu-items'
import { MenuProvider, MomentumContextMenu } from '../../../../components/context-menu-wrapper'
import { EditorDispatch } from '../../../../components/editor/action-types'
import { NO_OP } from '../../../../core/shared/utils'
import { GithubRepo } from '../../../../components/editor/store/editor-state'
import { contextMenu, useContextMenu } from 'react-contexify'

export const Ellipsis: React.FC<{
  children: any
  title?: string
  style?: React.CSSProperties
}> = ({ children, title, style }) => {
  return (
    <div
      style={{
        ...style,
        whiteSpace: 'nowrap',
        overflow: 'hidden',
        textOverflow: 'ellipsis',
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
  text: string
  onMouseUp: (e: React.MouseEvent) => void
}) => {
  return (
    <Button
      style={{ padding: '0 6px' }}
      spotlight
      highlight
      disabled={disabled}
      onMouseUp={onMouseUp}
    >
      {text}
    </Button>
  )
}

interface ConflictButtonProps {
  fullPath: string
  conflict: Conflict
  disabled: boolean
}

function getConflictMenuItems(
  githubRepo: GithubRepo,
  projectID: string,
  dispatch: EditorDispatch,
  path: string,
  conflict: Conflict,
): Array<ContextMenuItem<unknown>> {
  function applyChange(whichChange: 'utopia' | 'branch'): void {
    void resolveConflict(githubRepo, projectID, path, conflict, whichChange, dispatch)
  }
  switch (conflict.type) {
    case 'DIFFERING_TYPES':
      return [
        {
          name: 'Accept what is in Utopia.',
          enabled: true,
          action: () => {
            applyChange('utopia')
          },
        },
        {
          name: 'Apply what is in Github.',
          enabled: true,
          action: () => {
            applyChange('branch')
          },
        },
      ]
    case 'CURRENT_DELETED_BRANCH_CHANGED':
      return [
        {
          name: 'Delete the file.',
          enabled: true,
          action: () => {
            applyChange('utopia')
          },
        },
        {
          name: 'Restore the file from Github.',
          enabled: true,
          action: () => {
            applyChange('branch')
          },
        },
      ]

    case 'CURRENT_CHANGED_BRANCH_DELETED':
      return [
        {
          name: 'Keep the file in Utopia.',
          enabled: true,
          action: () => {
            applyChange('utopia')
          },
        },
        {
          name: 'Delete the file.',
          enabled: true,
          action: () => {
            applyChange('branch')
          },
        },
      ]

    default:
      const _exhaustiveCheck: never = conflict
      throw new Error(`Unhandled conflict type ${JSON.stringify(conflict)}`)
  }
}

const ConflictButton = React.memo((props: ConflictButtonProps) => {
  const menuId = `conflict-context-menu-${props.fullPath}`
  const dispatch = useEditorState((store) => {
    return store.dispatch
  }, 'ConflictButton dispatch')
  const githubRepo = useEditorState((store) => {
    return store.editor.githubSettings.targetRepository
  }, 'ConflictButton githubRepo')
  const projectID = useEditorState((store) => {
    return store.editor.id
  }, 'ConflictButton projectID')
  const menuItems = React.useMemo(() => {
    if (githubRepo != null && projectID != null) {
      return getConflictMenuItems(githubRepo, projectID, dispatch, props.fullPath, props.conflict)
    } else {
      return []
    }
  }, [props.fullPath, props.conflict, dispatch, githubRepo, projectID])
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
  githubWorking: boolean
  revertable: boolean
  showHeader: boolean
  conflicts?: string[]
}> = ({ changes, githubWorking, revertable, showHeader, conflicts }) => {
  const count = React.useMemo(() => getGithubFileChangesCount(changes), [changes])
  const dispatch = useEditorState((store) => store.dispatch, 'dispatch')
  const list = React.useMemo(() => githubFileChangesToList(changes), [changes])
  const treeConflicts = useEditorState(
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

  if (count === 0) {
    return null
  }

  return (
    <>
      {showHeader && (
        <Header
          count={count}
          revertable={revertable}
          githubWorking={githubWorking}
          onClickRevertAll={handleClickRevertAllFiles}
        />
      )}
      {list.map((i) => {
        const conflicting = conflicts?.includes(i.filename) ?? false
        const isTreeConflict = i.filename in treeConflicts
        return (
          <UIGridRow
            key={i.filename}
            padded
            variant='<----------1fr---------><-auto->'
            title={conflicting ? 'Potential conflicts' : i.filename}
            style={{
              gap: 2,
              color: conflicting ? '#f00' : 'inherit',
              cursor: conflicting ? 'help' : 'default',
            }}
          >
            <>
              <UIGridRow padded variant='|--16px--|<--------auto-------->'>
                <GithubFileStatusLetter status={i.status} />
                <FlexRow style={{ gap: 2 }}>
                  <>
                    <Ellipsis>{i.filename}</Ellipsis>
                    {when(conflicting, <WarningIcon color='error' />)}
                  </>
                </FlexRow>
              </UIGridRow>
              {when(
                revertable && !isTreeConflict,
                <RevertButton
                  disabled={githubWorking}
                  text='Revert'
                  onMouseUp={handleClickRevertFile(i)}
                />,
              )}
              {when(
                isTreeConflict,
                <ConflictButton
                  fullPath={i.filename}
                  conflict={treeConflicts[i.filename]}
                  disabled={githubWorking}
                />,
              )}
            </>
          </UIGridRow>
        )
      })}
    </>
  )
}

const Header: React.FC<{
  count: number
  revertable: boolean
  githubWorking: boolean
  onClickRevertAll: (e: React.MouseEvent) => void
}> = ({ count, revertable, githubWorking, onClickRevertAll }) => {
  return (
    <UIGridRow padded variant='<----------1fr---------><-auto->'>
      <>
        <div>
          {count} file{count !== 1 ? 's' : ''} changed
        </div>
        {when(
          revertable,
          <RevertButton disabled={githubWorking} text='Revert all' onMouseUp={onClickRevertAll} />,
        )}
      </>
    </UIGridRow>
  )
}
