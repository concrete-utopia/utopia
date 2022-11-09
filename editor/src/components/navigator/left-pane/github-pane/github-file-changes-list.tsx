/** @jsxRuntime classic */
/** @jsx jsx */
/** @jsxFrag React.Fragment */
import { jsx } from '@emotion/react'
import React from 'react'
import { WarningIcon } from '../../../../uuiui/warning-icon'
import {
  getGithubFileChangesCount,
  GithubFileChanges,
  GithubFileChangesListItem,
  githubFileChangesToList,
} from '../../../../core/shared/github'
import { Button, FlexRow } from '../../../../uuiui'
import * as EditorActions from '../../../editor/actions/action-creators'
import { useEditorState } from '../../../editor/store/store-hook'
import { GithubFileStatusLetter } from '../../../filebrowser/fileitem'
import { UIGridRow } from '../../../inspector/widgets/ui-grid-row'
import { when } from '../../../../utils/react-conditionals'

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
        const conflicting = conflicts?.includes(i.filename) || false
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
                revertable,
                <RevertButton
                  disabled={githubWorking}
                  text='Revert'
                  onMouseUp={handleClickRevertFile(i)}
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
