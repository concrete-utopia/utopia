/** @jsxRuntime classic */
/** @jsx jsx */
/** @jsxFrag React.Fragment */
import { jsx } from '@emotion/react'
import React from 'react'
import {
  getGithubFileChangesCount,
  GithubFileChanges,
  GithubFileChangesListItem,
  githubFileChangesToList,
} from '../../../../core/shared/github'
import { Button } from '../../../../uuiui'
import * as EditorActions from '../../../editor/actions/action-creators'
import { useEditorState } from '../../../editor/store/store-hook'
import { GithubFileStatusLetter } from '../../../filebrowser/fileitem'
import { UIGridRow } from '../../../inspector/widgets/ui-grid-row'

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
}> = ({ changes, githubWorking }) => {
  const count = React.useMemo(() => getGithubFileChangesCount(changes), [changes])
  const dispatch = useEditorState((store) => store.dispatch, 'dispatch')
  const list = React.useMemo(() => githubFileChangesToList(changes), [changes])

  const handleClickRevertAllFiles = React.useCallback(
    (e: React.MouseEvent) => {
      e.preventDefault()
      dispatch([EditorActions.showModal({ type: 'file-revert-all' })], 'everyone')
    },
    [dispatch],
  )

  const handleClickRevertFile = React.useCallback(
    (item: GithubFileChangesListItem) => (e: React.MouseEvent) => {
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
    [dispatch],
  )

  if (count === 0) {
    return null
  }

  return (
    <>
      <UIGridRow padded variant='<----------1fr---------><-auto->'>
        <div>
          {count} file{count !== 1 ? 's' : ''} changed
        </div>
        <RevertButton
          disabled={githubWorking}
          text='Revert all'
          onMouseUp={handleClickRevertAllFiles}
        />
      </UIGridRow>
      {list.map((i) => (
        <UIGridRow key={i.filename} padded variant='<----------1fr---------><-auto->'>
          <UIGridRow padded variant='|--16px--|<--------auto-------->'>
            <GithubFileStatusLetter status={i.status} />
            <Ellipsis title={i.filename}>{i.filename}</Ellipsis>
          </UIGridRow>
          <RevertButton
            disabled={githubWorking}
            text='Revert'
            onMouseUp={handleClickRevertFile(i)}
          />
        </UIGridRow>
      ))}
    </>
  )
}
