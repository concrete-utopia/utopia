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
import { Button, FlexColumn, FlexRow } from '../../../../uuiui'
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
        stroke='#2D2E33'
        strokeWidth='0.7'
      />
    </svg>
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
    <FlexColumn style={{ gap: 4 }}>
      {showHeader && (
        <Header
          count={count}
          revertable={revertable}
          githubWorking={githubWorking}
          onClickRevertAll={handleClickRevertAllFiles}
        />
      )}
      <FlexColumn style={{ border: '1px solid #2D2E33', borderRadius: 2 }}>
        {list.map((i) => {
          const conflicting = conflicts?.includes(i.filename) || false
          return (
            <FlexRow
              key={i.filename}
              title={conflicting ? 'Potential conflicts' : i.filename}
              style={{
                gap: 2,
                padding: '4px 8px',
                color: conflicting ? '#f00' : 'inherit',
                cursor: conflicting ? 'help' : 'default',
              }}
            >
              <FlexRow style={{ flex: 1, gap: 2 }}>
                <GithubFileStatusLetter status={i.status} />
                <FlexRow style={{ gap: 2 }}>
                  <>
                    <Ellipsis>{i.filename}</Ellipsis>
                    {when(conflicting, <WarningIcon color='error' />)}
                  </>
                </FlexRow>
              </FlexRow>
              {when(
                revertable,
                <RevertButton disabled={githubWorking} onMouseUp={handleClickRevertFile(i)} />,
              )}
            </FlexRow>
          )
        })}
      </FlexColumn>
    </FlexColumn>
  )
}

const Header: React.FC<{
  count: number
  revertable: boolean
  githubWorking: boolean
  onClickRevertAll: (e: React.MouseEvent) => void
}> = ({ count, revertable, githubWorking, onClickRevertAll }) => {
  return (
    <FlexColumn>
      <FlexRow>
        <div style={{ flex: 1 }}>
          {count} file{count !== 1 ? 's' : ''} changed
        </div>
        {when(
          revertable,
          <RevertButton disabled={githubWorking} text='Revert all' onMouseUp={onClickRevertAll} />,
        )}
      </FlexRow>
    </FlexColumn>
  )
}
