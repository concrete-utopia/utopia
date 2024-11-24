import React from 'react'
import { Substores, useEditorState } from './store/store-hook'
import { getImportOperationTextAsJsx } from './import-wizard/import-wizard-helpers'
import { getTotalImportStatusAndResult } from '../../core/shared/import/import-operation-service'
import type { TotalImportResult } from '../../core/shared/import/import-operation-types'
import type { Theme } from '../../uuiui'
import { useColorTheme } from '../../uuiui'
import { getCurrentTheme } from './store/editor-state'

export function LoadingEditorComponent() {
  const colorTheme = useColorTheme()

  const currentTheme: Theme = useEditorState(
    Substores.theme,
    (store) => getCurrentTheme(store.userState),
    'currentTheme',
  )

  const importState = useEditorState(
    Substores.restOfEditor,
    (store) => store.editor.importState,
    'LoadingEditorComponent importState',
  )

  const githubRepo = useEditorState(
    Substores.userState,
    (store) => store.userState.githubState.gitRepoToLoad,
    'LoadingEditorComponent githubRepoToLoad',
  )

  const totalImportResult: TotalImportResult = React.useMemo(
    () => getTotalImportStatusAndResult(importState),
    [importState],
  )

  const projectId = useEditorState(
    Substores.restOfEditor,
    (store) => store.editor.id,
    'LoadingEditorComponent projectId',
  )

  const cleared = React.useRef(false)

  const [, setTime] = React.useState(Date.now())
  React.useEffect(() => {
    const interval = setInterval(() => {
      setTime(Date.now())
    }, 50)
    return () => clearInterval(interval)
  }, [])

  const currentOperationToShow: {
    text: React.ReactNode
    id: string
    timeDone: number | null | undefined
    timeStarted: number | null | undefined
  } | null = React.useMemo(() => {
    if (totalImportResult.importStatus.status == 'not-started') {
      if (projectId == null) {
        return {
          text: 'Loading Editor...',
          id: 'loading-editor',
          timeDone: null,
          timeStarted: null,
        }
      } else {
        return {
          text: `Parsing files...`,
          id: 'parseFiles',
          timeDone: null,
          timeStarted: null,
        }
      }
    }
    for (const op of importState.importOperations) {
      if (op?.children?.length == 0 || op.type == 'refreshDependencies') {
        if (op.timeStarted != null && op.timeDone == null) {
          return {
            text: getImportOperationTextAsJsx(op),
            id: op.id ?? op.type,
            timeDone: op.timeDone,
            timeStarted: op.timeStarted,
          }
        }
      }
      if (op.type !== 'refreshDependencies') {
        for (const child of op.children ?? []) {
          if (child.timeStarted != null && child.timeDone == null) {
            return {
              text: getImportOperationTextAsJsx(child),
              id: child.id ?? child.type,
              timeDone: child.timeDone,
              timeStarted: child.timeStarted,
            }
          }
        }
      }
    }
    return null
  }, [totalImportResult, importState.importOperations, projectId])
  if (
    cleared.current ||
    (totalImportResult.importStatus.status == 'done' && githubRepo == null) ||
    totalImportResult.importStatus.status == 'paused'
  ) {
    cleared.current = true
    return null
  }

  return (
    <div
      id='utopia-editor-root-loading'
      className='editor-loading-screen'
      style={{
        zIndex: 1000,
        backgroundColor: colorTheme.bg6.value,
        color: colorTheme.fg0.value,
      }}
    >
      <img
        src={currentTheme === 'dark' ? '/editor/pyramid_dark.png' : '/editor/pyramid_light.png'}
        height='78px'
        alt='Utopia Logo'
        className='utopia-logo-pyramid'
      />

      <div className='progress-bar-shell' style={{ borderColor: colorTheme.fg0.value }}>
        <div
          className='progress-bar-progress animation-progress'
          style={{
            transform: 'translateX(-180px)',
            animationName: 'animation-keyframes-2',
            backgroundColor: colorTheme.fg0.value,
          }}
        ></div>
      </div>
      <div>
        <ul className='loading-screen-import-operations'>
          {currentOperationToShow != null ? (
            <li
              style={{
                listStyle: 'none',
                color: colorTheme.fg0.value,
              }}
              key={currentOperationToShow.id}
            >
              {currentOperationToShow.text}
            </li>
          ) : null}
        </ul>
      </div>
    </div>
  )
}
