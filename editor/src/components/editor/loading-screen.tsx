import React from 'react'
import { Substores, useEditorState } from './store/store-hook'
import { getImportOperationTextAsJsx } from './import-wizard/import-wizard-helpers'
import { getTotalImportStatusAndResult } from '../../core/shared/import/import-operation-service'
import type { TotalImportResult } from '../../core/shared/import/import-operation-types'
import type { Theme } from '../../uuiui'
import { useColorTheme } from '../../uuiui'
import { getCurrentTheme } from './store/editor-state'
import ReactDOM from 'react-dom'

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
          text: `Parsing files`,
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
    return {
      text: 'Loading Editor...',
      id: 'loading-editor',
      timeDone: null,
      timeStarted: null,
    }
  }, [totalImportResult, importState.importOperations, projectId])

  const shouldBeCleared = React.useMemo(() => {
    return (
      cleared.current ||
      (totalImportResult.importStatus.status == 'done' &&
        (githubRepo == null || totalImportResult.result == 'criticalError')) ||
      totalImportResult.importStatus.status == 'paused'
    )
  }, [totalImportResult, githubRepo])

  React.useEffect(() => {
    if (shouldBeCleared) {
      const loadingScreenWrapper = document.getElementById('loading-screen-wrapper')
      if (loadingScreenWrapper != null) {
        loadingScreenWrapper.remove()
      }
    }
  }, [shouldBeCleared])

  const portal = React.useRef(document.getElementById('loading-screen-progress-bar-portal')).current
  const hasMounted = React.useRef(false)
  if (portal == null) {
    return null
  }

  if (shouldBeCleared) {
    cleared.current = true
    return null
  }

  if (!hasMounted.current) {
    portal.innerHTML = ''
    hasMounted.current = true
  }

  const themeStyle =
    currentTheme === 'dark'
      ? `
    .editor-loading-screen { background-color: ${colorTheme.bg6.value} }
    .utopia-logo-pyramid.light { display: none; }
    .utopia-logo-pyramid.dark { display: block; }
  `
      : ''

  return ReactDOM.createPortal(
    <React.Fragment>
      <style>{themeStyle}</style>
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
    </React.Fragment>,
    portal,
  )
}
