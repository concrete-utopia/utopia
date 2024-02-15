import React from 'react'
import { useTriggerForkProject } from './persistence-hooks'
import { Substores, useEditorState } from './store/store-hook'

export const ForkSearchParamKey = 'fork'

export const ProjectForkFlow = React.memo(() => {
  const searchParams = new URLSearchParams(window.location.search)
  const shouldFork = searchParams.get(ForkSearchParamKey) === 'true'

  const projectId = useEditorState(
    Substores.restOfEditor,
    (store) => store.editor.id,
    'ProjectForkFlow projectId',
  )

  const triggerForkProject = useTriggerForkProject()

  React.useEffect(() => {
    if (shouldFork && projectId != null) {
      triggerForkProject()
    }
  }, [shouldFork, triggerForkProject, projectId])

  return null
})

ProjectForkFlow.displayName = 'ProjectForkFlow'
