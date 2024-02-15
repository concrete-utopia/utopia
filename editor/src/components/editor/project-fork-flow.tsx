import React from 'react'
import { useTriggerForkProject } from './persistence-hooks'
import { Substores, useEditorState } from './store/store-hook'

export const ForkSearchParamKey = 'fork'

export const ProjectForkFlow = React.memo(() => {
  const searchParams = new URLSearchParams(window.location.search)
  const shouldFork = searchParams.get(ForkSearchParamKey) === 'true'

  const projectName = useEditorState(
    Substores.restOfEditor,
    (store) => store.editor.projectName,
    'ProjectForkFlow projectName',
  )

  const triggerForkProject = useTriggerForkProject()

  React.useEffect(() => {
    if (shouldFork && projectName != null) {
      triggerForkProject()
    }
  }, [shouldFork, triggerForkProject, projectName])

  return null
})

ProjectForkFlow.displayName = 'ProjectForkFlow'
