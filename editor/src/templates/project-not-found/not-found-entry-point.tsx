import React from 'react'
import { createRoot } from 'react-dom/client'
import ProjectNotFound from './ProjectNotFound'
import { EditorID } from '../../core/shared/utils'

const rootElement = document.getElementById(EditorID)
if (rootElement != null) {
  const root = createRoot(rootElement)
  root.render(<ProjectNotFound projectId={null} loggedIn={false} />)
}
