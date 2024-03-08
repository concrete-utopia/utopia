import * as React from 'react'
import { ProjectsPage } from '../components/projects/projects-page-component'
import '@radix-ui/themes/styles.css'
import { Theme } from '@radix-ui/themes'

export default function Projects() {
  return (
    <Theme>
      <ProjectsPage />
    </Theme>
  )
}
