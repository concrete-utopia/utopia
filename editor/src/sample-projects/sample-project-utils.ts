import {
  DefaultPackageJson,
  openFileTab,
  PersistentModel,
  persistentModelForProjectContents,
} from '../components/editor/store/editor-state'
import {
  getDefaultUIJsFile,
  getSamplePreviewFile,
  getSamplePreviewHTMLFile,
} from '../core/model/new-project-files'
import { codeFile, directory } from '../core/model/project-file-utils'
import { ProjectContents } from '../core/shared/project-file-types'
import { getSampleComponentsFile, getUiBuilderUIJSFile } from './ui-builder-ui-js-file'

export const UI_BUILDER_PROJECT_ID = 'UI-BUILDER'

let generatedFiles: ProjectContents = {}
for (let index = 0; index < 2000; index++) {
  const path = `/src/code${index}.js`
  const code = `/** @jsx jsx */
import * as React from 'react'
import { Scene, Storyboard, jsx } from 'utopia-api'
console.log('${path} executed', new Error())
export var Component${index} = (props) => {
  return (
    <div
      style={{ width: '100%', height: '100%', backgroundColor: '#FFFFFF' }}
      layout={{ layoutSystem: 'pinSystem' }}
    />
  )
}`
  generatedFiles[path] = codeFile(code, null)
}

export function defaultProject(): PersistentModel {
  const projectContents: ProjectContents = {
    '/package.json': codeFile(JSON.stringify(DefaultPackageJson, null, 2), null),
    '/src': directory(),
    '/src/app.js': getDefaultUIJsFile(),
    '/assets': directory(),
    '/public': directory(),
    '/src/index.js': getSamplePreviewFile(),
    '/public/index.html': getSamplePreviewHTMLFile(),
  }

  let persistentModel = persistentModelForProjectContents(projectContents)
  persistentModel.openFiles = [openFileTab('/src/app.js'), ...persistentModel.openFiles]
  return persistentModel
}

function uiBuilderProject(): PersistentModel {
  const projectContents: ProjectContents = {
    '/package.json': codeFile(JSON.stringify(DefaultPackageJson, null, 2), null),
    '/src': directory(),
    '/src/app.js': getUiBuilderUIJSFile(),
    '/src/components.js': getSampleComponentsFile(),
    '/assets': directory(),
    '/public': directory(),
    '/src/index.js': getSamplePreviewFile(),
    '/public/index.html': getSamplePreviewHTMLFile(),
  }

  let persistentModel = persistentModelForProjectContents(projectContents)
  persistentModel.openFiles = [
    openFileTab('/src/app.js'),
    openFileTab('/src/components.js'),
    ...persistentModel.openFiles,
  ]

  return persistentModel
}

export interface SampleProject {
  name: string
  model: PersistentModel
}

export function isSampleProject(projectID: string): boolean {
  return sampleProjectForId(projectID) != null
}

export function sampleProjectForId(projectID: string): SampleProject | null {
  switch (projectID) {
    case UI_BUILDER_PROJECT_ID:
      return {
        name: projectID,
        model: uiBuilderProject(),
      }
    default:
      return null
  }
}
