import { DefaultTheme } from '../components/code-editor/code-editor-themes'
import { CURRENT_PROJECT_VERSION } from '../components/editor/actions/migrations/migrations'
import {
  DefaultPackageJson,
  EditorTab,
  openFileTab,
  PersistentModel,
  releaseNotesTab,
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
  const selectedTab: EditorTab = releaseNotesTab()
  const openFiles: Array<EditorTab> = [openFileTab('/src/app.js'), selectedTab]
  return {
    appID: null,
    projectVersion: CURRENT_PROJECT_VERSION,
    projectContents: {
      ...generatedFiles,
      '/package.json': codeFile(JSON.stringify(DefaultPackageJson, null, 2), null),
      '/src': directory(),
      '/src/app.js': getDefaultUIJsFile(),
      '/assets': directory(),
      '/public': directory(),
      '/src/index.js': getSamplePreviewFile(),
      '/public/index.html': getSamplePreviewHTMLFile(),
    },
    exportsInfo: [],
    buildResult: {},
    openFiles: openFiles,
    selectedFile: selectedTab,
    codeEditorErrors: {
      buildErrors: {},
      lintErrors: {},
    },
    codeEditorTheme: DefaultTheme,
    lastUsedFont: null,
    hiddenInstances: [],
    fileBrowser: {
      minimised: false,
    },
    dependencyList: {
      minimised: false,
    },
    projectSettings: {
      minimised: false,
    },
    navigator: {
      minimised: false,
    },
  }
}

function uiBuilderProject(): PersistentModel {
  const selectedTab: EditorTab = releaseNotesTab()
  const openFiles: Array<EditorTab> = [
    openFileTab('/src/app.js'),
    openFileTab('/src/components.js'),
    selectedTab,
  ]
  return {
    appID: null,
    projectVersion: CURRENT_PROJECT_VERSION,
    projectContents: {
      '/package.json': codeFile(JSON.stringify(DefaultPackageJson, null, 2), null),
      '/src': directory(),
      '/src/app.js': getUiBuilderUIJSFile(),
      '/src/components.js': getSampleComponentsFile(),
      '/assets': directory(),
      '/public': directory(),
      '/src/index.js': getSamplePreviewFile(),
      '/public/index.html': getSamplePreviewHTMLFile(),
    },
    exportsInfo: [],
    buildResult: {},
    openFiles: openFiles,
    selectedFile: selectedTab,
    codeEditorErrors: {
      buildErrors: {},
      lintErrors: {},
    },
    codeEditorTheme: DefaultTheme,
    lastUsedFont: null,
    hiddenInstances: [],
    fileBrowser: {
      minimised: false,
    },
    dependencyList: {
      minimised: false,
    },
    projectSettings: {
      minimised: false,
    },
    navigator: {
      minimised: false,
    },
  }
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
