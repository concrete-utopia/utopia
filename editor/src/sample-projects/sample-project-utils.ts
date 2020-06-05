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
import { getSampleComponentsFile, getUiBuilderUIJSFile } from './ui-builder-ui-js-file'

export const UI_BUILDER_PROJECT_ID = 'UI-BUILDER'

export function defaultProject(): PersistentModel {
  const selectedTab: EditorTab = releaseNotesTab()
  const openFiles: Array<EditorTab> = [openFileTab('/src/app.ui.js'), selectedTab]
  return {
    appID: null,
    projectVersion: CURRENT_PROJECT_VERSION,
    projectContents: {
      '/package.json': codeFile(JSON.stringify(DefaultPackageJson, null, 2), null),
      '/src': directory(),
      '/src/app.ui.js': getDefaultUIJsFile(),
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
    openFileTab('/src/app.ui.js'),
    openFileTab('/src/components.js'),
    selectedTab,
  ]
  return {
    appID: null,
    projectVersion: CURRENT_PROJECT_VERSION,
    projectContents: {
      '/package.json': codeFile(JSON.stringify(DefaultPackageJson, null, 2), null),
      '/src': directory(),
      '/src/app.ui.js': getUiBuilderUIJSFile(),
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
        name: 'UI Builder',
        model: uiBuilderProject(),
      }
    default:
      return null
  }
}
