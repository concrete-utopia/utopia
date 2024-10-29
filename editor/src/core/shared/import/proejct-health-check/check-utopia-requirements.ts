import {
  addFileToProjectContents,
  packageJsonFileFromProjectContents,
} from '../../../../components/assets'
import type { ProjectContentTreeRoot } from 'utopia-shared/src/types'
import { codeFile, isTextFile, RevisionsState } from '../../project-file-types'
import { notifyCheckingRequirement, notifyResolveRequirement } from './utopia-requirements-service'
import { RequirementResolutionResult } from './utopia-requirements-types'
import { applyToAllUIJSFiles } from '../../../../core/model/project-file-utils'
import type { EditorDispatch } from '../../../../components/editor/action-types'

export function checkAndFixUtopiaRequirements(
  dispatch: EditorDispatch,
  parsedProjectContents: ProjectContentTreeRoot,
): ProjectContentTreeRoot {
  let projectContents = parsedProjectContents
  // check and fix package.json
  projectContents = checkAndFixPackageJson(dispatch, projectContents)
  // check language
  checkProjectLanguage(dispatch, projectContents)
  // check react version
  checkReactVersion(dispatch, projectContents)
  return projectContents
}

function getPackageJson(
  projectContents: ProjectContentTreeRoot,
): { utopia?: Record<string, string>; dependencies?: Record<string, string> } | null {
  const packageJson = packageJsonFileFromProjectContents(projectContents)
  if (packageJson != null && isTextFile(packageJson)) {
    return JSON.parse(packageJson.fileContents.code)
  }
  return null
}

function checkAndFixPackageJson(
  dispatch: EditorDispatch,
  projectContents: ProjectContentTreeRoot,
): ProjectContentTreeRoot {
  notifyCheckingRequirement(dispatch, 'packageJsonEntries', 'Checking package.json')
  const parsedPackageJson = getPackageJson(projectContents)
  if (parsedPackageJson == null) {
    notifyResolveRequirement(
      dispatch,
      'packageJsonEntries',
      RequirementResolutionResult.Critical,
      'The file package.json was not found',
    )
    return projectContents
  }
  if (parsedPackageJson.utopia == null) {
    parsedPackageJson.utopia = {
      'main-ui': 'utopia/storyboard.js',
    }
    const result = addFileToProjectContents(
      projectContents,
      '/package.json',
      codeFile(
        JSON.stringify(parsedPackageJson, null, 2),
        null,
        0,
        RevisionsState.CodeAheadButPleaseTellVSCodeAboutIt,
      ),
    )
    notifyResolveRequirement(
      dispatch,
      'packageJsonEntries',
      RequirementResolutionResult.Fixed,
      'Fixed utopia entry in package.json',
    )
    return result
  } else {
    notifyResolveRequirement(
      dispatch,
      'packageJsonEntries',
      RequirementResolutionResult.Found,
      'Valid package.json found',
    )
  }

  return projectContents
}

function checkProjectLanguage(
  dispatch: EditorDispatch,
  projectContents: ProjectContentTreeRoot,
): void {
  notifyCheckingRequirement(dispatch, 'language', 'Checking project language')
  let jsCount = 0
  let tsCount = 0
  applyToAllUIJSFiles(projectContents, (filename, uiJSFile) => {
    if ((filename.endsWith('.ts') || filename.endsWith('.tsx')) && !filename.endsWith('.d.ts')) {
      tsCount++
    } else if (filename.endsWith('.js') || filename.endsWith('.jsx')) {
      jsCount++
    }
    return uiJSFile
  })
  if (tsCount > 0) {
    notifyResolveRequirement(
      dispatch,
      'language',
      RequirementResolutionResult.Critical,
      'There are Typescript files in the project',
      'typescript',
    )
  } else if (jsCount == 0) {
    // in case it's a .coffee project, python, etc
    notifyResolveRequirement(
      dispatch,
      'language',
      RequirementResolutionResult.Critical,
      'No JS/JSX files found',
      'javascript',
    )
  } else {
    notifyResolveRequirement(
      dispatch,
      'language',
      RequirementResolutionResult.Found,
      'Project uses JS/JSX',
      'javascript',
    )
  }
}

function checkReactVersion(
  dispatch: EditorDispatch,
  projectContents: ProjectContentTreeRoot,
): void {
  notifyCheckingRequirement(dispatch, 'reactVersion', 'Checking React version')
  const parsedPackageJson = getPackageJson(projectContents)
  if (
    parsedPackageJson == null ||
    parsedPackageJson.dependencies == null ||
    parsedPackageJson.dependencies.react == null
  ) {
    return notifyResolveRequirement(
      dispatch,
      'reactVersion',
      RequirementResolutionResult.Critical,
      'React is not in dependencies',
    )
  }
  const reactVersion = parsedPackageJson.dependencies.react
  // TODO: check react version
  return notifyResolveRequirement(
    dispatch,
    'reactVersion',
    RequirementResolutionResult.Found,
    'React version is ok',
    reactVersion,
  )
}
