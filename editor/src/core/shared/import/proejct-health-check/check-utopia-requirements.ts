import {
  addFileToProjectContents,
  packageJsonFileFromProjectContents,
} from '../../../../components/assets'
import type { ProjectContentTreeRoot } from 'utopia-shared/src/types'
import { codeFile, isTextFile, RevisionsState } from '../../project-file-types'
import { notifyCheckingRequirement, notifyResolveRequirement } from './utopia-requirements-service'
import { RequirementResolutionResult } from './utopia-requirements-types'
import { applyToAllUIJSFiles } from '../../../../core/model/project-file-utils'

export function checkAndFixUtopiaRequirements(
  parsedProjectContents: ProjectContentTreeRoot,
): ProjectContentTreeRoot {
  let projectContents = parsedProjectContents
  // check and fix package.json
  projectContents = checkAndFixPackageJson(projectContents)
  // check language
  checkProjectLanguage(projectContents)
  // check react version
  checkReactVersion(projectContents)
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

function checkAndFixPackageJson(projectContents: ProjectContentTreeRoot): ProjectContentTreeRoot {
  notifyCheckingRequirement('packageJsonEntries', 'Checking package.json')
  const parsedPackageJson = getPackageJson(projectContents)
  if (parsedPackageJson == null) {
    notifyResolveRequirement(
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
      'packageJsonEntries',
      RequirementResolutionResult.Fixed,
      'Fixed utopia entry in package.json',
    )
    return result
  } else {
    notifyResolveRequirement(
      'packageJsonEntries',
      RequirementResolutionResult.Found,
      'Valid package.json found',
    )
  }

  return projectContents
}

function checkProjectLanguage(projectContents: ProjectContentTreeRoot): void {
  notifyCheckingRequirement('language', 'Checking project language')
  let jsCount = 0
  let tsCount = 0
  applyToAllUIJSFiles(projectContents, (filename, uiJSFile) => {
    if (filename.endsWith('.ts') || filename.endsWith('.tsx')) {
      tsCount++
    } else if (filename.endsWith('.js') || filename.endsWith('.jsx')) {
      jsCount++
    }
    return uiJSFile
  })
  if (tsCount > 0) {
    notifyResolveRequirement(
      'language',
      RequirementResolutionResult.Critical,
      'Majority of project files are in TS/TSX',
      'typescript',
    )
  } else if (jsCount == 0) {
    // in case it's a .coffee project, python, etc
    notifyResolveRequirement(
      'language',
      RequirementResolutionResult.Critical,
      'No JS/JSX files found',
      'javascript',
    )
  } else {
    notifyResolveRequirement(
      'language',
      RequirementResolutionResult.Found,
      'Project uses JS/JSX',
      'javascript',
    )
  }
}

function checkReactVersion(projectContents: ProjectContentTreeRoot): void {
  notifyCheckingRequirement('reactVersion', 'Checking React version')
  const parsedPackageJson = getPackageJson(projectContents)
  if (
    parsedPackageJson == null ||
    parsedPackageJson.dependencies == null ||
    parsedPackageJson.dependencies.react == null
  ) {
    return notifyResolveRequirement(
      'reactVersion',
      RequirementResolutionResult.Critical,
      'React is not in dependencies',
    )
  }
  const reactVersion = parsedPackageJson.dependencies.react
  // TODO: check react version
  return notifyResolveRequirement(
    'reactVersion',
    RequirementResolutionResult.Found,
    'React version is ok',
    reactVersion,
  )
}
