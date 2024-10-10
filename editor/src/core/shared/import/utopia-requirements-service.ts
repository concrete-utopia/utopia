import type { ProjectContentTreeRoot } from 'utopia-shared/src/types'
import {
  importCheckUtopiaRequirementAndFix,
  type ImportOperationResult,
} from './import-operation-types'
import { notifyOperationFinished, notifyOperationStarted } from './import-operation-service'
import {
  addFileToProjectContents,
  packageJsonFileFromProjectContents,
} from '../../../components/assets'
import { codeFile, isTextFile, RevisionsState } from '../project-file-types'
import { applyToAllUIJSFiles } from '../../model/project-file-utils'

let utopiaRequirementsResolutions: Record<string, UtopiaRequirementResolution> = {
  storyboard: initialResolution(),
  packageJsonEntries: initialResolution(),
  language: initialResolution(),
  reactVersion: initialResolution(),
}

type UtopiaRequirement = keyof typeof utopiaRequirementsResolutions

const initialTexts: Record<UtopiaRequirement, string> = {
  storyboard: 'Checking storyboard.js',
  packageJsonEntries: 'Checking package.json',
  language: 'Checking project language',
  reactVersion: 'Checking React version',
}

function initialResolution(): UtopiaRequirementResolution {
  return {
    status: 'not-started',
  }
}

export function resetUtopiaRequirementsResolutions() {
  utopiaRequirementsResolutions = Object.fromEntries(
    Object.keys(utopiaRequirementsResolutions).map((key) => [key, initialResolution()]),
  ) as Record<UtopiaRequirement, UtopiaRequirementResolution>
  notifyOperationStarted({
    type: 'checkUtopiaRequirements',
    children: Object.keys(utopiaRequirementsResolutions).map((key) =>
      importCheckUtopiaRequirementAndFix(
        key as UtopiaRequirement,
        initialTexts[key as UtopiaRequirement],
      ),
    ),
  })
}

export function notifyCheckingUtopiaRequirement(requirement: UtopiaRequirement, text: string) {
  utopiaRequirementsResolutions[requirement].status = 'pending'
  notifyOperationStarted({
    type: 'checkUtopiaRequirementAndFix',
    id: requirement,
    text: text,
  })
}

export function notifyResolveUtopiaRequirement(
  requirementName: UtopiaRequirement,
  resolution: UtopiaRequirementResolutionResult,
  text: string,
  value?: string,
) {
  utopiaRequirementsResolutions[requirementName] = {
    status: 'done',
    resolution: resolution,
    value: value,
  }
  const result =
    resolution === 'found' || resolution === 'fixed'
      ? 'success'
      : resolution === 'partial'
      ? 'warn'
      : 'error'
  notifyOperationFinished(
    {
      type: 'checkUtopiaRequirementAndFix',
      id: requirementName,
      text: text,
      resolution: resolution,
    },
    result,
  )
  const aggregatedStatus = getAggregatedStatus()
  if (aggregatedStatus != 'pending') {
    notifyOperationFinished({ type: 'checkUtopiaRequirements' }, aggregatedStatus)
  }
}

function getAggregatedStatus(): ImportOperationResult | 'pending' {
  for (const resolution of Object.values(utopiaRequirementsResolutions)) {
    if (resolution.status != 'done') {
      return 'pending'
    }
    if (resolution.resolution == 'critical') {
      return 'error'
    }
    if (resolution.resolution == 'partial') {
      return 'warn'
    }
  }
  return 'success'
}

type UtopiaRequirementResolutionStatus = 'not-started' | 'pending' | 'done'
type UtopiaRequirementResolutionResult = 'found' | 'fixed' | 'partial' | 'critical'

type UtopiaRequirementResolution = {
  status: UtopiaRequirementResolutionStatus
  value?: string
  resolution?: UtopiaRequirementResolutionResult
}

export function checkAndFixUtopiaRequirements(
  parsedProjectContents: ProjectContentTreeRoot,
): ProjectContentTreeRoot {
  let projectContents = parsedProjectContents
  // check package.json
  projectContents = checkAndFixPackageJson(projectContents)
  // check language
  projectContents = checkProjectLanguage(projectContents)
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
  notifyCheckingUtopiaRequirement('packageJsonEntries', 'Checking package.json')
  const parsedPackageJson = getPackageJson(projectContents)
  if (parsedPackageJson == null) {
    notifyResolveUtopiaRequirement(
      'packageJsonEntries',
      'critical',
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
    notifyResolveUtopiaRequirement(
      'packageJsonEntries',
      'fixed',
      'Fixed utopia entry in package.json',
    )
    return result
  } else {
    notifyResolveUtopiaRequirement('packageJsonEntries', 'found', 'Valid package.json found')
  }

  return projectContents
}

function checkProjectLanguage(projectContents: ProjectContentTreeRoot): ProjectContentTreeRoot {
  notifyCheckingUtopiaRequirement('language', 'Checking project language')
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
  if (tsCount > jsCount) {
    notifyResolveUtopiaRequirement(
      'language',
      'critical',
      'Majority of project files are in TS/TSX',
      'typescript',
    )
  } else {
    notifyResolveUtopiaRequirement('language', 'found', 'Project uses JS/JSX', 'javascript')
  }
  return projectContents
}

function checkReactVersion(projectContents: ProjectContentTreeRoot): void {
  notifyCheckingUtopiaRequirement('reactVersion', 'Checking React version')
  const parsedPackageJson = getPackageJson(projectContents)
  if (
    parsedPackageJson == null ||
    parsedPackageJson.dependencies == null ||
    parsedPackageJson.dependencies.react == null
  ) {
    return notifyResolveUtopiaRequirement(
      'reactVersion',
      'critical',
      'React is not in dependencies',
    )
  }
  const reactVersion = parsedPackageJson.dependencies.react
  // TODO: check react version
  return notifyResolveUtopiaRequirement(
    'reactVersion',
    'found',
    'React version is ok',
    reactVersion,
  )
}
