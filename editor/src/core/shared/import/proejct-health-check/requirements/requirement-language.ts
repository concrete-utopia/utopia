import type { ProjectContentTreeRoot } from 'utopia-shared/src/types'
import {
  RequirementResolutionResult,
  type RequirementCheck,
  type RequirementCheckResult,
} from '../utopia-requirements-types'
import { applyToAllUIJSFiles } from '../../../../model/project-file-utils'

export default class CheckProjectLanguage implements RequirementCheck {
  getStartText(): string {
    return 'Checking project language'
  }
  check(projectContents: ProjectContentTreeRoot): RequirementCheckResult {
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
      return {
        resolution: RequirementResolutionResult.Critical,
        resultText: 'There are Typescript files in the project',
        resultValue: 'typescript',
      }
    } else if (jsCount == 0) {
      // in case it's a .coffee project, python, etc
      return {
        resolution: RequirementResolutionResult.Critical,
        resultText: 'No JS/JSX files found',
      }
    } else {
      return {
        resolution: RequirementResolutionResult.Found,
        resultText: 'Project uses JS/JSX',
        resultValue: 'javascript',
      }
    }
  }
}
