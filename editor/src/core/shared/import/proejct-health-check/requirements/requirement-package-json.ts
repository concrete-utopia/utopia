import type { ProjectContentTreeRoot } from 'utopia-shared/src/types'
import { RevisionsState } from 'utopia-shared/src/types'
import { getPackageJson } from '../check-utopia-requirements'
import type { RequirementCheck, RequirementCheckResult } from '../utopia-requirements-types'
import { RequirementResolutionResult } from '../utopia-requirements-types'
import { addFileToProjectContents } from '../../../../../components/assets'
import { codeFile } from '../../../../../core/shared/project-file-types'

export default class PackageJsonCheckAndFix implements RequirementCheck {
  getStartText(): string {
    return 'Checking package.json'
  }
  check(projectContents: ProjectContentTreeRoot): RequirementCheckResult {
    const parsedPackageJson = getPackageJson(projectContents)
    if (parsedPackageJson == null) {
      return {
        resolution: RequirementResolutionResult.Critical,
        resultText: 'The file package.json was not found',
      }
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
      return {
        resolution: RequirementResolutionResult.Fixed,
        resultText: 'Added utopia entry to package.json',
        newProjectContents: result,
      }
    } else {
      return {
        resolution: RequirementResolutionResult.Found,
        resultText: 'Valid package.json found',
      }
    }
  }
}
