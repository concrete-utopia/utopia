import { simpleDefaultProject } from '../../../../../sample-projects/sample-project-utils'
import { RequirementResolutionResult } from '../utopia-requirements-types'
import CheckProjectLanguage from './requirement-language'
import CheckPackageJson from './requirement-package-json'
import CheckReactVersion from './requirement-react'
import { codeFile, textFile, textFileContents } from '../../../../../core/shared/project-file-types'
import { RevisionsState } from '../../../../../core/shared/project-file-types'
import { unparsed } from '../../../../../core/shared/project-file-types'
import { contentsToTree } from '../../../../../components/assets'
import { DefaultPackageJson } from '../../../../../components/editor/store/editor-state'
import { getPackageJson } from '../../../../../components/assets'
import CheckStoryboard from './requirement-storyboard'
import CheckServerPackages from './requirement-server-packages'
import { parseProjectContents } from '../../../../../sample-projects/sample-project-utils.test-utils'

describe('requirements checks', () => {
  describe('project language', () => {
    it('should return success for a project with only js files', () => {
      const check = new CheckProjectLanguage()
      const project = simpleDefaultProject()
      const projectContents = project.projectContents
      const result = check.check(projectContents)
      expect(result.resolution).toBe(RequirementResolutionResult.Passed)
    })

    it('should return failure for a project with ts files', () => {
      const check = new CheckProjectLanguage()
      const project = simpleDefaultProject({
        additionalFiles: {
          '/src/app.ts': codeFile(``, null),
        },
      })
      const projectContents = project.projectContents
      const result = check.check(projectContents)
      expect(result.resolution).toBe(RequirementResolutionResult.Critical)
    })
    it('should not fail for a project with .d.ts files', () => {
      const check = new CheckProjectLanguage()
      const project = simpleDefaultProject({
        additionalFiles: {
          '/src/app.d.ts': codeFile(``, null),
        },
      })
      const projectContents = project.projectContents
      const result = check.check(projectContents)
      expect(result.resolution).toBe(RequirementResolutionResult.Passed)
    })
    it('should fail for a project with no js files', () => {
      const check = new CheckProjectLanguage()
      const projectContents = contentsToTree({
        '/src/app.python': codeFile(``, null),
      })
      const result = check.check(projectContents)
      expect(result.resolution).toBe(RequirementResolutionResult.Critical)
    })
  })

  describe('package json', () => {
    it('should return success for a project with a package.json and a utopia entry', () => {
      const check = new CheckPackageJson()
      const project = simpleDefaultProject()
      const projectContents = project.projectContents
      const result = check.check(projectContents)
      expect(result.resolution).toBe(RequirementResolutionResult.Passed)
    })
    it('should return failure for a project without a package.json', () => {
      const check = new CheckPackageJson()
      const project = simpleDefaultProject()
      const projectContents = project.projectContents
      delete projectContents['package.json']
      const result = check.check(projectContents)
      expect(result.resolution).toBe(RequirementResolutionResult.Critical)
    })

    it('should add a utopia entry if there is no utopia entry', () => {
      const { utopia, ...packageJson } = DefaultPackageJson
      const check = new CheckPackageJson()
      const project = simpleDefaultProject({
        additionalFiles: {
          '/package.json': textFile(
            textFileContents(
              JSON.stringify(packageJson, null, 2),
              unparsed,
              RevisionsState.CodeAhead,
            ),
            null,
            null,
            0,
          ),
        },
      })
      const projectContents = project.projectContents
      const result = check.check(projectContents)
      expect(result.resolution).toBe(RequirementResolutionResult.Fixed)
      expect(result.newProjectContents).not.toBeNull()
      const newPackageJson = getPackageJson(result.newProjectContents!)
      expect(newPackageJson).not.toBeNull()
      expect(newPackageJson!.utopia?.['main-ui']).not.toBeUndefined()
    })
  })

  describe('react version', () => {
    it('should return success for a project with a react version 16.8 - 18.x', () => {
      const check = new CheckReactVersion()
      const project = simpleDefaultProject()
      const projectContents = project.projectContents
      const result = check.check(projectContents)
      expect(result.resolution).toBe(RequirementResolutionResult.Passed)
    })
    it('should return failure for a project with a react version less than 16.8', () => {
      const check = new CheckReactVersion()
      const project = simpleDefaultProject({
        additionalFiles: {
          '/package.json': textFile(
            textFileContents(
              JSON.stringify(
                {
                  ...DefaultPackageJson,
                  dependencies: {
                    ...DefaultPackageJson.dependencies,
                    react: '16.7.0',
                  },
                },
                null,
                2,
              ),
              unparsed,
              RevisionsState.CodeAhead,
            ),
            null,
            null,
            0,
          ),
        },
      })
      const projectContents = project.projectContents
      const result = check.check(projectContents)
      expect(result.resolution).toBe(RequirementResolutionResult.Critical)
    })
    it('should return a failure for react version 19', () => {
      const check = new CheckReactVersion()
      const project = simpleDefaultProject({
        additionalFiles: {
          '/package.json': textFile(
            textFileContents(
              JSON.stringify(
                {
                  ...DefaultPackageJson,
                  dependencies: {
                    ...DefaultPackageJson.dependencies,
                    react: '19.0.0',
                  },
                },
                null,
                2,
              ),
              unparsed,
              RevisionsState.CodeAhead,
            ),
            null,
            null,
            0,
          ),
        },
      })
      const projectContents = project.projectContents
      const result = check.check(projectContents)
      expect(result.resolution).toBe(RequirementResolutionResult.Critical)
    })
  })

  describe('storyboard', () => {
    it('should return success for a project with a storyboard', () => {
      const check = new CheckStoryboard()
      const project = simpleDefaultProject()
      const projectContents = project.projectContents
      const result = check.check(projectContents)
      expect(result.resolution).toBe(RequirementResolutionResult.Passed)
    })
    it('should create a storyboard if there is no storyboard', () => {
      const check = new CheckStoryboard()
      const project = simpleDefaultProject({
        storyboardFile: null,
      })
      const projectContents = project.projectContents
      delete projectContents['utopia/storyboard.js']
      const result = check.check(projectContents)
      expect(result.resolution).toBe(RequirementResolutionResult.Fixed)
      expect(result.newProjectContents).not.toBeNull()
      expect(result.newProjectContents!['utopia/storyboard.js']).not.toBeNull()
    })
  })

  describe('server packages', () => {
    it('should return success for a project with no server packages', () => {
      const check = new CheckServerPackages()
      const project = simpleDefaultProject()
      const projectContents = project.projectContents
      const result = check.check(projectContents)
      expect(result.resolution).toBe(RequirementResolutionResult.Passed)
    })

    it('should return failure for a project with server packages', () => {
      const check = new CheckServerPackages()
      const project = simpleDefaultProject({
        additionalFiles: {
          '/package.json': textFile(
            textFileContents(
              JSON.stringify({ dependencies: { next: '14.2.5' } }, null, 2),
              unparsed,
              RevisionsState.CodeAhead,
            ),
            null,
            null,
            0,
          ),
        },
      })
      const projectContents = project.projectContents
      const result = check.check(projectContents)
      expect(result.resolution).toBe(RequirementResolutionResult.Critical)
    })

    it('should return partial for a project with node builtins', () => {
      const check = new CheckServerPackages()
      const project = simpleDefaultProject({
        additionalFiles: {
          '/src/app.js': textFile(
            textFileContents(
              `import { readFileSync } from 'fs'`,
              unparsed,
              RevisionsState.CodeAhead,
            ),
            null,
            null,
            0,
          ),
        },
      })
      const parsedProjectContents = parseProjectContents(project.projectContents)
      const result = check.check(parsedProjectContents)
      expect(result.resolution).toBe(RequirementResolutionResult.Partial)
    })

    it('should return success for a project with node builtins that are shimmed', () => {
      const check = new CheckServerPackages()
      const project = simpleDefaultProject({
        additionalFiles: {
          '/src/app.js': textFile(
            textFileContents(
              `import { readFileSync } from 'fs'`,
              unparsed,
              RevisionsState.CodeAhead,
            ),
            null,
            null,
            0,
          ),
          '/package.json': textFile(
            textFileContents(
              JSON.stringify({ dependencies: { fs: '1.0.0' } }, null, 2),
              unparsed,
              RevisionsState.CodeAhead,
            ),
            null,
            null,
            0,
          ),
        },
      })
      const parsedProjectContents = parseProjectContents(project.projectContents)
      const result = check.check(parsedProjectContents)
      expect(result.resolution).toBe(RequirementResolutionResult.Passed)
    })
  })
})
