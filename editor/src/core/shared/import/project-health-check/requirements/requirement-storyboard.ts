import type { ProjectContentTreeRoot } from 'utopia-shared/src/types'
import type { RequirementCheckResult, RequirementCheckStage } from '../utopia-requirements-types'
import { RequirementResolutionResult } from '../utopia-requirements-types'
import type { RequirementCheck } from '../utopia-requirements-types'
import { defaultEither } from '../../../../../core/shared/either'
import { StoryboardFilePath } from '../../../../../components/editor/store/editor-state'
import { getPackageJsonFromProjectContents } from '../../../../../components/editor/store/editor-state'
import {
  addFileToProjectContents,
  getProjectFileByFilePath,
} from '../../../../../components/assets'
import { codeFile } from '../../../../../core/shared/project-file-types'
import { addStoryboardFileToProject } from '../../../../../core/model/storyboard-utils'

export default class CheckStoryboard implements RequirementCheck {
  stage: RequirementCheckStage = 'pre-parsed'
  check(projectContents: ProjectContentTreeRoot): RequirementCheckResult {
    return createStoryboardFileIfNecessaryInner(projectContents)
  }
}

export function createStoryboardFileIfNecessary(
  projectContents: ProjectContentTreeRoot,
): ProjectContentTreeRoot {
  const result = createStoryboardFileIfNecessaryInner(projectContents)
  return result.newProjectContents ?? projectContents
}

function createStoryboardFileIfNecessaryInner(
  projectContents: ProjectContentTreeRoot,
): RequirementCheckResult {
  const storyboardFile = getProjectFileByFilePath(projectContents, StoryboardFilePath)
  if (storyboardFile != null) {
    return {
      resolution: RequirementResolutionResult.Passed,
      resultText: 'Storyboard.js found',
    }
  }

  const result =
    createStoryboardFileIfRemixProject(projectContents) ??
    createStoryboardFileIfMainComponentPresent(projectContents) ??
    createStoryboardFileWithPlaceholderContents(projectContents)

  if (result == projectContents) {
    return {
      resolution: RequirementResolutionResult.Partial,
      resultText: 'Storyboard.js skipped',
    }
  } else {
    return {
      resolution: RequirementResolutionResult.Fixed,
      resultText: 'Storyboard.js created',
      newProjectContents: result,
    }
  }
}

function createStoryboardFileIfRemixProject(
  projectContents: ProjectContentTreeRoot,
): ProjectContentTreeRoot | null {
  const packageJsonContents = defaultEither(
    null,
    getPackageJsonFromProjectContents(projectContents),
  )
  if (packageJsonContents == null) {
    return null
  }
  const remixNotIncluded = packageJsonContents['dependencies']?.['@remix-run/react'] == null
  if (remixNotIncluded) {
    return null
  }

  const updatedProjectContents = addFileToProjectContents(
    projectContents,
    StoryboardFilePath,
    codeFile(DefaultStoryboardWithRemix, null, 1),
  )
  return updatedProjectContents
}

function createStoryboardFileIfMainComponentPresent(
  projectContents: ProjectContentTreeRoot,
): ProjectContentTreeRoot | null {
  return addStoryboardFileToProject(projectContents)
}

function createStoryboardFileWithPlaceholderContents(
  projectContents: ProjectContentTreeRoot,
): ProjectContentTreeRoot {
  const updatedProjectContents = addFileToProjectContents(
    projectContents,
    StoryboardFilePath,
    codeFile(DefaultStoryboardContents, null, 1),
  )
  return updatedProjectContents
}

const DefaultStoryboardWithRemix = `import * as React from 'react'
import { Storyboard, RemixScene } from 'utopia-api'

export var storyboard = (
  <Storyboard>
    <RemixScene
      style={{
        position: 'absolute',
        width: 644,
        height: 750,
        left: 200,
        top: 30,
        overflow: 'hidden',
      }}
      data-label='Mood Board'
    />
  </Storyboard>
)
`

const DefaultStoryboardContents = `import * as React from 'react'
import { Scene, Storyboard } from 'utopia-api'

export var storyboard = (
  <Storyboard>
    <Scene
      style={{
        width: 603,
        height: 794,
        position: 'absolute',
        left: 212,
        top: 128,
        display: 'flex',
        flexDirection: 'column',
        padding: '253px 101px',
        alignItems: 'center',
        justifyContent: 'center',
      }}
    >
      <span
        style={{
          wordBreak: 'break-word',
          fontSize: '25px',
          width: 257,
          height: 130,
        }}
      >
        Open the insert menu or press the + button in the
        toolbar to insert components
      </span>
    </Scene>
  </Storyboard>
  )
`
