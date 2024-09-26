import {
  addFileToProjectContents,
  getProjectFileByFilePath,
  removeFromProjectContents,
} from '../../components/assets'
import type { EditorState } from '../../components/editor/store/editor-state'
import {
  editorModelFromPersistentModel,
  StoryboardFilePath,
} from '../../components/editor/store/editor-state'
import { complexDefaultProject } from '../../sample-projects/sample-project-utils'
import { clearTopLevelElementUniqueIDs } from '../shared/element-template'
import type { ParsedTextFile } from '../shared/project-file-types'
import {
  foldParsedTextFile,
  isParseSuccess,
  isTextFile,
  RevisionsState,
  textFile,
  textFileContents,
  unparsed,
} from '../shared/project-file-types'
import { emptySet } from '../shared/set-utils'
import { NO_OP } from '../shared/utils'
import { lintAndParse } from '../workers/parser-printer/parser-printer'
import { addStoryboardFileToProject } from './storyboard-utils'

function createTestProjectLackingStoryboardFile(): EditorState {
  const appFile = `
import React from 'react'
export var App = (props) => {
  return <div style={{ ...props.style}} data-uid={'aaa'} data-label={'Hat'} />
}`
  const baseModel = complexDefaultProject()
  const parsedFile = lintAndParse(
    StoryboardFilePath,
    [],
    appFile,
    'trim-bounds',
    'do-not-apply-steganography',
  ) as ParsedTextFile

  if (!isParseSuccess(parsedFile)) {
    throw new Error('The test file parse failed')
  }

  const projectContentsWithoutStoryboard = removeFromProjectContents(
    baseModel.projectContents,
    StoryboardFilePath,
  )

  const persistentModel = {
    ...baseModel,
    projectContents: addFileToProjectContents(
      projectContentsWithoutStoryboard,
      '/src/app.js',
      textFile(
        textFileContents(appFile, parsedFile, RevisionsState.BothMatch),
        null,
        parsedFile,
        Date.now(),
      ),
    ),
  }

  return editorModelFromPersistentModel(persistentModel, NO_OP)
}

describe('addStoryboardFileToProject', () => {
  it('adds storyboard file to project that does not have one', () => {
    const actualResult = addStoryboardFileToProject(
      createTestProjectLackingStoryboardFile().projectContents,
    )
    if (actualResult == null) {
      throw new Error('Editor state was not updated.')
    } else {
      const storyboardFile = getProjectFileByFilePath(actualResult, StoryboardFilePath)
      if (storyboardFile == null) {
        throw new Error('No storyboard file was created.')
      } else {
        if (isTextFile(storyboardFile)) {
          const topLevelElements = foldParsedTextFile(
            (_) => [],
            (success) => {
              return success.topLevelElements.map(clearTopLevelElementUniqueIDs)
            },
            (_) => [],
            storyboardFile.fileContents.parsed,
          )
          expect(topLevelElements).toMatchSnapshot()
        } else {
          throw new Error('Storyboard file is not a UI JS file.')
        }
      }
    }
  })
  it('does not add a storyboard file to a project that already has one', () => {
    const expectedFile = textFile(
      textFileContents(
        'oh no, this is not a real storyboard file',
        unparsed,
        RevisionsState.CodeAhead,
      ),
      null,
      null,
      0,
    )
    let editorModel = createTestProjectLackingStoryboardFile()
    editorModel = {
      ...editorModel,
      projectContents: addFileToProjectContents(
        editorModel.projectContents,
        StoryboardFilePath,
        expectedFile,
      ),
    }
    const actualResult = addStoryboardFileToProject(editorModel.projectContents)
    expect(actualResult).toBeNull()
  })
})
