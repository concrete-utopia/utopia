import { directory } from '../../core/model/project-file-utils'
import { objectMap } from '../../core/shared/object-utils'
import {
  InstancePath,
  isTextFile,
  ProjectContents,
  ProjectFile,
  RevisionsState,
  TextFile,
  textFile,
  textFileContents,
  unparsed,
} from '../../core/shared/project-file-types'
import { lintAndParse, parseCode } from '../../core/workers/parser-printer/parser-printer'
import { ProjectContentTreeRoot, contentsToTree, getContentsTreeFileFromString } from '../assets'
import { DefaultPackageJson, StoryboardFilePath } from '../editor/store/editor-state'
import * as TP from '../../core/shared/template-path'

function createCodeFile(path: string, contents: string): TextFile {
  const result = lintAndParse(path, contents)
  return textFile(textFileContents(contents, result, RevisionsState.CodeAhead), null, Date.now())
}

export function defaultProjectContentsForNormalising(): ProjectContentTreeRoot {
  let projectContents: ProjectContents = {
    '/package.json': textFile(
      textFileContents(
        JSON.stringify(DefaultPackageJson, null, 2),
        unparsed,
        RevisionsState.BothMatch,
      ),
      null,
      0,
    ),
    '/src': directory(),
    '/utopia': directory(),
    [StoryboardFilePath]: createCodeFile(
      StoryboardFilePath,
      `/** @jsx jsx */
import * as React from 'react'
import { Scene, Storyboard, jsx } from 'utopia-api'
import { App } from '/src/app.js'

export var SameFileApp = (props) => {
  return <div data-uid='same-file-app-div' />
}

export var storyboard = (
  <Storyboard data-uid='storyboard-entity'>
    <Scene
      data-uid='scene-1-entity'
      component={App}
      props={{}}
      style={{ position: 'absolute', left: 0, top: 0, width: 375, height: 812 }}
    />
    <Scene
      data-uid='scene-2-entity'
      component={SameFileApp}
      props={{}}
      style={{ position: 'absolute', left: 0, top: 0, width: 375, height: 812 }}
    />
  </Storyboard>
)`,
    ),
    '/src/app.js': createCodeFile(
      '/src/app.js',
      `/** @jsx jsx */
import * as React from 'react'
import { jsx } from 'utopia-api'
import { Card } from '/src/card.js'
export var App = (props) => {
  return <div data-uid='app-outer-div'>
    <Card data-uid='card-instance' />
  </div>
}`,
    ),
    '/src/card.js': createCodeFile(
      '/src/card.js',
      `/** @jsx jsx */
import * as React from 'react'
import { jsx, Rectangle } from 'utopia-api'
export var Card = (props) => {
  return <div data-uid='card-outer-div'>
    <div data-uid='card-inner-div' />
    <Rectangle data-uid='card-inner-rectangle' /> 
  </div>
}`,
    ),
    '/utopia/unparsedstoryboard.js': createCodeFile(
      '/utopia/unparsedstoryboard.js',
      `/** @jsx jsx */
import * as React from 'react'
import { Scene, Storyboard, jsx } from 'utopia-api'
import { App } from '/src/app.js'
export var storyboard = (
  <Storyboard data-uid='storyboard-entity'>
    <Scene
      data-uid='scene-1-entity'
      component={App}
      props={{}}
      style={{ position: 'absolute', left: 0, top: 0, width: 375, height: 812 }}
    />
  </Storyboard>
)`,
    ),
  }

  projectContents = objectMap((projectFile: ProjectFile, fullPath: string) => {
    if (isTextFile(projectFile) && fullPath !== '/utopia/unparsedstoryboard.js') {
      const code = projectFile.fileContents.code
      const parsedFile = parseCode(fullPath, code)
      return textFile(textFileContents(code, parsedFile, RevisionsState.BothMatch), null, 1000)
    } else {
      return projectFile
    }
  }, projectContents)

  return contentsToTree(projectContents)
}

export function getTextFileByPath(projectContents: ProjectContentTreeRoot, path: string): TextFile {
  const possibleResult = getContentsTreeFileFromString(projectContents, path)
  if (isTextFile(possibleResult)) {
    return possibleResult
  } else {
    throw new Error(`Unable to find a text file at path ${path}.`)
  }
}

export function instancePathFromString(path: string): InstancePath {
  const fromStringResult = TP.fromString(path)
  if (TP.isScenePath(fromStringResult)) {
    throw new Error(`${path} represents a scene path.`)
  } else {
    return TP.dynamicPathToStaticPath(fromStringResult)
  }
}
