import { objectMap } from '../../core/shared/object-utils'
import {
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
import { directory } from '../../core/model/project-file-utils'
import { contentsToTree, ProjectContentTreeRoot } from '../assets'
import { DefaultPackageJson, StoryboardFilePath } from '../editor/store/editor-state'

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
  )
  `,
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
  }
  `,
    ),
    '/src/card.js': createCodeFile(
      '/src/app.js',
      `/** @jsx jsx */
  import * as React from 'react'
  import { jsx, Rectangle } from 'utopia-api'
  export var Card = (props) => {
    return <div data-uid='card-outer-div'>
      <div data-uid='card-inner-div' />
      <Rectangle data-uid='card-inner-rectangle' /> 
    </div>
  }
  `,
    ),
    '/utopia/unparsedstoryboard.js': createCodeFile(
      StoryboardFilePath,
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
  )
  `,
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

export function createCodeFile(path: string, contents: string): TextFile {
  const result = lintAndParse(path, contents)
  return textFile(textFileContents(contents, result, RevisionsState.CodeAhead), null, Date.now())
}
