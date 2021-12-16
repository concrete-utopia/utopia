import { contentsToTree, ProjectContentTreeRoot } from '../components/assets'
import {
  DefaultPackageJson,
  PersistentModel,
  persistentModelForProjectContents,
  StoryboardFilePath,
} from '../components/editor/store/editor-state'
import {
  appJSFile,
  getDefaultUIJsFile,
  getSamplePreviewFile,
  getSamplePreviewHTMLFile,
} from '../core/model/new-project-files'
import { directory } from '../core/model/project-file-utils'
import {
  ProjectContents,
  RevisionsState,
  TextFile,
  textFile,
  textFileContents,
  unparsed,
} from '../core/shared/project-file-types'

export function simpleDefaultProject(): PersistentModel {
  const projectContents: ProjectContents = {
    '/package.json': textFile(
      textFileContents(
        JSON.stringify(DefaultPackageJson, null, 2),
        unparsed,
        RevisionsState.CodeAhead,
      ),
      null,
      null,
      0,
    ),
    '/src': directory(),
    '/src/app.js': appJSFile(),
    [StoryboardFilePath]: getDefaultUIJsFile(),
    '/assets': directory(),
    '/public': directory(),
    '/src/index.js': getSamplePreviewFile(),
    '/public/index.html': getSamplePreviewHTMLFile(),
  }

  let persistentModel = persistentModelForProjectContents(contentsToTree(projectContents))
  return persistentModel
}

export function createComplexDefaultProjectContents(): ProjectContents {
  function createCodeFile(path: string, contents: string): TextFile {
    return textFile(
      textFileContents(contents, unparsed, RevisionsState.CodeAhead),
      null,
      null,
      Date.now(),
    )
  }

  return {
    '/package.json': textFile(
      textFileContents(
        JSON.stringify(DefaultPackageJson, null, 2),
        unparsed,
        RevisionsState.CodeAhead,
      ),
      null,
      null,
      0,
    ),
    '/src': directory(),
    '/assets': directory(),
    '/public': directory(),
    '/src/index.js': createCodeFile('/src/index.js', getSamplePreviewFile().fileContents.code),
    '/public/index.html': getSamplePreviewHTMLFile(),
    [StoryboardFilePath]: createCodeFile(
      StoryboardFilePath,
      `import * as React from 'react'
import { Scene, Storyboard } from 'utopia-api/components'
import { App } from '/src/app.js'

export var SameFileApp = (props) => {
  return <div data-uid='same-file-app-div' style={{ position: 'relative', width: '100%', height: '100%', backgroundColor: '#FFFFFF' }} />
}

export var storyboard = (
  <Storyboard data-uid='storyboard-entity'>
    <Scene
      data-label='Imported App'
      data-uid='scene-1-entity'
      style={{ position: 'absolute', left: 0, top: 0, width: 375, height: 812 }}
    >
      <App data-uid='app-entity' />
    </Scene>
    <Scene
      data-label='Same File App'
      data-uid='scene-2-entity'
      style={{ position: 'absolute', left: 400, top: 0, width: 375, height: 812 }}
    >
      <SameFileApp data-uid='same-file-app-entity' />
    </Scene>
  </Storyboard>
)`,
    ),
    '/src/app.js': createCodeFile(
      '/src/app.js',
      `import * as React from 'react'
import { Card } from '/src/card.js'
export var App = (props) => {
  return (
    <div
      data-uid='app-outer-div'
      style={{ position: 'relative', width: '100%', height: '100%', backgroundColor: '#FFFFFF' }}
    >
      <Card data-uid='card-instance' style={{ position: 'absolute', left: 0, top: 0, width: 200, height: 300}} />
    </div>
  )
}`,
    ),
    '/src/card.js': createCodeFile(
      '/src/card.js',
      `import * as React from 'react'
import { Rectangle } from 'utopia-api/components'
export var Card = (props) => {
  return <div data-uid='card-outer-div' style={{...props.style}}>
    <div data-uid='card-inner-div' style={{ position: 'absolute', left: 0, top: 0, width: 50, height: 50, backgroundColor: 'red' }} />
    <Rectangle data-uid='card-inner-rectangle' style={{ position: 'absolute', left: 100, top: 200, width: 50, height: 50, backgroundColor: 'blue' }} />
  </div>
}`,
    ),
  }
}

export function complexDefaultProject(): PersistentModel {
  const projectContents = createComplexDefaultProjectContents()
  const persistentModel = persistentModelForProjectContents(contentsToTree(projectContents))
  return persistentModel
}

export function defaultProject(): PersistentModel {
  if (process.env.NODE_ENV === 'production') {
    return simpleDefaultProject()
  } else {
    return complexDefaultProject()
  }
}
