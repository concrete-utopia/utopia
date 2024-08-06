import * as Benny from 'benny'
import { fromField, fromTypeGuard, traverseArray } from './optics/optic-creators'
import type { Optic } from './optics/optics'
import { modify, toArrayOf, toFirst } from './optics/optic-utilities'
import type { ProjectContentTreeRoot, TextFile } from 'utopia-shared/src/types'
import { contentsToTree, contentsTreeOptic } from '../../components/assets'
import type { ProjectContents } from './project-file-types'
import {
  RevisionsState,
  directory,
  isTextFile,
  textFile,
  textFileContents,
  unparsed,
} from './project-file-types'
import { getSamplePreviewFile, getSamplePreviewHTMLFile } from '../model/new-project-files'

export function createComplexDefaultProjectContents(): ProjectContents {
  function createCodeFile(contents: string): TextFile {
    return textFile(textFileContents(contents, unparsed, RevisionsState.CodeAhead), null, null, 0)
  }

  return {
    '/package.json': textFile(
      textFileContents(
        JSON.stringify({ dependencies: [] }, null, 2),
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
    '/src/appcore/core.js': createCodeFile(`const value = 1`),
    '/src/appcore/core1.js': createCodeFile(`const value = 1`),
    '/src/appcore/core2.js': createCodeFile(`const value = 1`),
    '/src/appcore/core3.js': createCodeFile(`const value = 1`),
    '/src/appcore/core4.js': createCodeFile(`const value = 1`),
    '/src/appcore/correct/horse/battery/staple/xkcd.js': createCodeFile(`const value = 'xkcd'`),
    '/src/index.js': createCodeFile(getSamplePreviewFile().fileContents.code),
    '/public/index.html': getSamplePreviewHTMLFile(),
    '/utopia/storyboard.js': createCodeFile(
      `import * as React from 'react'
import Utopia, {
  Scene,
  Storyboard,
} from 'utopia-api'
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
      `import * as React from 'react'
export var Card = (props) => {
  return <div data-uid='card-outer-div' style={{...props.style}}>
    <div data-uid='card-inner-div' data-testid='card-inner-div' style={{ position: 'absolute', left: 0, top: 0, width: 50, height: 50, backgroundColor: 'red' }} />
  </div>
}`,
    ),
  }
}

export async function benchmarkOptics(): Promise<void> {
  await Benny.suite(
    'get an array from a nested array',
    Benny.add('array from nested array', () => {
      const optic: Optic<Array<Array<number>>, number> = traverseArray<Array<number>>().compose(
        traverseArray(),
      )
      const array: Array<Array<number>> = [
        [1, 2, 3],
        [4, 5, 6],
        [7, 8, 9],
      ]
      return () => {
        toArrayOf(optic, array)
      }
    }),
    Benny.cycle(),
    Benny.complete(),
    Benny.save({ file: 'array from nested array', details: true }),
  )
  await Benny.suite(
    'first value from a nested array',
    Benny.add('first value from nested array', () => {
      const optic: Optic<Array<Array<number>>, number> = traverseArray<Array<number>>().compose(
        traverseArray(),
      )
      const array: Array<Array<number>> = [
        [1, 2, 3],
        [4, 5, 6],
        [7, 8, 9],
      ]
      return () => {
        toFirst(optic, array)
      }
    }),
    Benny.cycle(),
    Benny.complete(),
    Benny.save({ file: 'first value from nested array', details: true }),
  )
  await Benny.suite(
    'modify a nested array',
    Benny.add('modify a nested array', () => {
      const optic: Optic<Array<Array<number>>, number> = traverseArray<Array<number>>().compose(
        traverseArray(),
      )
      const array: Array<Array<number>> = [
        [1, 2, 3],
        [4, 5, 6],
        [7, 8, 9],
      ]
      return () => {
        modify(optic, (n) => n * 2, array)
      }
    }),
    Benny.cycle(),
    Benny.complete(),
    Benny.save({ file: 'modify a nested array', details: true }),
  )
  await Benny.suite(
    'get an array from project contents',
    Benny.add('array from project contents', () => {
      const optic = contentsTreeOptic
        .compose(fromField('file'))
        .compose(fromTypeGuard(isTextFile))
        .compose(fromField('fileContents'))
        .compose(fromField('code'))
      const contentsTree: ProjectContentTreeRoot = contentsToTree(
        createComplexDefaultProjectContents(),
      )
      return () => {
        toArrayOf(optic, contentsTree)
      }
    }),
    Benny.cycle(),
    Benny.complete(),
    Benny.save({ file: 'array from project contents', details: true }),
  )
  await Benny.suite(
    'modify project contents',
    Benny.add('modify project contents', () => {
      const optic = contentsTreeOptic
        .compose(fromField('file'))
        .compose(fromTypeGuard(isTextFile))
        .compose(fromField('fileContents'))
        .compose(fromField('code'))
      const contentsTree: ProjectContentTreeRoot = contentsToTree(
        createComplexDefaultProjectContents(),
      )
      return () => {
        modify(optic, (code) => code + ' // modified', contentsTree)
      }
    }),
    Benny.cycle(),
    Benny.complete(),
    Benny.save({ file: 'modify project contents', details: true }),
  )
}
