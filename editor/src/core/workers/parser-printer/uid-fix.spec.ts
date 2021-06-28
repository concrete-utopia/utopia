import { getUtopiaID } from '../../model/element-template-utils'
import { getComponentsFromTopLevelElements } from '../../model/project-file-utils'
import {
  isJSXElement,
  isJSXElementLikeWithChildren,
  isJSXFragment,
  JSXElementChild,
} from '../../shared/element-template'
import { isParseSuccess, ParsedTextFile } from '../../shared/project-file-types'
import { emptySet } from '../../shared/set-utils'
import { lintAndParse } from './parser-printer'

describe('fixParseSuccessUIDs', () => {
  it('does not fix identical file', () => {
    const newFile = lintAndParse('test.js', baseFileContents, baseFile, emptySet())
    expect(getUidTree(newFile)).toEqual(getUidTree(baseFile))
    expect(getUidTree(newFile)).toMatchInlineSnapshot(`
      "4ed
        random-uuid
      001
        f6d
      storyboard
        scene
          component"
    `)
  })

  it('does not die on top level element change', () => {
    const newFile = lintAndParse('test.js', baseFileWithTwoTopLevelComponents, null, emptySet())
    const newFileFixed = lintAndParse(
      'test.js',
      baseFileWithTwoTopLevelComponentsUpdated,
      newFile,
      emptySet(),
    )
    expect(getUidTree(newFileFixed)).toEqual(getUidTree(newFile))
    expect(getUidTree(newFileFixed)).toMatchInlineSnapshot(`
      "4ed
        random-uuid
      0bf
        a2e
      001
        f6d
      storyboard
        scene
          component"
    `)
  })

  it('does not die on top level fragment element change', () => {
    const fileWithFragment = lintAndParse(
      'test.js',
      baseFileWithTopLevelFragmentComponent,
      null,
      emptySet(),
    )
    const fileWithFragmentUpdated = lintAndParse(
      'test.js',
      fileWithTopLevelFragmentComponentUpdateContents,
      fileWithFragment,
      emptySet(),
    )
    expect(getUidTree(fileWithFragmentUpdated)).toEqual(getUidTree(fileWithFragment))
    expect(getUidTree(fileWithFragmentUpdated)).toMatchInlineSnapshot(`
      "4ed
        random-uuid
      random-uuid
        001
        f6d
      storyboard
        scene
          component"
    `)
  })

  it('founds and fixes a single line change', () => {
    const newFile = lintAndParse('test.js', fileWithSingleUpdateContents, baseFile, emptySet())
    expect(getUidTree(newFile)).toEqual(getUidTree(baseFile))
    expect(getUidTree(newFile)).toMatchInlineSnapshot(`
      "4ed
        random-uuid
      001
        f6d
      storyboard
        scene
          component"
    `)
  })

  it('avoids uid shifting caused by single prepending insertion', () => {
    const newFile = lintAndParse('test.js', fileWithOneInsertedView, baseFile, emptySet())
    expect(getUidTree(newFile)).toMatchInlineSnapshot(`
      "4ed
        random-uuid
      001
        a2e
        f6d
      storyboard
        scene
          component"
    `)
  })

  it('double duplication', () => {
    const newFile = lintAndParse('test.js', fileWithTwoDuplicatedViews, baseFile, emptySet())
    expect(getUidTree(newFile)).toMatchInlineSnapshot(`
      "4ed
        random-uuid
      001
        f6d
        1d1
        8d0
      storyboard
        scene
          component"
    `)
  })

  it('insertion at the beginning', () => {
    const threeViews = lintAndParse('test.js', fileWithTwoDuplicatedViews, null, emptySet())
    const fourViews = lintAndParse(
      'test.js',
      fileWithTwoDuplicatesAndInsertion,
      threeViews,
      emptySet(),
    )
    expect(getUidTree(fourViews)).toMatchInlineSnapshot(`
      "4ed
        random-uuid
      001
        f07
        f6d
        1d1
        8d0
      storyboard
        scene
          component"
    `)
  })

  it('uids should match including root element when root element changes', () => {
    const firstResult = lintAndParse('test.js', baseFileContents, null, emptySet())
    const secondResult = lintAndParse(
      'test.js',
      baseFileContentsWithDifferentBackground,
      firstResult,
      emptySet(),
    )
    expect(getUidTree(firstResult)).toEqual(getUidTree(secondResult))
  })
})

const baseFileContents = createFileText(`
export var SameFileApp = (props) => {
  return (
    <div
      style={{ position: 'relative', width: '100%', height: '100%', backgroundColor: '#FFFFFF' }}
    >
      <View
        style={{
          width: 191,
        }}
      />
    </div>
  )
}
`)

const baseFileContentsWithDifferentBackground = createFileText(`
export var SameFileApp = (props) => {
  return (
    <div
      style={{ position: 'relative', width: '100%', height: '100%', backgroundColor: 'red' }}
    >
      <View
        style={{
          width: 191,
        }}
      />
    </div>
  )
}
`)

const baseFileWithTwoTopLevelComponents = createFileText(`
export var SecondComponent = (props) => {
  return (
    <div
      style={{ position: 'relative', width: '100%', height: '100%', backgroundColor: '#000000' }}
    >
      <View
        style={{
          width: 100,
        }}
      />
    </div>
  )
}

export var SameFileApp = (props) => {
  return (
    <div
      style={{ position: 'relative', width: '100%', height: '100%', backgroundColor: '#FFFFFF' }}
    >
      <View
        style={{
          width: 191,
        }}
      />
    </div>
  )
}
`)

const baseFileWithTwoTopLevelComponentsUpdated = createFileText(`
export var SecondComponent = (props) => {
  return (
    <div
      style={{ position: 'relative', width: '100%', height: '100%', backgroundColor: 'hotpink' }}
    >
      <View
        style={{
          width: 100,
        }}
      />
    </div>
  )
}

export var SameFileApp = (props) => {
  return (
    <div
      style={{ position: 'relative', width: '100%', height: '100%', backgroundColor: '#FFFFFF' }}
    >
      <View
        style={{
          width: 191,
        }}
      />
    </div>
  )
}
`)

const fileWithSingleUpdateContents = createFileText(`
export var SameFileApp = (props) => {
  return (
    <div
      style={{ position: 'relative', width: '100%', height: '100%', backgroundColor: '#FFFFFF' }}
    >
      <View
        style={{
          width: 101, // <- this is updated
        }}
      />
    </div>
  )
}
`)

const fileWithOneInsertedView = createFileText(`
export var SameFileApp = (props) => {
  return (
    <div
      style={{ position: 'relative', width: '100%', height: '100%', backgroundColor: '#FFFFFF' }}
    >
      <View
        style={{
          width: 100,
        }}
      />
      <View
        style={{
          width: 191,
        }}
      />
    </div>
  )
}
`)

const fileWithTwoDuplicatedViews = createFileText(`
export var SameFileApp = (props) => {
  return (
    <div
      style={{ position: 'relative', width: '100%', height: '100%', backgroundColor: '#FFFFFF' }}
    >
      <View
        style={{
          width: 191,
        }}
      />
      <View
        style={{
          width: 191,
        }}
      />
      <View
        style={{
          width: 191,
        }}
      />
    </div>
  )
}
`)

const fileWithTwoDuplicatesAndInsertion = createFileText(`
export var SameFileApp = (props) => {
  return (
    <div
      style={{ position: 'relative', width: '100%', height: '100%', backgroundColor: '#FFFFFF' }}
    >
      <View
        style={{
          width: 5, // a new View!
        }}
      />
      <View
        style={{
          width: 191,
        }}
      />
      <View
        style={{
          width: 191,
        }}
      />
      <View
        style={{
          width: 191,
        }}
      />
    </div>
  )
}
`)

const baseFileWithTopLevelFragmentComponent = createFileText(`
export var SameFileApp = (props) => {
  return (
    <>
      <div
        style={{ position: 'relative', width: '100%', height: '100%', backgroundColor: '#FFFFFF' }}
      >
      </div>
      <View
        style={{
          width: 191,
        }}
      />
    </>
  )
}
`)

const fileWithTopLevelFragmentComponentUpdateContents = createFileText(`
export var SameFileApp = (props) => {
  return (
    <>
      <div
        style={{ position: 'relative', width: '100%', height: '100%', backgroundColor: '#FFFFFF' }}
      >
      </div>
      <View
        style={{
          width: 250, // this is updated
        }}
      />
    </>
  )
}
`)

function createFileText(codeSnippet: string): string {
  return `
  /** @jsx jsx */
  import * as React from 'react'
  import { Scene, Storyboard } from 'utopia-api'
  import { View } from 'utopia-api'

  // arbitrary block
  export var Arbitrary = props => {
    return (
      <View>
        {<div />}
      </View>
    )
  }

  ${codeSnippet}
  
  export var storyboard = (
    <Storyboard data-uid='storyboard'>
      <Scene
        data-label='Same File App'
        data-uid='scene'
        style={{ position: 'absolute', left: 0, top: 0, width: 375, height: 812 }}
      >
        <SameFileApp data-uid='component' />
      </Scene>
    </Storyboard>
  )
  `
}

const baseFile = lintAndParse('test.js', baseFileContents, null, emptySet())

function getUidTree(parsedFile: ParsedTextFile): string {
  if (!isParseSuccess(parsedFile)) {
    return 'FILE NOT PARSE SUCCESS (◕︵◕)'
  } else {
    let printedUidLines: Array<string> = []
    const components = getComponentsFromTopLevelElements(parsedFile.topLevelElements)

    function walkElementChildren(depthSoFar: number, elements: Array<JSXElementChild>): void {
      elements.forEach((element) => {
        const uid = getUtopiaID(element)
        const uidWithoutRandomUUID = uid.includes('-') ? 'random-uuid' : uid
        printedUidLines.push(`${'  '.repeat(depthSoFar)}${uidWithoutRandomUUID}`)

        if (element != null && isJSXElementLikeWithChildren(element)) {
          walkElementChildren(depthSoFar + 1, element.children)
        }
      })
    }

    components.forEach((component) => walkElementChildren(0, [component.rootElement]))
    return printedUidLines.join('\n')
  }
}
