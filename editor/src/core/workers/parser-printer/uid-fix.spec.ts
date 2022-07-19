import { getUtopiaID } from '../../model/element-template-utils'
import { getComponentsFromTopLevelElements } from '../../model/project-file-utils'
import {
  isJSXElement,
  isJSXElementLikeWithChildren,
  isJSXFragment,
  JSXElementChild,
} from '../../shared/element-template'
import { isParseSuccess, ParsedTextFile, ParseSuccess } from '../../shared/project-file-types'
import { emptySet } from '../../shared/set-utils'
import { lintAndParse } from './parser-printer'

function asParseSuccessOrNull(file: ParsedTextFile): ParseSuccess | null {
  return isParseSuccess(file) ? file : null
}

describe('fixParseSuccessUIDs', () => {
  it('does not fix identical file', () => {
    const newFile = lintAndParse(
      'test.js',
      baseFileContents,
      asParseSuccessOrNull(baseFile),
      emptySet(),
    )
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
      asParseSuccessOrNull(newFile),
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
      asParseSuccessOrNull(fileWithFragment),
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
    const newFile = lintAndParse(
      'test.js',
      fileWithSingleUpdateContents,
      asParseSuccessOrNull(baseFile),
      emptySet(),
    )
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
    const newFile = lintAndParse(
      'test.js',
      fileWithOneInsertedView,
      asParseSuccessOrNull(baseFile),
      emptySet(),
    )
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
    const newFile = lintAndParse(
      'test.js',
      fileWithTwoDuplicatedViews,
      asParseSuccessOrNull(baseFile),
      emptySet(),
    )
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
      asParseSuccessOrNull(threeViews),
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
      asParseSuccessOrNull(firstResult),
      emptySet(),
    )
    expect(getUidTree(firstResult)).toEqual(getUidTree(secondResult))
  })

  it('handles elements within arbitrary blocks', () => {
    const firstResult = lintAndParse('test.js', fileWithArbitraryBlockInside, null, emptySet())
    const secondResult = lintAndParse(
      'test.js',
      fileWithSlightlyDifferentArbitraryBlockInside,
      asParseSuccessOrNull(firstResult),
      emptySet(),
    )
    expect(getUidTree(firstResult)).toEqual(getUidTree(secondResult))
  })

  it('handles newly inserted duplicate elements without duplicating uids', () => {
    const firstResult = lintAndParse('test.js', fileWithBasicDiv, null, emptySet())
    const secondResult = lintAndParse(
      'test.js',
      fileWithBasicDivDuplicated,
      asParseSuccessOrNull(firstResult),
      emptySet(),
    )
    expect(getUidTree(secondResult)).toMatchInlineSnapshot(`
      "4ed
        random-uuid
      001
        593
          c85
            random-uuid
      storyboard
        scene
          component"
    `)
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

const fileWithArbitraryBlockInside = createFileText(`
export var SameFileApp = (props) => {
  return (
    <div
      style={{ position: 'relative', width: '100%', height: '100%', backgroundColor: '#FFFFFF' }}
    >
      {[1, 2, 3].map((index) => {
      <View
        style={{
          width: 191 + index,
        }}
      />
      })}
    </div>
  )
}
`)

const fileWithSlightlyDifferentArbitraryBlockInside = createFileText(`
export var SameFileApp = (props) => {
  return (
    <div
      style={{ position: 'relative', width: '100%', height: '100%', backgroundColor: '#FFFFFF' }}
    >
      {[1, 2, 3].map((index) => {
      <View
        style={{
          width: 161 + index,
        }}
      />
      })}
    </div>
  )
}
`)

const fileWithBasicDiv = createFileText(`
export var SameFileApp = (props) => {
  return (
    <div
      style={{ position: 'relative', width: '100%', height: '100%', backgroundColor: '#FFFFFF' }}
    >
      <div>Hello</div>
    </div>
  )
}
`)

const fileWithBasicDivDuplicated = createFileText(`
export var SameFileApp = (props) => {
  return (
    <div
      style={{ position: 'relative', width: '100%', height: '100%', backgroundColor: '#FFFFFF' }}
    >
      <div><div>Hello</div></div>
    </div>
  )
}
`)

function createFileText(codeSnippet: string): string {
  return `
  /** @jsx jsx */
  import * as React from 'react'
  import Utopia, {
    Scene,
    Storyboard,
    registerModule,
  } from 'utopia-api'
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
