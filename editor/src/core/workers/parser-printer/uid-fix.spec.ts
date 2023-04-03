import { getUtopiaID } from '../../../core/shared/uid-utils'
import { getComponentsFromTopLevelElements } from '../../model/project-file-utils'
import * as FastCheck from 'fast-check'
import {
  isJSXElement,
  isJSXElementLike,
  isJSXFragment,
  JSExpression,
  JSXElementChild,
} from '../../shared/element-template'
import { isParseSuccess, ParsedTextFile, ParseSuccess } from '../../shared/project-file-types'
import { emptySet } from '../../shared/set-utils'
import { lintAndParse } from './parser-printer'
import { jsxAttributeArbitrary, jsxElementChildArbitrary } from './parser-printer.test-utils'
import { Arbitrary } from 'fast-check'
import { fixExpressionUIDs, fixJSXElementChildUIDs } from './uid-fix'

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
      344
        3da
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
      f0f
        95f
      344
        3da
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
      a61
        344
        3da
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
      344
        3da
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
      344
        95f
        3da
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
      344
        3da
        f22
        931
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
      344
        686
        3da
        f22
        931
      storyboard
        scene
          component"
    `)
  })

  it('multiple uid changes but the number of elements stays the same', () => {
    const threeViews = lintAndParse('test.js', fileWithTwoDuplicatedViews, null, emptySet())
    const updatedThreeViews = lintAndParse(
      'test.js',
      fileWithTwoDuplicatedViewsWithChanges,
      asParseSuccessOrNull(threeViews),
      emptySet(),
    )
    expect(getUidTree(updatedThreeViews)).toEqual(getUidTree(threeViews))
  })

  it('can handle text children fine', () => {
    const start = lintAndParse('test.js', fileWithTwoTextElements, null, emptySet())
    const end = lintAndParse(
      'test.js',
      fileWithTwoTextElementsWithChanges,
      asParseSuccessOrNull(start),
      emptySet(),
    )
    expect(getUidTree(end)).toEqual(getUidTree(start))
  })

  it('can handle changes to a parent and child', () => {
    const start = lintAndParse('test.js', baseFileContents, null, emptySet())
    const end = lintAndParse(
      'test.js',
      fileWithChildAndParentUpdated,
      asParseSuccessOrNull(start),
      emptySet(),
    )
    expect(getUidTree(end)).toEqual(getUidTree(start))
  })

  it('re-ordered elements should re-order the uid tree', () => {
    const beforeReOrder = lintAndParse('test.js', fileWithOneInsertedView, null, emptySet())
    const afterReOrder = lintAndParse(
      'test.js',
      fileWithOneInsertedViewReOrdered,
      asParseSuccessOrNull(beforeReOrder),
      emptySet(),
    )
    expect(getUidTree(beforeReOrder)).toMatchInlineSnapshot(`
      "4ed
        random-uuid
      344
        95f
        3da
      storyboard
        scene
          component"
    `)
    expect(getUidTree(afterReOrder)).toMatchInlineSnapshot(`
      "4ed
        random-uuid
      344
        3da
        95f
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
      344
        593
          c85
            random-uuid
      storyboard
        scene
          component"
    `)
  })
})

describe('fixExpressionUIDs', () => {
  it('if expressions are the same type, their uids will be copied over', () => {
    const arbitraryExpressionPair: Arbitrary<{ first: JSExpression; second: JSExpression }> =
      FastCheck.tuple(jsxAttributeArbitrary(2), jsxAttributeArbitrary(2)).map(([first, second]) => {
        return {
          first: first,
          second: second,
        }
      })
    function checkCall(value: { first: JSExpression; second: JSExpression }): boolean {
      const { first, second } = value
      const afterFix = fixExpressionUIDs(first, second, {
        mutableAllNewUIDs: emptySet(),
        mappings: [],
      })
      if (first.type === second.type) {
        return first.uid === afterFix.uid
      } else {
        return first.uid !== afterFix.uid
      }
    }
    const prop = FastCheck.property(arbitraryExpressionPair, checkCall)
    FastCheck.assert(prop, { verbose: true })
  })
})

describe('fixJSXElementChildUIDs', () => {
  it('if expressions are the same type, their uids will be copied over', () => {
    const arbitraryElementPair: Arbitrary<{ first: JSXElementChild; second: JSXElementChild }> =
      FastCheck.tuple(jsxElementChildArbitrary(3), jsxElementChildArbitrary(3)).map(
        ([first, second]) => {
          return {
            first: first,
            second: second,
          }
        },
      )
    function checkCall(value: { first: JSXElementChild; second: JSXElementChild }): boolean {
      const { first, second } = value
      const afterFix = fixJSXElementChildUIDs(first, second, {
        mutableAllNewUIDs: emptySet(),
        mappings: [],
      })
      if (first.type === second.type) {
        return first.uid === afterFix.uid
      } else {
        return first.uid !== afterFix.uid
      }
    }
    const prop = FastCheck.property(arbitraryElementPair, checkCall)
    FastCheck.assert(prop, { verbose: true })
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

const fileWithChildAndParentUpdated = createFileText(`
export var SameFileApp = (props) => {
  return (
    <div
      style={{ position: 'relative', width: '150', height: '100%', backgroundColor: '#FFFFFF' }}
    >
      <View
        style={{
          width: 150,
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

const fileWithOneInsertedViewReOrdered = createFileText(`
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
          width: 100,
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

const fileWithTwoDuplicatedViewsWithChanges = createFileText(`
export var SameFileApp = (props) => {
  return (
    <div
      style={{ position: 'relative', width: '100%', height: '100%', backgroundColor: '#FFFFFF' }}
    >
      <View
        style={{
          width: 191,
          height: 100,
        }}
      />
      <View
        style={{
          width: 191,
          height: 200,
        }}
      />
      <View
        style={{
          width: 191,
          height: 300,
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

const fileWithTwoTextElements = createFileText(`
export var SameFileApp = (props) => {
  return (
    <div
      style={{ position: 'relative', width: '100%', height: '100%', backgroundColor: '#FFFFFF' }}
    >
      <div
        style={{
          width: 100,
        }}
      >
      Hello
      </div>
      <div
        style={{
          width: 200,
        }}
      >
      There
      </div>
    </div>
  )
}
`)

const fileWithTwoTextElementsWithChanges = createFileText(`
export var SameFileApp = (props) => {
  return (
    <div
      style={{ position: 'relative', width: '100%', height: '100%', backgroundColor: '#FFFFFF' }}
    >
      <div
        style={{
          width: 150,
        }}
      >
      Hello,
      </div>
      <div
        style={{
          width: 250,
        }}
      >
      There!
      </div>
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

        if (element != null && isJSXElementLike(element)) {
          walkElementChildren(depthSoFar + 1, element.children)
        }
      })
    }

    components.forEach((component) => walkElementChildren(0, [component.rootElement]))
    return printedUidLines.join('\n')
  }
}
