/* eslint jest/expect-expect: ["error", { "assertFunctionNames": ["expect", "FastCheck.assert"] }] */
import { getUtopiaID, parseUID } from '../../../core/shared/uid-utils'
import { getComponentsFromTopLevelElements } from '../../model/project-file-utils'
import * as FastCheck from 'fast-check'
import {
  emptyComments,
  isJSXElement,
  isJSXElementLike,
  isJSXFragment,
  JSExpression,
  jsExpressionOtherJavaScript,
  jsxElement,
  JSXElement,
  JSXElementChild,
  TopLevelElement,
  walkElements,
} from '../../shared/element-template'
import { isParseSuccess, ParsedTextFile, ParseSuccess } from '../../shared/project-file-types'
import { emptySet } from '../../shared/set-utils'
import { lintAndParse } from './parser-printer'
import {
  elementsStructure,
  jsxAttributeArbitrary,
  jsxElementChildArbitrary,
} from './parser-printer.test-utils'
import { Arbitrary } from 'fast-check'
import { fixExpressionUIDs, fixJSXElementChildUIDs, FixUIDsState } from './uid-fix'
import { foldEither } from '../../../core/shared/either'
import { getAllUniqueUIdsFromElementChild } from '../../../core/model/get-unique-ids'

function asParseSuccessOrNull(file: ParsedTextFile): ParseSuccess | null {
  return isParseSuccess(file) ? file : null
}

function validateJSXElementUID(element: JSXElement): void {
  // Ensure that the `data-uid`attribute matches the `uid` property.
  const uidAttribute = parseUID(element.props, emptyComments)
  return foldEither(
    () => {
      throw new Error(`No data-uid field in element with uid ${element.uid}.`)
    },
    (fromProps) => {
      if (fromProps !== element.uid) {
        throw new Error(
          `Got data-uid field for ${fromProps} in element (of type ${element.type}) with uid ${element.uid}.`,
        )
      }
    },
    uidAttribute,
  )
}

function validateTopLevelElementsElementUIDs(topLevelElements: Array<TopLevelElement>): void {
  walkElements(topLevelElements, (element) => {
    if (isJSXElement(element)) {
      validateJSXElementUID(element)
    }
  })
}

function validateParsedTextFileElementUIDs(parsedTextFile: ParsedTextFile): void {
  if (isParseSuccess(parsedTextFile)) {
    validateTopLevelElementsElementUIDs(parsedTextFile.topLevelElements)
  }
}

function validateHighlightBoundsForUIDs(parsedTextFile: ParsedTextFile): void {
  if (isParseSuccess(parsedTextFile)) {
    for (const highlightBoundsUID of Object.keys(parsedTextFile.highlightBounds)) {
      const highlightBounds = parsedTextFile.highlightBounds[highlightBoundsUID]
      if (highlightBoundsUID !== highlightBounds.uid) {
        throw new Error(
          `A highlight bounds with a uid of ${highlightBounds.uid} was stored against a key of ${highlightBoundsUID}`,
        )
      }
    }
  }
}

function lintAndParseAndValidateResult(
  filename: string,
  content: string,
  oldParseResultForUIDComparison: ParseSuccess | null,
  alreadyExistingUIDs_MUTABLE: Set<string>,
  shouldTrimBounds: 'trim-bounds' | 'do-not-trim-bounds',
): ParsedTextFile {
  const result = lintAndParse(
    filename,
    content,
    oldParseResultForUIDComparison,
    alreadyExistingUIDs_MUTABLE,
    shouldTrimBounds,
  )
  validateParsedTextFileElementUIDs(result)
  validateHighlightBoundsForUIDs(result)
  return result
}

describe('fixParseSuccessUIDs', () => {
  it('does not fix identical file', () => {
    const newFile = lintAndParseAndValidateResult(
      'test.js',
      baseFileContents,
      asParseSuccessOrNull(baseFile),
      emptySet(),
      'trim-bounds',
    )
    expect(getUidTree(newFile)).toEqual(getUidTree(baseFile))
    expect(getUidTree(newFile)).toMatchInlineSnapshot(`
      "4ed
        4e0
      434
        112
      storyboard
        scene
          component"
    `)
  })

  it('does not die on top level element change', () => {
    const newFile = lintAndParseAndValidateResult(
      'test.js',
      baseFileWithTwoTopLevelComponents,
      null,
      emptySet(),
      'trim-bounds',
    )
    const newFileFixed = lintAndParseAndValidateResult(
      'test.js',
      baseFileWithTwoTopLevelComponentsUpdated,
      asParseSuccessOrNull(newFile),
      emptySet(),
      'trim-bounds',
    )
    expect(getUidTree(newFileFixed)).toEqual(getUidTree(newFile))
    expect(getUidTree(newFileFixed)).toMatchInlineSnapshot(`
      "4ed
        4e0
      e86
        c60
      434
        112
      storyboard
        scene
          component"
    `)
  })

  it('does not die on top level fragment element change', () => {
    const fileWithFragment = lintAndParseAndValidateResult(
      'test.js',
      baseFileWithTopLevelFragmentComponent,
      null,
      emptySet(),
      'trim-bounds',
    )
    const fileWithFragmentUpdated = lintAndParseAndValidateResult(
      'test.js',
      fileWithTopLevelFragmentComponentUpdateContents,
      asParseSuccessOrNull(fileWithFragment),
      emptySet(),
      'trim-bounds',
    )
    expect(getUidTree(fileWithFragmentUpdated)).toEqual(getUidTree(fileWithFragment))
    expect(getUidTree(fileWithFragmentUpdated)).toMatchInlineSnapshot(`
      "4ed
        4e0
      292
        434
        112
      storyboard
        scene
          component"
    `)
  })

  it('founds and fixes a single line change', () => {
    const newFile = lintAndParseAndValidateResult(
      'test.js',
      fileWithSingleUpdateContents,
      asParseSuccessOrNull(baseFile),
      emptySet(),
      'trim-bounds',
    )
    expect(getUidTree(newFile)).toEqual(getUidTree(baseFile))
    expect(getUidTree(newFile)).toMatchInlineSnapshot(`
      "4ed
        4e0
      434
        112
      storyboard
        scene
          component"
    `)
  })

  it('avoids uid shifting caused by single prepending insertion', () => {
    const newFile = lintAndParseAndValidateResult(
      'test.js',
      fileWithOneInsertedView,
      asParseSuccessOrNull(baseFile),
      emptySet(),
      'trim-bounds',
    )
    expect(getUidTree(newFile)).toMatchInlineSnapshot(`
      "4ed
        4e0
      434
        c60
        112
      storyboard
        scene
          component"
    `)
  })

  it('double duplication', () => {
    const newFile = lintAndParseAndValidateResult(
      'test.js',
      fileWithTwoDuplicatedViews,
      asParseSuccessOrNull(baseFile),
      emptySet(),
      'trim-bounds',
    )
    expect(getUidTree(newFile)).toMatchInlineSnapshot(`
      "4ed
        4e0
      434
        112
        dda
        03b
      storyboard
        scene
          component"
    `)
  })

  it('insertion at the beginning', () => {
    const threeViews = lintAndParseAndValidateResult(
      'test.js',
      fileWithTwoDuplicatedViews,
      null,
      emptySet(),
      'trim-bounds',
    )
    const fourViews = lintAndParseAndValidateResult(
      'test.js',
      fileWithTwoDuplicatesAndInsertion,
      asParseSuccessOrNull(threeViews),
      emptySet(),
      'trim-bounds',
    )
    expect(getUidTree(fourViews)).toMatchInlineSnapshot(`
      "4ed
        4e0
      434
        a6c
        112
        dda
        03b
      storyboard
        scene
          component"
    `)
  })

  it('multiple uid changes but the number of elements stays the same', () => {
    const threeViews = lintAndParseAndValidateResult(
      'test.js',
      fileWithTwoDuplicatedViews,
      null,
      emptySet(),
      'trim-bounds',
    )
    const updatedThreeViews = lintAndParseAndValidateResult(
      'test.js',
      fileWithTwoDuplicatedViewsWithChanges,
      asParseSuccessOrNull(threeViews),
      emptySet(),
      'trim-bounds',
    )
    expect(getUidTree(updatedThreeViews)).toEqual(getUidTree(threeViews))
  })

  it('can handle text children fine', () => {
    const start = lintAndParseAndValidateResult(
      'test.js',
      fileWithTwoTextElements,
      null,
      emptySet(),
      'trim-bounds',
    )
    const end = lintAndParseAndValidateResult(
      'test.js',
      fileWithTwoTextElementsWithChanges,
      asParseSuccessOrNull(start),
      emptySet(),
      'trim-bounds',
    )
    expect(getUidTree(end)).toEqual(getUidTree(start))
  })

  it('can handle changes to a parent and child', () => {
    const start = lintAndParseAndValidateResult(
      'test.js',
      baseFileContents,
      null,
      emptySet(),
      'trim-bounds',
    )
    const end = lintAndParseAndValidateResult(
      'test.js',
      fileWithChildAndParentUpdated,
      asParseSuccessOrNull(start),
      emptySet(),
      'trim-bounds',
    )
    expect(getUidTree(end)).toEqual(getUidTree(start))
  })

  it('re-ordered elements should re-order the uid tree', () => {
    const beforeReOrder = lintAndParseAndValidateResult(
      'test.js',
      fileWithOneInsertedView,
      null,
      emptySet(),
      'trim-bounds',
    )
    const afterReOrder = lintAndParseAndValidateResult(
      'test.js',
      fileWithOneInsertedViewReOrdered,
      asParseSuccessOrNull(beforeReOrder),
      emptySet(),
      'trim-bounds',
    )
    expect(getUidTree(beforeReOrder)).toMatchInlineSnapshot(`
      "4ed
        4e0
      434
        c60
        112
      storyboard
        scene
          component"
    `)
    expect(getUidTree(afterReOrder)).toMatchInlineSnapshot(`
      "4ed
        4e0
      434
        112
        c60
      storyboard
        scene
          component"
    `)
  })

  it('uids should match including root element when root element changes', () => {
    const firstResult = lintAndParseAndValidateResult(
      'test.js',
      baseFileContents,
      null,
      emptySet(),
      'trim-bounds',
    )
    const secondResult = lintAndParseAndValidateResult(
      'test.js',
      baseFileContentsWithDifferentBackground,
      asParseSuccessOrNull(firstResult),
      emptySet(),
      'trim-bounds',
    )
    expect(getUidTree(firstResult)).toEqual(getUidTree(secondResult))
  })

  it('handles elements within arbitrary blocks', () => {
    const firstResult = lintAndParseAndValidateResult(
      'test.js',
      fileWithArbitraryBlockInside,
      null,
      emptySet(),
      'trim-bounds',
    )
    const secondResult = lintAndParseAndValidateResult(
      'test.js',
      fileWithSlightlyDifferentArbitraryBlockInside,
      asParseSuccessOrNull(firstResult),
      emptySet(),
      'trim-bounds',
    )
    expect(getUidTree(firstResult)).toEqual(getUidTree(secondResult))
  })

  it('handles newly inserted duplicate elements without duplicating uids', () => {
    const firstResult = lintAndParseAndValidateResult(
      'test.js',
      fileWithBasicDiv,
      null,
      emptySet(),
      'trim-bounds',
    )
    expect(getUidTree(firstResult)).toMatchInlineSnapshot(`
      "4ed
        4e0
      434
        c85
          f9b
      storyboard
        scene
          component"
    `)
    const secondResult = lintAndParseAndValidateResult(
      'test.js',
      fileWithBasicDivDuplicated,
      asParseSuccessOrNull(firstResult),
      emptySet(),
      'trim-bounds',
    )
    expect(getUidTree(secondResult)).toMatchInlineSnapshot(`
      "4ed
        4e0
      434
        c85
          aab
            f9b
      storyboard
        scene
          component"
    `)
  })
})

function hasNoDuplicateUIDs(expression: JSXElementChild): boolean {
  const uniqueIDsResult = getAllUniqueUIdsFromElementChild(expression)
  return Object.keys(uniqueIDsResult.duplicateIDs).length === 0
}

function checkUIDValues([first, second]: [JSXElementChild, JSXElementChild]): boolean {
  const uidsOfSecond = getAllUniqueUIdsFromElementChild(second).allIDs
  // Check:
  // - first has no internal duplicates.
  // - second has no internal duplicates.
  // - first doesn't have a uid which is within the second value.
  return (
    hasNoDuplicateUIDs(first) && hasNoDuplicateUIDs(second) && !uidsOfSecond.includes(first.uid)
  )
}

describe('fixExpressionUIDs', () => {
  it('uids will be copied over', () => {
    // If an entry has duplicated UIDs, then it will result in a differing output as the logic attempts to ensure
    // the UIDs are unique.
    const arbitraryExpressionPair: Arbitrary<{ first: JSExpression; second: JSExpression }> =
      FastCheck.tuple(jsxAttributeArbitrary(2), jsxAttributeArbitrary(2))
        .filter(checkUIDValues)
        .map(([first, second]) => {
          return {
            first: first,
            second: second,
          }
        })
    function checkCall(value: { first: JSExpression; second: JSExpression }): boolean {
      const { first, second } = value
      const afterFix = fixExpressionUIDs(first, second, {
        mutableAllNewUIDs: emptySet(),
        uidsExpectedToBeSeen: emptySet(),
        mappings: [],
        uidUpdateMethod: 'copy-uids-fix-duplicates',
      })
      return first.uid === afterFix.uid
    }
    const prop = FastCheck.property(arbitraryExpressionPair, checkCall)
    FastCheck.assert(prop, { verbose: true })
  })
})

describe('fixJSXElementChildUIDs', () => {
  it('handles the case where an element gets wrapped with an expression', () => {
    const before = jsxElement('div', 'div-uid', [], [])
    const after = jsExpressionOtherJavaScript(
      'something',
      'return something',
      [],
      null,
      {
        'div-uid': before,
      },
      'expression-uid',
    )
    const fixUIDsState: FixUIDsState = {
      mutableAllNewUIDs: emptySet(),
      uidsExpectedToBeSeen: new Set('div-uid'),
      mappings: [],
      uidUpdateMethod: 'copy-uids-fix-duplicates',
    }

    const actualResult = fixJSXElementChildUIDs(before, after, fixUIDsState)
    expect(actualResult.uid).not.toEqual('div-uid')
  })
  it('uids will be copied over', () => {
    // If an entry has duplicated UIDs, then it will result in a differing output as the logic attempts to ensure
    // the UIDs are unique.
    const arbitraryElementPair: Arbitrary<{ first: JSXElementChild; second: JSXElementChild }> =
      FastCheck.tuple(jsxElementChildArbitrary(3), jsxElementChildArbitrary(3))
        .filter(checkUIDValues)
        .map(([first, second]) => {
          return {
            first: first,
            second: second,
          }
        })
    function checkCall(value: { first: JSXElementChild; second: JSXElementChild }): boolean {
      const { first, second } = value
      const afterFix = fixJSXElementChildUIDs(first, second, {
        mutableAllNewUIDs: emptySet(),
        uidsExpectedToBeSeen: emptySet(),
        mappings: [],
        uidUpdateMethod: 'copy-uids-fix-duplicates',
      })
      return first.uid === afterFix.uid
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

const baseFile = lintAndParseAndValidateResult(
  'test.js',
  baseFileContents,
  null,
  emptySet(),
  'trim-bounds',
)

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
