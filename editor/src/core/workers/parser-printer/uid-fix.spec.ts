/* eslint jest/expect-expect: ["error", { "assertFunctionNames": ["expect", "FastCheck.assert"] }] */
import { getUtopiaID, parseUID } from '../../../core/shared/uid-utils'
import { getComponentsFromTopLevelElements } from '../../model/project-file-utils'
import * as FastCheck from 'fast-check'
import type {
  JSExpression,
  JSXElement,
  JSXElementChild,
  JSXMapExpression,
  TopLevelElement,
} from '../../shared/element-template'
import {
  arbitraryJSBlock,
  isUtopiaJSXComponent,
  jsExpressionValue,
  jsxAttributesFromMap,
  jsxMapExpression,
} from '../../shared/element-template'
import { unparsedCode } from '../../shared/element-template'
import {
  emptyComments,
  isJSXElement,
  isJSXElementLike,
  isJSXFragment,
  jsExpressionOtherJavaScript,
  jsxElement,
  walkElements,
} from '../../shared/element-template'
import type { ParsedTextFile, ParseSuccess } from '../../shared/project-file-types'
import {
  isParseSuccess,
  RevisionsState,
  textFile,
  textFileContents,
} from '../../shared/project-file-types'
import { emptySet, intersection } from '../../shared/set-utils'
import { isEmptyObject } from '../../shared/object-utils'
import { lintAndParse } from './parser-printer'
import {
  elementsStructure,
  jsxAttributeArbitrary,
  jsxElementChildArbitrary,
} from './parser-printer.test-utils'
import type { Arbitrary } from 'fast-check'
import type { FixUIDsState } from './uid-fix'
import {
  fixElementsWithin,
  fixExpressionUIDs,
  fixJSXElementChildUIDs,
  fixParseSuccessUIDs,
  fixUIDsInJavascriptStrings,
} from './uid-fix'
import { foldEither } from '../../../core/shared/either'
import {
  getAllUniqueUids,
  getAllUniqueUIdsFromElementChild,
} from '../../../core/model/get-unique-ids'
import { contentsToTree } from '../../../components/assets'
import {
  filtered,
  fromArrayIndex,
  fromField,
  fromTypeGuard,
  notNull,
  traverseArray,
} from '../../../core/shared/optics/optic-creators'
import { modify, set, unsafeGet } from '../../../core/shared/optics/optic-utilities'

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
    'do-not-apply-steganography',
  )
  validateParsedTextFileElementUIDs(result)
  validateHighlightBoundsForUIDs(result)
  const projectContents = contentsToTree({
    ['test.js']: textFile(
      textFileContents(content, result, RevisionsState.BothMatch),
      null,
      oldParseResultForUIDComparison,
      0,
    ),
  })
  const duplicateUIDs = getAllUniqueUids(projectContents).duplicateIDs
  if (Object.keys(duplicateUIDs).length > 0) {
    throw new Error(`Duplicate UIDs identified ${JSON.stringify(duplicateUIDs)}`)
  }
  return result
}

describe('fixParseSuccessUIDs', () => {
  it('handles some repositioning of entities', () => {
    const modifiedBaseFile = modify(
      fromTypeGuard(isParseSuccess).compose(fromField('topLevelElements')),
      (topLevelElements) => {
        const updatedTopLevelElements = modify(
          traverseArray<TopLevelElement>()
            .compose(fromTypeGuard(isUtopiaJSXComponent))
            .compose(fromField('rootElement'))
            .compose(fromField('uid')),
          (uid) => {
            return uid + 'suffix'
          },
          topLevelElements,
        )
        return [
          unparsedCode('// Some nonsense that should do nothing.'),
          ...updatedTopLevelElements,
        ]
      },
      baseFile,
    )
    const parsedFile = lintAndParseAndValidateResult(
      'test.js',
      baseFileContents,
      asParseSuccessOrNull(modifiedBaseFile),
      emptySet(),
      'trim-bounds',
    )
    expect(getUidTree(parsedFile)).toEqual(getUidTree(modifiedBaseFile))
    expect(getUidTree(parsedFile)).toMatchInlineSnapshot(`
      "d3fsuffix
        f9a
      81dsuffix
        62e
      storyboardsuffix
        scene
          component"
    `)
  })
  it('handles a newly inserted component at the start of the file', () => {
    const initialParse = lintAndParseAndValidateResult(
      'test.js',
      duplicateDataUIDPropUIDBaseTestCase,
      null,
      emptySet(),
      'trim-bounds',
    )
    expect(getUidTree(initialParse)).toMatchInlineSnapshot(`
      "f81
        666
          1f5
        a3c
          00b"
    `)
    if (isParseSuccess(initialParse)) {
      const secondParse = lintAndParseAndValidateResult(
        'test.js',
        duplicateDataUIDPropUIDTestCaseAdditionalComponent,
        initialParse,
        emptySet(),
        'trim-bounds',
      )
      expect(getUidTree(secondParse)).toMatchInlineSnapshot(`
        "f81
          666
        638
          acd
            59b
          110
            7b1"
      `)
    } else {
      throw new Error(`First parse did not succeed.`)
    }
  })
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
      "d3f
        f9a
      81d
        62e
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
      "d3f
        f9a
      468
        f8f
      475
        bbc
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
      "d3f
        f9a
      d25
        83b
        39d
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
      "d3f
        f9a
      81d
        62e
      storyboard
        scene
          component"
    `)
  })

  it('finds and fixes a component inside a code block', () => {
    const baseTernary = createFileText(baseTernaryText)
    const base = lintAndParseAndValidateResult(
      'test.js',
      baseTernary,
      null,
      emptySet(),
      'trim-bounds',
    )
    const baseTernaryChangedText = baseTernaryText.replace(
      /<Money /g,
      `<Money withoutCurrency={false}`,
    )
    expect(baseTernaryChangedText).not.toEqual(baseTernaryText)
    const baseTernaryChanged = createFileText(baseTernaryChangedText)
    const newFile = lintAndParseAndValidateResult(
      'test.js',
      baseTernaryChanged,
      asParseSuccessOrNull(base),
      emptySet(),
      'trim-bounds',
    )
    expect(getUidTree(newFile)).toEqual(getUidTree(base))
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
      "d3f
        f9a
      81d
        62e
        916
      storyboard
        scene
          component"
    `)
  })

  it('avoids uid shifting caused by first element being updated', () => {
    const newFile = lintAndParseAndValidateResult(
      'test.js',
      fileWithFistDuplicatedViewUpdate,
      asParseSuccessOrNull(baseFileWithDuplicateViews),
      emptySet(),
      'trim-bounds',
    )
    expect(getUidTree(baseFileWithDuplicateViews)).toEqual(getUidTree(newFile))
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
      "d3f
        f9a
      81d
        62e
        916
        c7e
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
      "d3f
        f9a
      d99
        62e
        916
        c7e
        147
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
      "d3f
        f9a
      245
        f8f
        916
      storyboard
        scene
          component"
    `)
    expect(getUidTree(afterReOrder)).toMatchInlineSnapshot(`
      "d3f
        f9a
      245
        f8f
        916
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
      "d3f
        f9a
      8a8
        f98
          3cc
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
      "d3f
        f9a
      8a8
        f98
          758
            3cc
      storyboard
        scene
          component"
    `)
  })
  it(`handles an entirely commented out JSX expression which was not previously commented out`, () => {
    const firstResult = lintAndParseAndValidateResult(
      'test.js',
      uncommentedJSXExpression,
      null,
      emptySet(),
      'trim-bounds',
    )
    expect(getUidTree(firstResult)).toMatchInlineSnapshot(`
      "d0f
        b01
          364
          c85"
    `)
    const secondResult = lintAndParseAndValidateResult(
      'test.js',
      commentedOutJSXExpression,
      asParseSuccessOrNull(firstResult),
      emptySet(),
      'trim-bounds',
    )
    expect(getUidTree(secondResult)).toMatchInlineSnapshot(`
      "d0f
        b01
          364"
    `)
  })
  it(`handles arbitrary JS blocks and their UIDs`, () => {
    const parsedResult = lintAndParseAndValidateResult(
      'test.js',
      fileWithArbitraryBlockInside,
      null,
      emptySet(),
      'trim-bounds',
    )
    const toRootElementOptic = fromTypeGuard(isParseSuccess)
      .compose(fromField('topLevelElements'))
      .compose(traverseArray())
      .compose(filtered(isUtopiaJSXComponent))
      .compose(fromTypeGuard(isUtopiaJSXComponent))
      .compose(fromField('rootElement'))
      .compose(fromField('uid'))
    const toArbitraryBlockOptic = fromTypeGuard(isParseSuccess)
      .compose(fromField('topLevelElements'))
      .compose(traverseArray())
      .compose(filtered(isUtopiaJSXComponent))
      .compose(fromTypeGuard(isUtopiaJSXComponent))
      .compose(fromField('arbitraryJSBlock'))
      .compose(notNull())
      .compose(fromField('uid'))
    const uidFromElement = unsafeGet(toRootElementOptic, parsedResult)
    const fudgedResult = set(toArbitraryBlockOptic, uidFromElement, parsedResult)
    const fixedResult = fixParseSuccessUIDs(null, fudgedResult, new Set(), new Set())
    const projectContents = contentsToTree({
      ['test.js']: textFile(
        textFileContents(fileWithArbitraryBlockInside, fixedResult, RevisionsState.BothMatch),
        null,
        null,
        0,
      ),
    })
    const duplicateUIDs = getAllUniqueUids(projectContents).duplicateIDs
    expect(Object.keys(duplicateUIDs)).toHaveLength(0)
  })
})

function checkUIDValues([first, second]: [JSXElementChild, JSXElementChild]): boolean {
  const firstUIDsResult = getAllUniqueUIdsFromElementChild(first)
  const secondUIDsResult = getAllUniqueUIdsFromElementChild(second)
  // Check:
  // - first has no internal duplicates.
  // - second has no internal duplicates.
  // - first doesn't have a uid which is within the second value.
  return (
    isEmptyObject(firstUIDsResult.duplicateIDs) &&
    isEmptyObject(secondUIDsResult.duplicateIDs) &&
    intersection([new Set(firstUIDsResult.allIDs), new Set(secondUIDsResult.allIDs)]).size === 0
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
      [],
      'something',
      'something',
      'return something',
      [],
      null,
      {
        'div-uid': before,
      },
      emptyComments,
      'expression-uid',
    )
    const fixUIDsState: FixUIDsState = {
      mutableAllNewUIDs: emptySet(),
      uidsExpectedToBeSeen: new Set(['div-uid']),
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

const baseFileWithDuplicatedViewsContents = createFileText(`
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
    </div>
  )
}
`)

const uncommentedJSXExpression = `import * as React from 'react'
import { Scene, Storyboard } from 'utopia-api'
import { App } from '/src/app.js'
import { Playground } from '/src/playground.js'

export var storyboard = (
  <Storyboard>
    <div
      style={{
        backgroundColor: '#aaaaaa33',
        position: 'absolute',
        left: 209,
        top: 99,
        width: 340,
        height: 338,
      }}
    >
      <div
        style={{
          backgroundColor: '#aaaaaa33',
          position: 'absolute',
          left: 80,
          top: 60,
          width: 104,
          height: 95,
        }}
      />
      <div
        style={{
          backgroundColor: '#aaaaaa33',
          position: 'absolute',
          left: 128,
          top: 205,
          width: 96,
          height: 80,
        }}
      />
    </div>
  </Storyboard>
)
`
const commentedOutJSXExpression = `import * as React from 'react'
import { Scene, Storyboard } from 'utopia-api'
import { App } from '/src/app.js'
import { Playground } from '/src/playground.js'

export var storyboard = (
  <Storyboard>
    <div
      style={{
        backgroundColor: '#aaaaaa33',
        position: 'absolute',
        left: 209,
        top: 99,
        width: 340,
        height: 338,
      }}
    >
      {/* <div
        style={{
          backgroundColor: '#aaaaaa33',
          position: 'absolute',
          left: 80,
          top: 60,
          width: 104,
          height: 95,
        }}
      />
      <div
        style={{
          backgroundColor: '#aaaaaa33',
          position: 'absolute',
          left: 128,
          top: 205,
          width: 96,
          height: 80,
        }}
      /> */}
    </div>
  </Storyboard>
)
`

const duplicateDataUIDPropUIDBaseTestCase = `import * as React from 'react'
import { Scene, Storyboard } from 'utopia-api'
import { App } from '/src/app.js'
import { Playground } from '/src/playground.js'

export var storyboard = (
  <Storyboard>
    <Scene
      style={{
        width: 700,
        height: 759,
        position: 'absolute',
        left: 212,
        top: 128,
      }}
      data-label='Playground'
    >
      <Playground style={{}} />
    </Scene>
    <Scene
      style={{
        width: 744,
        height: 1133,
        position: 'absolute',
        left: 1036,
        top: 128,
      }}
      data-label='My App'
    >
      <App style={{}} />
    </Scene>
  </Storyboard>
)
`
const duplicateDataUIDPropUIDTestCaseAdditionalComponent = `import * as React from 'react'
import { Scene, Storyboard } from 'utopia-api'
import { App } from '/src/app.js'
import { Playground } from '/src/playground.js'

const ThisComponent = (props) => (
  <div>
    <div />
  </div>
)

export var storyboard = (
  <Storyboard>
    <Scene
      style={{
        width: 700,
        height: 759,
        position: 'absolute',
        left: 212,
        top: 128,
      }}
      data-label='Playground'
    >
      <Playground style={{}} />
    </Scene>
    <Scene
      style={{
        width: 744,
        height: 1133,
        position: 'absolute',
        left: 1036,
        top: 128,
      }}
      data-label='My App'
    >
      <App style={{}} />
    </Scene>
  </Storyboard>
)
`

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

const baseTernaryText = `
return (
  <div className='product-price'>
    {<Money />}
  </div>
)
`

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

const fileWithFistDuplicatedViewUpdate = createFileText(`
export var SameFileApp = (props) => {
  return (
    <div
      style={{ position: 'relative', width: '100%', height: '100%', backgroundColor: '#FFFFFF' }}
    >
      <View
        style={{
          width: 191,
          height: 200
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
      {[1, 2, 3].map((index) => (
      <View
        style={{
          width: 191 + index,
        }}
      />
      ))}
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
      {[1, 2, 3].map((index) => (
      <View
        style={{
          width: 161 + index,
        }}
      />
      ))}
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

const baseFileWithDuplicateViews = lintAndParseAndValidateResult(
  'test.js',
  baseFileWithDuplicatedViewsContents,
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
