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
  shouldTrimBounds: 'trim-bounds' | 'do-not-trim-bounds',
): ParsedTextFile {
  const result = lintAndParse(
    filename,
    content,
    oldParseResultForUIDComparison,
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
      'trim-bounds',
    )
    expect(getUidTree(initialParse)).toMatchInlineSnapshot(`
      "f818c1747811f04164db770536eadb13
        666b46bd6a197a3c8dde290c1f81a0d5
          1f5d9db64d29025d6f586c44baf43361
        a3c2e3b0fe3cd535c92ab32d9d260f98
          00bc6a1106aa8c2f6b90ca156135858c"
    `)
    if (isParseSuccess(initialParse)) {
      const secondParse = lintAndParseAndValidateResult(
        'test.js',
        duplicateDataUIDPropUIDTestCaseAdditionalComponent,
        initialParse,
        'trim-bounds',
      )
      expect(getUidTree(secondParse)).toMatchInlineSnapshot(`
        "3ec2b351bf8ef48de43dfe9b7506aaba
          afb38a4e9c1c887de3cbf376e2c42c49
        63856ee4499cc5352a7e50832e6ac613
          acdc3f52cb06acdfc50dddc5ce79a13a
            59b7172983602914220c3a60fce5d43b
          110836f7f1d3e94d6aa748544f370d49
            7b10c81701a735c6840abda77635c91f"
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
      'trim-bounds',
    )
    expect(getUidTree(newFile)).toEqual(getUidTree(baseFile))
    expect(getUidTree(newFile)).toMatchInlineSnapshot(`
      "d3f6697961d39c36f550b6db4bf831a0
        e6979c4f851f9bb73f3d18f23bd744df
      81db842c7281e3f3e188e9932513466b
        62ebfdd49503b752ffda2bf06ca75a82
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
      'trim-bounds',
    )
    const newFileFixed = lintAndParseAndValidateResult(
      'test.js',
      baseFileWithTwoTopLevelComponentsUpdated,
      asParseSuccessOrNull(newFile),
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
      'trim-bounds',
    )
    const fileWithFragmentUpdated = lintAndParseAndValidateResult(
      'test.js',
      fileWithTopLevelFragmentComponentUpdateContents,
      asParseSuccessOrNull(fileWithFragment),
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
    const base = lintAndParseAndValidateResult('test.js', baseTernary, null, 'trim-bounds')
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
      'trim-bounds',
    )
    expect(getUidTree(newFile)).toEqual(getUidTree(base))
  })

  it('avoids uid shifting caused by single prepending insertion', () => {
    const newFile = lintAndParseAndValidateResult(
      'test.js',
      fileWithOneInsertedView,
      asParseSuccessOrNull(baseFile),
      'trim-bounds',
    )
    expect(getUidTree(newFile)).toMatchInlineSnapshot(`
      "d3f6697961d39c36f550b6db4bf831a0
        e6979c4f851f9bb73f3d18f23bd744df
      245874f6b6a7e43a0a1341b02080c965
        f8f3c2f7a28e391a504ff3625a26f020
        916ff4f1ff9c661f48487151d49ad27d
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
      'trim-bounds',
    )
    expect(getUidTree(baseFileWithDuplicateViews)).toEqual(getUidTree(newFile))
  })

  it('double duplication', () => {
    const newFile = lintAndParseAndValidateResult(
      'test.js',
      fileWithTwoDuplicatedViews,
      asParseSuccessOrNull(baseFile),
      'trim-bounds',
    )
    expect(getUidTree(newFile)).toMatchInlineSnapshot(`
      "d3f6697961d39c36f550b6db4bf831a0
        e6979c4f851f9bb73f3d18f23bd744df
      d999cb792d68571ad924f6be3070c835
        62ebfdd49503b752ffda2bf06ca75a82
        916ff4f1ff9c661f48487151d49ad27d
        c7ed5d4d4ba469fef1c97504b3b4892c
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
      'trim-bounds',
    )
    const fourViews = lintAndParseAndValidateResult(
      'test.js',
      fileWithTwoDuplicatesAndInsertion,
      asParseSuccessOrNull(threeViews),
      'trim-bounds',
    )
    expect(getUidTree(fourViews)).toMatchInlineSnapshot(`
      "d3f6697961d39c36f550b6db4bf831a0
        e6979c4f851f9bb73f3d18f23bd744df
      d534698f4e870899e562d583f70f54a9
        d82d7028b26d305ec06b7087c14648ff
        916ff4f1ff9c661f48487151d49ad27d
        c7ed5d4d4ba469fef1c97504b3b4892c
        1476324b20317e4b2e8a4f9c6f324f0b
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
      'trim-bounds',
    )
    const updatedThreeViews = lintAndParseAndValidateResult(
      'test.js',
      fileWithTwoDuplicatedViewsWithChanges,
      asParseSuccessOrNull(threeViews),
      'trim-bounds',
    )
    expect(getUidTree(updatedThreeViews)).toEqual(getUidTree(threeViews))
  })

  it('can handle text children fine', () => {
    const start = lintAndParseAndValidateResult(
      'test.js',
      fileWithTwoTextElements,
      null,
      'trim-bounds',
    )
    const end = lintAndParseAndValidateResult(
      'test.js',
      fileWithTwoTextElementsWithChanges,
      asParseSuccessOrNull(start),
      'trim-bounds',
    )
    expect(getUidTree(end)).toEqual(getUidTree(start))
  })

  it('can handle changes to a parent and child', () => {
    const start = lintAndParseAndValidateResult('test.js', baseFileContents, null, 'trim-bounds')
    const end = lintAndParseAndValidateResult(
      'test.js',
      fileWithChildAndParentUpdated,
      asParseSuccessOrNull(start),
      'trim-bounds',
    )
    expect(getUidTree(end)).toEqual(getUidTree(start))
  })

  it('re-ordered elements should re-order the uid tree', () => {
    const beforeReOrder = lintAndParseAndValidateResult(
      'test.js',
      fileWithOneInsertedView,
      null,
      'trim-bounds',
    )
    const afterReOrder = lintAndParseAndValidateResult(
      'test.js',
      fileWithOneInsertedViewReOrdered,
      asParseSuccessOrNull(beforeReOrder),
      'trim-bounds',
    )
    expect(getUidTree(beforeReOrder)).toMatchInlineSnapshot(`
      "d3f6697961d39c36f550b6db4bf831a0
        e6979c4f851f9bb73f3d18f23bd744df
      245874f6b6a7e43a0a1341b02080c965
        f8f3c2f7a28e391a504ff3625a26f020
        916ff4f1ff9c661f48487151d49ad27d
      storyboard
        scene
          component"
    `)
    expect(getUidTree(afterReOrder)).toMatchInlineSnapshot(`
      "d3f6697961d39c36f550b6db4bf831a0
        e6979c4f851f9bb73f3d18f23bd744df
      245874f6b6a7e43a0a1341b02080c965
        62ebfdd49503b752ffda2bf06ca75a82
        b237498c6a4cbc45a5ab07487204fe1e
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
      'trim-bounds',
    )
    const secondResult = lintAndParseAndValidateResult(
      'test.js',
      baseFileContentsWithDifferentBackground,
      asParseSuccessOrNull(firstResult),
      'trim-bounds',
    )
    expect(getUidTree(firstResult)).toEqual(getUidTree(secondResult))
  })

  it('handles elements within arbitrary blocks', () => {
    const firstResult = lintAndParseAndValidateResult(
      'test.js',
      fileWithArbitraryBlockInside,
      null,
      'trim-bounds',
    )
    const secondResult = lintAndParseAndValidateResult(
      'test.js',
      fileWithSlightlyDifferentArbitraryBlockInside,
      asParseSuccessOrNull(firstResult),
      'trim-bounds',
    )
    expect(getUidTree(firstResult)).toEqual(getUidTree(secondResult))
  })

  it('handles newly inserted duplicate elements without duplicating uids', () => {
    const firstResult = lintAndParseAndValidateResult(
      'test.js',
      fileWithBasicDiv,
      null,
      'trim-bounds',
    )
    expect(getUidTree(firstResult)).toMatchInlineSnapshot(`
      "d3f6697961d39c36f550b6db4bf831a0
        e6979c4f851f9bb73f3d18f23bd744df
      8a8b1b1214c62918cea2e3255bebf817
        f98f0baa974e60085117f5dd4fe2ebb5
          735fabee61300aa803b4bd92d0a5a0c1
      storyboard
        scene
          component"
    `)
    const secondResult = lintAndParseAndValidateResult(
      'test.js',
      fileWithBasicDivDuplicated,
      asParseSuccessOrNull(firstResult),
      'trim-bounds',
    )
    expect(getUidTree(secondResult)).toMatchInlineSnapshot(`
      "d3f6697961d39c36f550b6db4bf831a0
        e6979c4f851f9bb73f3d18f23bd744df
      8a8b1b1214c62918cea2e3255bebf817
        622604d00aefec5a1c6e03d06dd16324
          12adfff650ec7ea0528b2bf2a8fbf69a
            b5f2b6ec5ec45c0a19ecfa4718847d4e
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
      'trim-bounds',
    )
    expect(getUidTree(firstResult)).toMatchInlineSnapshot(`
      "d0fe908deffd8963ade2563a5228c782
        b0124c71ae85fc9ed37d761091c42d14
          3647ad2e40dab7862fbb7b363df4813e
          c8540c083c7f9a7a2fdac69c67441b16"
    `)
    const secondResult = lintAndParseAndValidateResult(
      'test.js',
      commentedOutJSXExpression,
      asParseSuccessOrNull(firstResult),
      'trim-bounds',
    )
    expect(getUidTree(secondResult)).toMatchInlineSnapshot(`
      "d0fe908deffd8963ade2563a5228c782
        b0124c71ae85fc9ed37d761091c42d14
          c1a64df560a1fe659d2a25ccbbaa3f06"
    `)
  })
  it(`handles arbitrary JS blocks and their UIDs`, () => {
    const parsedResult = lintAndParseAndValidateResult(
      'test.js',
      fileWithArbitraryBlockInside,
      null,
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
    const fixedResult = fixParseSuccessUIDs(null, fudgedResult, new Set())
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

const baseFile = lintAndParseAndValidateResult('test.js', baseFileContents, null, 'trim-bounds')

const baseFileWithDuplicateViews = lintAndParseAndValidateResult(
  'test.js',
  baseFileWithDuplicatedViewsContents,
  null,
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
