import {
  emptyComments,
  jsxArraySpread,
  jsxArrayValue,
  jsExpressionFunctionCall,
  jsExpressionNestedArray,
  jsExpressionNestedObject,
  JSExpressionOtherJavaScript,
  jsExpressionOtherJavaScript,
  jsxAttributesEntry,
  jsxAttributesSpread,
  jsExpressionValue,
  JSExpressionValue,
  jsxElement,
  jsxPropertyAssignment,
  jsxSpreadAssignment,
  MultiLineComment,
  ParsedComments,
  SingleLineComment,
  utopiaJSXComponent,
} from '../../../core/shared/element-template'
import * as EP from '../../../core/shared/element-path'
import { emptyImports } from '../../../core/workers/common/project-file-utils'
import { addToComplexMap, emptyComplexMap } from '../../../utils/map'
import {
  defaultElementWarnings,
  DerivedState,
  regularNavigatorEntry,
  transientCanvasState,
  TransientCanvasState,
  transientFileState,
} from './editor-state'
import {
  CommentKeepDeepEqualityCall,
  DerivedStateKeepDeepEquality,
  JSXArrayElementKeepDeepEqualityCall,
  JSXArraySpreadKeepDeepEqualityCall,
  JSXArrayValueKeepDeepEqualityCall,
  JSXAttributeFunctionCallKeepDeepEqualityCall,
  JSXAttributeKeepDeepEqualityCall,
  JSXAttributeNestedArrayKeepDeepEqualityCall,
  JSXAttributeNestedObjectKeepDeepEqualityCall,
  JSXAttributeOtherJavaScriptKeepDeepEqualityCall,
  JSXAttributesEntryDeepEqualityCall,
  JSXAttributesKeepDeepEqualityCall,
  JSXAttributesPartDeepEqualityCall,
  JSXAttributesSpreadDeepEqualityCall,
  JSXAttributeValueKeepDeepEqualityCall,
  JSXPropertyAssignmentKeepDeepEqualityCall,
  JSXPropertyKeepDeepEqualityCall,
  JSXSpreadAssignmentKeepDeepEqualityCall,
  MultiLineCommentKeepDeepEqualityCall,
  ParsedCommentsKeepDeepEqualityCall,
  SingleLineCommentKeepDeepEqualityCall,
  TransientCanvasStateKeepDeepEquality,
} from './store-deep-equality-instances'

describe('TransientCanvasStateKeepDeepEquality', () => {
  const oldValue: TransientCanvasState = transientCanvasState(
    [EP.elementPath([['scene'], ['aaa', 'bbb']])],
    [EP.elementPath([['scene'], ['aaa', 'ccc']])],
    [EP.elementPath([['scene'], ['aaa', 'ccc']])],
    {
      ['/utopia/app.js']: transientFileState(
        [
          utopiaJSXComponent(
            'App',
            false,
            'var',
            'block',
            null,
            [],
            jsxElement('div', 'eee', [], []),
            null,
            false,
            emptyComments,
          ),
        ],
        emptyImports(),
      ),
    },
    [],
  )
  const newSameValue: TransientCanvasState = transientCanvasState(
    [EP.elementPath([['scene'], ['aaa', 'bbb']])],
    [EP.elementPath([['scene'], ['aaa', 'ccc']])],
    [EP.elementPath([['scene'], ['aaa', 'ccc']])],
    {
      ['/utopia/app.js']: transientFileState(
        [
          utopiaJSXComponent(
            'App',
            false,
            'var',
            'block',
            null,
            [],
            jsxElement('div', 'eee', [], []),
            null,
            false,
            emptyComments,
          ),
        ],
        emptyImports(),
      ),
    },
    [],
  )
  const newDifferentValue: TransientCanvasState = transientCanvasState(
    [EP.elementPath([['scene'], ['aaa', 'ddd']])],
    [EP.elementPath([['scene'], ['aaa', 'ccc']])],
    [EP.elementPath([['scene'], ['aaa', 'ccc']])],
    {
      ['/utopia/app.js']: transientFileState(
        [
          utopiaJSXComponent(
            'App',
            false,
            'var',
            'block',
            null,
            [],
            jsxElement('div', 'eee', [], []),
            null,
            false,
            emptyComments,
          ),
        ],
        emptyImports(),
      ),
    },
    [],
  )

  it('same reference returns the same reference', () => {
    const result = TransientCanvasStateKeepDeepEquality()(oldValue, oldValue)
    expect(result.value).toBe(oldValue)
    expect(result.areEqual).toEqual(true)
  })
  it('same value returns the same reference', () => {
    const result = TransientCanvasStateKeepDeepEquality()(oldValue, newSameValue)
    expect(result.value).toBe(oldValue)
    expect(result.areEqual).toEqual(true)
  })
  it('different but similar value handled appropriately', () => {
    const result = TransientCanvasStateKeepDeepEquality()(oldValue, newDifferentValue)
    expect(result.value.selectedViews![0].parts).toBe(newDifferentValue.selectedViews![0].parts)
    expect(result.value.highlightedViews).toBe(oldValue.highlightedViews)
    expect(result.value.filesState).toBe(oldValue.filesState)
    expect(result.value).toEqual(newDifferentValue)
    expect(result.areEqual).toEqual(false)
  })
})

describe('DerivedStateKeepDeepEquality', () => {
  const oldValue: DerivedState = {
    navigatorTargets: [regularNavigatorEntry(EP.elementPath([['scene'], ['aaa', 'bbb']]))],
    visibleNavigatorTargets: [regularNavigatorEntry(EP.elementPath([['scene'], ['aaa', 'bbb']]))],
    controls: [],
    transientState: transientCanvasState(
      [EP.elementPath([['scene'], ['aaa', 'bbb']])],
      [EP.elementPath([['scene'], ['aaa', 'ccc']])],
      [EP.elementPath([['scene'], ['aaa', 'ccc']])],
      {
        ['/utopia/app.js']: transientFileState(
          [
            utopiaJSXComponent(
              'App',
              false,
              'var',
              'block',
              null,
              [],
              jsxElement('div', 'eee', [], []),
              null,
              false,
              emptyComments,
            ),
          ],
          emptyImports(),
        ),
      },
      [],
    ),
    elementWarnings: addToComplexMap(
      EP.toString,
      emptyComplexMap(),
      EP.elementPath([['scene'], ['aaa', 'bbb']]),
      defaultElementWarnings,
    ),
  }
  const newSameValue: DerivedState = {
    navigatorTargets: [regularNavigatorEntry(EP.elementPath([['scene'], ['aaa', 'bbb']]))],
    visibleNavigatorTargets: [regularNavigatorEntry(EP.elementPath([['scene'], ['aaa', 'bbb']]))],
    controls: [],
    transientState: transientCanvasState(
      [EP.elementPath([['scene'], ['aaa', 'bbb']])],
      [EP.elementPath([['scene'], ['aaa', 'ccc']])],
      [EP.elementPath([['scene'], ['aaa', 'ccc']])],
      {
        ['/utopia/app.js']: transientFileState(
          [
            utopiaJSXComponent(
              'App',
              false,
              'var',
              'block',
              null,
              [],
              jsxElement('div', 'eee', [], []),
              null,
              false,
              emptyComments,
            ),
          ],
          emptyImports(),
        ),
      },
      [],
    ),
    elementWarnings: addToComplexMap(
      EP.toString,
      emptyComplexMap(),
      EP.elementPath([['scene'], ['aaa', 'bbb']]),
      defaultElementWarnings,
    ),
  }
  const newDifferentValue: DerivedState = {
    navigatorTargets: [regularNavigatorEntry(EP.elementPath([['scene'], ['aaa', 'ddd']]))],
    visibleNavigatorTargets: [regularNavigatorEntry(EP.elementPath([['scene'], ['aaa', 'bbb']]))],
    controls: [],
    transientState: transientCanvasState(
      [EP.elementPath([['scene'], ['aaa', 'bbb']])],
      [EP.elementPath([['scene'], ['aaa', 'ccc']])],
      [EP.elementPath([['scene'], ['aaa', 'ccc']])],
      {
        ['/utopia/app.js']: transientFileState(
          [
            utopiaJSXComponent(
              'App',
              false,
              'var',
              'block',
              null,
              [],
              jsxElement('div', 'eee', [], []),
              null,
              false,
              emptyComments,
            ),
          ],
          emptyImports(),
        ),
      },
      [],
    ),
    elementWarnings: addToComplexMap(
      EP.toString,
      emptyComplexMap(),
      EP.elementPath([['scene'], ['aaa', 'bbb']]),
      defaultElementWarnings,
    ),
  }
  it('same reference returns the same reference', () => {
    const result = DerivedStateKeepDeepEquality()(oldValue, oldValue)
    expect(result.value).toBe(oldValue)
    expect(result.areEqual).toEqual(true)
  })
  it('same value returns the same reference', () => {
    const result = DerivedStateKeepDeepEquality()(oldValue, newSameValue)
    expect(result.value).toBe(oldValue)
    expect(result.areEqual).toEqual(true)
  })
  it('different but similar value handled appropriately', () => {
    const result = DerivedStateKeepDeepEquality()(oldValue, newDifferentValue)
    expect(result.value.navigatorTargets[0].elementPath).toBe(
      newDifferentValue.navigatorTargets[0].elementPath,
    )
    expect(result.value.visibleNavigatorTargets).toBe(oldValue.visibleNavigatorTargets)
    expect(result.value.controls).toBe(oldValue.controls)
    expect(result.value.transientState).toBe(oldValue.transientState)
    expect(result.value.elementWarnings).toBe(oldValue.elementWarnings)
    expect(result.value).toEqual(newDifferentValue)
    expect(result.areEqual).toEqual(false)
  })
})

describe('MultiLineCommentKeepDeepEqualityCall', () => {
  const oldValue: MultiLineComment = {
    type: 'MULTI_LINE_COMMENT',
    comment: 'A comment',
    rawText: '/* A comment */',
    trailingNewLine: false,
    pos: 10,
  }
  const newSameValue: MultiLineComment = {
    type: 'MULTI_LINE_COMMENT',
    comment: 'A comment',
    rawText: '/* A comment */',
    trailingNewLine: false,
    pos: 10,
  }
  const newDifferentValue: MultiLineComment = {
    type: 'MULTI_LINE_COMMENT',
    comment: 'A comment',
    rawText: '/* A comment */',
    trailingNewLine: true,
    pos: 10,
  }

  it('same reference returns the same reference', () => {
    const result = MultiLineCommentKeepDeepEqualityCall(oldValue, oldValue)
    expect(result.value).toBe(oldValue)
    expect(result.areEqual).toEqual(true)
  })
  it('same value returns the same reference', () => {
    const result = MultiLineCommentKeepDeepEqualityCall(oldValue, newSameValue)
    expect(result.value).toBe(oldValue)
    expect(result.areEqual).toEqual(true)
  })
  it('different but similar value handled appropriately', () => {
    const result = MultiLineCommentKeepDeepEqualityCall(oldValue, newDifferentValue)
    expect(result.value.type).toBe(oldValue.type)
    expect(result.value.comment).toBe(oldValue.comment)
    expect(result.value.rawText).toBe(oldValue.rawText)
    expect(result.value.trailingNewLine).toBe(newDifferentValue.trailingNewLine)
    expect(result.value.pos).toBe(oldValue.pos)
    expect(result.value).toEqual(newDifferentValue)
    expect(result.areEqual).toEqual(false)
  })
})

describe('SingleLineCommentKeepDeepEqualityCall', () => {
  const oldValue: SingleLineComment = {
    type: 'SINGLE_LINE_COMMENT',
    comment: 'A comment',
    rawText: '// A comment',
    trailingNewLine: false,
    pos: 10,
  }
  const newSameValue: SingleLineComment = {
    type: 'SINGLE_LINE_COMMENT',
    comment: 'A comment',
    rawText: '// A comment',
    trailingNewLine: false,
    pos: 10,
  }
  const newDifferentValue: SingleLineComment = {
    type: 'SINGLE_LINE_COMMENT',
    comment: 'A comment',
    rawText: '// A comment',
    trailingNewLine: true,
    pos: 10,
  }

  it('same reference returns the same reference', () => {
    const result = SingleLineCommentKeepDeepEqualityCall(oldValue, oldValue)
    expect(result.value).toBe(oldValue)
    expect(result.areEqual).toEqual(true)
  })
  it('same value returns the same reference', () => {
    const result = SingleLineCommentKeepDeepEqualityCall(oldValue, newSameValue)
    expect(result.value).toBe(oldValue)
    expect(result.areEqual).toEqual(true)
  })
  it('different but similar value handled appropriately', () => {
    const result = SingleLineCommentKeepDeepEqualityCall(oldValue, newDifferentValue)
    expect(result.value.type).toBe(oldValue.type)
    expect(result.value.comment).toBe(oldValue.comment)
    expect(result.value.rawText).toBe(oldValue.rawText)
    expect(result.value.trailingNewLine).toBe(newDifferentValue.trailingNewLine)
    expect(result.value.pos).toBe(oldValue.pos)
    expect(result.value).toEqual(newDifferentValue)
    expect(result.areEqual).toEqual(false)
  })
})

describe('CommentKeepDeepEqualityCall', () => {
  const oldMultiValue: MultiLineComment = {
    type: 'MULTI_LINE_COMMENT',
    comment: 'A comment',
    rawText: '/* A comment */',
    trailingNewLine: false,
    pos: 10,
  }
  const newSameMultiValue: MultiLineComment = {
    type: 'MULTI_LINE_COMMENT',
    comment: 'A comment',
    rawText: '/* A comment */',
    trailingNewLine: false,
    pos: 10,
  }
  const newDifferentMultiValue: MultiLineComment = {
    type: 'MULTI_LINE_COMMENT',
    comment: 'A comment',
    rawText: '/* A comment */',
    trailingNewLine: true,
    pos: 10,
  }

  const oldSingleValue: SingleLineComment = {
    type: 'SINGLE_LINE_COMMENT',
    comment: 'A comment',
    rawText: '// A comment',
    trailingNewLine: false,
    pos: 10,
  }
  const newSameSingleValue: SingleLineComment = {
    type: 'SINGLE_LINE_COMMENT',
    comment: 'A comment',
    rawText: '// A comment',
    trailingNewLine: false,
    pos: 10,
  }
  const newDifferentSingleValue: SingleLineComment = {
    type: 'SINGLE_LINE_COMMENT',
    comment: 'A comment',
    rawText: '// A comment',
    trailingNewLine: true,
    pos: 10,
  }

  it('same reference returns the same reference', () => {
    const resultSingle = CommentKeepDeepEqualityCall(oldSingleValue, oldSingleValue)
    expect(resultSingle.value).toBe(oldSingleValue)
    expect(resultSingle.areEqual).toEqual(true)

    const resultMulti = CommentKeepDeepEqualityCall(oldMultiValue, oldMultiValue)
    expect(resultMulti.value).toBe(oldMultiValue)
    expect(resultMulti.areEqual).toEqual(true)
  })
  it('same value returns the same reference', () => {
    const resultSingle = CommentKeepDeepEqualityCall(oldSingleValue, newSameSingleValue)
    expect(resultSingle.value).toBe(oldSingleValue)
    expect(resultSingle.areEqual).toEqual(true)

    const resultMulti = CommentKeepDeepEqualityCall(oldMultiValue, newSameMultiValue)
    expect(resultMulti.value).toBe(oldMultiValue)
    expect(resultMulti.areEqual).toEqual(true)
  })
  it('different but similar value handled appropriately', () => {
    const resultSingle = CommentKeepDeepEqualityCall(oldSingleValue, newDifferentSingleValue)
    expect(resultSingle.value.type).toBe(oldSingleValue.type)
    expect(resultSingle.value.comment).toBe(oldSingleValue.comment)
    expect(resultSingle.value.rawText).toBe(oldSingleValue.rawText)
    expect(resultSingle.value.trailingNewLine).toBe(newDifferentSingleValue.trailingNewLine)
    expect(resultSingle.value.pos).toBe(oldSingleValue.pos)
    expect(resultSingle.value).toEqual(newDifferentSingleValue)
    expect(resultSingle.areEqual).toEqual(false)

    const resultMulti = CommentKeepDeepEqualityCall(oldMultiValue, newDifferentMultiValue)
    expect(resultMulti.value.type).toBe(oldMultiValue.type)
    expect(resultMulti.value.comment).toBe(oldMultiValue.comment)
    expect(resultMulti.value.rawText).toBe(oldMultiValue.rawText)
    expect(resultMulti.value.trailingNewLine).toBe(newDifferentMultiValue.trailingNewLine)
    expect(resultMulti.value.pos).toBe(oldMultiValue.pos)
    expect(resultMulti.value).toEqual(newDifferentMultiValue)
    expect(resultMulti.areEqual).toEqual(false)
  })
})

describe('ParsedCommentsKeepDeepEqualityCall', () => {
  const oldValue: ParsedComments = {
    leadingComments: [
      {
        type: 'SINGLE_LINE_COMMENT',
        comment: 'A comment',
        rawText: '// A comment',
        trailingNewLine: false,
        pos: 10,
      },
    ],
    trailingComments: [
      {
        type: 'MULTI_LINE_COMMENT',
        comment: 'A comment',
        rawText: '/* A comment */',
        trailingNewLine: false,
        pos: 10,
      },
    ],
  }
  const newSameValue: ParsedComments = {
    leadingComments: [
      {
        type: 'SINGLE_LINE_COMMENT',
        comment: 'A comment',
        rawText: '// A comment',
        trailingNewLine: false,
        pos: 10,
      },
    ],
    trailingComments: [
      {
        type: 'MULTI_LINE_COMMENT',
        comment: 'A comment',
        rawText: '/* A comment */',
        trailingNewLine: false,
        pos: 10,
      },
    ],
  }
  const newDifferentValue: ParsedComments = {
    leadingComments: [
      {
        type: 'SINGLE_LINE_COMMENT',
        comment: 'A comment',
        rawText: '// A comment',
        trailingNewLine: false,
        pos: 10,
      },
    ],
    trailingComments: [
      {
        type: 'MULTI_LINE_COMMENT',
        comment: 'A comment',
        rawText: '/* A comment */',
        trailingNewLine: true,
        pos: 10,
      },
    ],
  }

  it('same reference returns the same reference', () => {
    const result = ParsedCommentsKeepDeepEqualityCall(oldValue, oldValue)
    expect(result.value).toBe(oldValue)
    expect(result.areEqual).toEqual(true)
  })
  it('same value returns the same reference', () => {
    const result = ParsedCommentsKeepDeepEqualityCall(oldValue, newSameValue)
    expect(result.value).toBe(oldValue)
    expect(result.areEqual).toEqual(true)
  })
  it('different but similar value handled appropriately', () => {
    const result = ParsedCommentsKeepDeepEqualityCall(oldValue, newDifferentValue)
    expect(result.value.leadingComments).toBe(oldValue.leadingComments)
    expect(result.value.trailingComments[0].type).toBe(oldValue.trailingComments[0].type)
    expect(result.value.trailingComments[0].comment).toBe(oldValue.trailingComments[0].comment)
    expect(result.value.trailingComments[0].rawText).toBe(oldValue.trailingComments[0].rawText)
    expect(result.value.trailingComments[0].trailingNewLine).toBe(
      newDifferentValue.trailingComments[0].trailingNewLine,
    )
    expect(result.value.trailingComments[0].pos).toBe(oldValue.trailingComments[0].pos)
    expect(result.value).toEqual(newDifferentValue)
    expect(result.areEqual).toEqual(false)
  })
})

describe('JSXAttributeValueKeepDeepEqualityCall', () => {
  const oldValue = jsExpressionValue('old', emptyComments, 'old')
  const newSameValue = jsExpressionValue('old', emptyComments, 'old')
  const newDifferentValue = jsExpressionValue('new', emptyComments, 'new')

  it('same reference returns the same reference', () => {
    const result = JSXAttributeValueKeepDeepEqualityCall(oldValue, oldValue)
    expect(result.value).toBe(oldValue)
    expect(result.areEqual).toEqual(true)
  })
  it('same value returns the same reference', () => {
    const result = JSXAttributeValueKeepDeepEqualityCall(oldValue, newSameValue)
    expect(result.value).toBe(oldValue)
    expect(result.areEqual).toEqual(true)
  })
  it('different but similar value handled appropriately', () => {
    const result = JSXAttributeValueKeepDeepEqualityCall(oldValue, newDifferentValue)
    expect(result.value.type).toBe(oldValue.type)
    expect(result.value.comments).toBe(oldValue.comments)
    expect(result.value.value).toBe(newDifferentValue.value)
    expect(result.value).toEqual(newDifferentValue)
    expect(result.areEqual).toEqual(false)
  })
})

describe('JSXAttributeOtherJavaScriptKeepDeepEqualityCall', () => {
  const oldValue: JSExpressionOtherJavaScript = {
    type: 'ATTRIBUTE_OTHER_JAVASCRIPT',
    originalJavascript: 'old',
    javascript: 'old',
    transpiledJavascript: 'old',
    definedElsewhere: ['old'],
    sourceMap: { a: 1, b: [2] } as any,
    uniqueID: 'old',
    elementsWithin: {},
  }
  const newSameValue: JSExpressionOtherJavaScript = {
    type: 'ATTRIBUTE_OTHER_JAVASCRIPT',
    originalJavascript: 'old',
    javascript: 'old',
    transpiledJavascript: 'old',
    definedElsewhere: ['old'],
    sourceMap: { a: 1, b: [2] } as any,
    uniqueID: 'old',
    elementsWithin: {},
  }
  const newDifferentValue: JSExpressionOtherJavaScript = {
    type: 'ATTRIBUTE_OTHER_JAVASCRIPT',
    originalJavascript: 'new',
    javascript: 'new',
    transpiledJavascript: 'old',
    definedElsewhere: ['old'],
    sourceMap: { a: 1, b: [2] } as any,
    uniqueID: 'new',
    elementsWithin: {},
  }

  it('same reference returns the same reference', () => {
    const result = JSXAttributeOtherJavaScriptKeepDeepEqualityCall()(oldValue, oldValue)
    expect(result.value).toBe(oldValue)
    expect(result.areEqual).toEqual(true)
  })
  it('same value returns the same reference', () => {
    const result = JSXAttributeOtherJavaScriptKeepDeepEqualityCall()(oldValue, newSameValue)
    expect(result.value).toBe(oldValue)
    expect(result.areEqual).toEqual(true)
  })
  it('different but similar value handled appropriately', () => {
    const result = JSXAttributeOtherJavaScriptKeepDeepEqualityCall()(oldValue, newDifferentValue)
    expect(result.value.type).toBe(oldValue.type)
    expect(result.value.javascript).toBe(newDifferentValue.javascript)
    expect(result.value.transpiledJavascript).toBe(oldValue.transpiledJavascript)
    expect(result.value.definedElsewhere).toBe(oldValue.definedElsewhere)
    expect(result.value.sourceMap).toBe(oldValue.sourceMap)
    expect(result.value.uniqueID).not.toBe(oldValue.uniqueID)
    expect(result.value.elementsWithin).toBe(oldValue.elementsWithin)
    expect(result.value).toEqual(newDifferentValue)
    expect(result.areEqual).toEqual(false)
  })
})

describe('JSXArrayValueKeepDeepEqualityCall', () => {
  const oldValue = jsxArrayValue(jsExpressionValue('old', emptyComments, 'old'), emptyComments)
  const newSameValue = jsxArrayValue(jsExpressionValue('old', emptyComments, 'old'), emptyComments)
  const newDifferentValue = jsxArrayValue(
    jsExpressionValue('new', emptyComments, 'new'),
    emptyComments,
  )

  it('same reference returns the same reference', () => {
    const result = JSXArrayValueKeepDeepEqualityCall()(oldValue, oldValue)
    expect(result.value).toBe(oldValue)
    expect(result.areEqual).toEqual(true)
  })
  it('same value returns the same reference', () => {
    const result = JSXArrayValueKeepDeepEqualityCall()(oldValue, newSameValue)
    expect(result.value).toBe(oldValue)
    expect(result.areEqual).toEqual(true)
  })
  it('different but similar value handled appropriately', () => {
    const result = JSXArrayValueKeepDeepEqualityCall()(oldValue, newDifferentValue)
    expect(result.value.type).toBe(oldValue.type)
    expect(result.value.comments).toBe(oldValue.comments)
    expect(result.value.value.type).toBe(newDifferentValue.value.type)
    expect((result.value.value as JSExpressionValue<string>).value).toBe(
      (newDifferentValue.value as JSExpressionValue<string>).value,
    )
    expect((result.value.value as JSExpressionValue<string>).comments).toBe(
      (newDifferentValue.value as JSExpressionValue<string>).comments,
    )
    expect(result.value).toEqual(newDifferentValue)
    expect(result.areEqual).toEqual(false)
  })
})

describe('JSXArraySpreadKeepDeepEqualityCall', () => {
  const oldValue = jsxArraySpread(jsExpressionValue('old', emptyComments, 'old'), emptyComments)
  const newSameValue = jsxArraySpread(jsExpressionValue('old', emptyComments, 'old'), emptyComments)
  const newDifferentValue = jsxArraySpread(
    jsExpressionValue('new', emptyComments, 'new'),
    emptyComments,
  )

  it('same reference returns the same reference', () => {
    const result = JSXArraySpreadKeepDeepEqualityCall()(oldValue, oldValue)
    expect(result.value).toBe(oldValue)
    expect(result.areEqual).toEqual(true)
  })
  it('same value returns the same reference', () => {
    const result = JSXArraySpreadKeepDeepEqualityCall()(oldValue, newSameValue)
    expect(result.value).toBe(oldValue)
    expect(result.areEqual).toEqual(true)
  })
  it('different but similar value handled appropriately', () => {
    const result = JSXArraySpreadKeepDeepEqualityCall()(oldValue, newDifferentValue)
    expect(result.value.type).toBe(oldValue.type)
    expect(result.value.comments).toBe(oldValue.comments)
    expect(result.value.value.type).toBe(newDifferentValue.value.type)
    expect((result.value.value as JSExpressionValue<string>).value).toBe(
      (newDifferentValue.value as JSExpressionValue<string>).value,
    )
    expect((result.value.value as JSExpressionValue<string>).comments).toBe(
      (newDifferentValue.value as JSExpressionValue<string>).comments,
    )
    expect(result.value).toEqual(newDifferentValue)
    expect(result.areEqual).toEqual(false)
  })
})

describe('JSXArrayElementKeepDeepEqualityCall', () => {
  const oldValue = jsxArrayValue(jsExpressionValue('old', emptyComments, 'old'), emptyComments)
  const newSameValue = jsxArrayValue(jsExpressionValue('old', emptyComments, 'old'), emptyComments)
  const newDifferentValue = jsxArrayValue(
    jsExpressionValue('new', emptyComments, 'new'),
    emptyComments,
  )

  it('same reference returns the same reference', () => {
    const result = JSXArrayElementKeepDeepEqualityCall()(oldValue, oldValue)
    expect(result.value).toBe(oldValue)
    expect(result.areEqual).toEqual(true)
  })
  it('same value returns the same reference', () => {
    const result = JSXArrayElementKeepDeepEqualityCall()(oldValue, newSameValue)
    expect(result.value).toBe(oldValue)
    expect(result.areEqual).toEqual(true)
  })
  it('different but similar value handled appropriately', () => {
    const result = JSXArrayElementKeepDeepEqualityCall()(oldValue, newDifferentValue)
    expect(result.value.type).toBe(oldValue.type)
    expect(result.value.comments).toBe(oldValue.comments)
    expect(result.value.value.type).toBe(newDifferentValue.value.type)
    expect((result.value.value as JSExpressionValue<string>).value).toBe(
      (newDifferentValue.value as JSExpressionValue<string>).value,
    )
    expect((result.value.value as JSExpressionValue<string>).comments).toBe(
      (newDifferentValue.value as JSExpressionValue<string>).comments,
    )
    expect(result.value).toEqual(newDifferentValue)
    expect(result.areEqual).toEqual(false)
  })
})

describe('JSXAttributeNestedArrayKeepDeepEqualityCall', () => {
  const oldValue = jsExpressionNestedArray(
    [jsxArrayValue(jsExpressionValue('old', emptyComments, 'old'), emptyComments)],
    emptyComments,
    'oldarray',
  )
  const newSameValue = jsExpressionNestedArray(
    [jsxArrayValue(jsExpressionValue('old', emptyComments, 'old'), emptyComments)],
    emptyComments,
    'oldarray',
  )
  const newDifferentValue = jsExpressionNestedArray(
    [jsxArrayValue(jsExpressionValue('new', emptyComments, 'new'), emptyComments)],
    emptyComments,
    'newarray',
  )

  it('same reference returns the same reference', () => {
    const result = JSXAttributeNestedArrayKeepDeepEqualityCall()(oldValue, oldValue)
    expect(result.value).toBe(oldValue)
    expect(result.areEqual).toEqual(true)
  })
  it('same value returns the same reference', () => {
    const result = JSXAttributeNestedArrayKeepDeepEqualityCall()(oldValue, newSameValue)
    expect(result.value).toBe(oldValue)
    expect(result.areEqual).toEqual(true)
  })
  it('different but similar value handled appropriately', () => {
    const result = JSXAttributeNestedArrayKeepDeepEqualityCall()(oldValue, newDifferentValue)
    expect(result.value.type).toBe(oldValue.type)
    expect(result.value.comments).toBe(oldValue.comments)
    expect(result.value.content[0].value.type).toBe(newDifferentValue.content[0].value.type)
    expect((result.value.content[0].value as JSExpressionValue<string>).value).toBe(
      (newDifferentValue.content[0].value as JSExpressionValue<string>).value,
    )
    expect((result.value.content[0].value as JSExpressionValue<string>).comments).toBe(
      (newDifferentValue.content[0].value as JSExpressionValue<string>).comments,
    )
    expect(result.value).toEqual(newDifferentValue)
    expect(result.areEqual).toEqual(false)
  })
})

describe('JSXSpreadAssignmentKeepDeepEqualityCall', () => {
  const oldValue = jsxSpreadAssignment(
    jsExpressionValue('old', emptyComments, 'old'),
    emptyComments,
  )
  const newSameValue = jsxSpreadAssignment(
    jsExpressionValue('old', emptyComments, 'old'),
    emptyComments,
  )
  const newDifferentValue = jsxSpreadAssignment(
    jsExpressionValue('new', emptyComments, 'new'),
    emptyComments,
  )

  it('same reference returns the same reference', () => {
    const result = JSXSpreadAssignmentKeepDeepEqualityCall()(oldValue, oldValue)
    expect(result.value).toBe(oldValue)
    expect(result.areEqual).toEqual(true)
  })
  it('same value returns the same reference', () => {
    const result = JSXSpreadAssignmentKeepDeepEqualityCall()(oldValue, newSameValue)
    expect(result.value).toBe(oldValue)
    expect(result.areEqual).toEqual(true)
  })
  it('different but similar value handled appropriately', () => {
    const result = JSXSpreadAssignmentKeepDeepEqualityCall()(oldValue, newDifferentValue)
    expect(result.value.type).toBe(oldValue.type)
    expect(result.value.comments).toBe(oldValue.comments)
    expect(result.value.value.type).toBe(newDifferentValue.value.type)
    expect((result.value.value as JSExpressionValue<string>).value).toBe(
      (newDifferentValue.value as JSExpressionValue<string>).value,
    )
    expect((result.value.value as JSExpressionValue<string>).comments).toBe(
      (newDifferentValue.value as JSExpressionValue<string>).comments,
    )
    expect(result.value).toEqual(newDifferentValue)
    expect(result.areEqual).toEqual(false)
  })
})

describe('JSXPropertyAssignmentKeepDeepEqualityCall', () => {
  const oldValue = jsxPropertyAssignment(
    'key',
    jsExpressionValue('old', emptyComments, 'old'),
    emptyComments,
    emptyComments,
  )
  const newSameValue = jsxPropertyAssignment(
    'key',
    jsExpressionValue('old', emptyComments, 'old'),
    emptyComments,
    emptyComments,
  )
  const newDifferentValue = jsxPropertyAssignment(
    'key',
    jsExpressionValue('new', emptyComments, 'new'),
    emptyComments,
    emptyComments,
  )

  it('same reference returns the same reference', () => {
    const result = JSXPropertyAssignmentKeepDeepEqualityCall()(oldValue, oldValue)
    expect(result.value).toBe(oldValue)
    expect(result.areEqual).toEqual(true)
  })
  it('same value returns the same reference', () => {
    const result = JSXPropertyAssignmentKeepDeepEqualityCall()(oldValue, newSameValue)
    expect(result.value).toBe(oldValue)
    expect(result.areEqual).toEqual(true)
  })
  it('different but similar value handled appropriately', () => {
    const result = JSXPropertyAssignmentKeepDeepEqualityCall()(oldValue, newDifferentValue)
    expect(result.value.type).toBe(oldValue.type)
    expect(result.value.key).toBe(oldValue.key)
    expect(result.value.comments).toBe(oldValue.comments)
    expect(result.value.keyComments).toBe(oldValue.keyComments)
    expect(result.value.value.type).toBe(newDifferentValue.value.type)
    expect((result.value.value as JSExpressionValue<string>).value).toBe(
      (newDifferentValue.value as JSExpressionValue<string>).value,
    )
    expect((result.value.value as JSExpressionValue<string>).comments).toBe(
      (newDifferentValue.value as JSExpressionValue<string>).comments,
    )
    expect(result.value).toEqual(newDifferentValue)
    expect(result.areEqual).toEqual(false)
  })
})

describe('JSXPropertyKeepDeepEqualityCall', () => {
  const oldValue = jsxSpreadAssignment(
    jsExpressionValue('old', emptyComments, 'old'),
    emptyComments,
  )
  const newSameValue = jsxSpreadAssignment(
    jsExpressionValue('old', emptyComments, 'old'),
    emptyComments,
  )
  const newDifferentValue = jsxSpreadAssignment(
    jsExpressionValue('new', emptyComments, 'new'),
    emptyComments,
  )

  it('same reference returns the same reference', () => {
    const result = JSXPropertyKeepDeepEqualityCall()(oldValue, oldValue)
    expect(result.value).toBe(oldValue)
    expect(result.areEqual).toEqual(true)
  })
  it('same value returns the same reference', () => {
    const result = JSXPropertyKeepDeepEqualityCall()(oldValue, newSameValue)
    expect(result.value).toBe(oldValue)
    expect(result.areEqual).toEqual(true)
  })
  it('different but similar value handled appropriately', () => {
    const result = JSXPropertyKeepDeepEqualityCall()(oldValue, newDifferentValue)
    expect(result.value.type).toBe(oldValue.type)
    expect(result.value.comments).toBe(oldValue.comments)
    expect(result.value.value.type).toBe(newDifferentValue.value.type)
    expect((result.value.value as JSExpressionValue<string>).value).toBe(
      (newDifferentValue.value as JSExpressionValue<string>).value,
    )
    expect((result.value.value as JSExpressionValue<string>).comments).toBe(
      (newDifferentValue.value as JSExpressionValue<string>).comments,
    )
    expect(result.value).toEqual(newDifferentValue)
    expect(result.areEqual).toEqual(false)
  })
})

describe('JSXAttributeNestedObjectKeepDeepEqualityCall', () => {
  const oldValue = jsExpressionNestedObject(
    [jsxSpreadAssignment(jsExpressionValue('old', emptyComments, 'old'), emptyComments)],
    emptyComments,
    'old',
  )
  const newSameValue = jsExpressionNestedObject(
    [jsxSpreadAssignment(jsExpressionValue('old', emptyComments, 'old'), emptyComments)],
    emptyComments,
    'old',
  )
  const newDifferentValue = jsExpressionNestedObject(
    [jsxSpreadAssignment(jsExpressionValue('new', emptyComments, 'new'), emptyComments)],
    emptyComments,
    'new',
  )

  it('same reference returns the same reference', () => {
    const result = JSXAttributeNestedObjectKeepDeepEqualityCall()(oldValue, oldValue)
    expect(result.value).toBe(oldValue)
    expect(result.areEqual).toEqual(true)
  })
  it('same value returns the same reference', () => {
    const result = JSXAttributeNestedObjectKeepDeepEqualityCall()(oldValue, newSameValue)
    expect(result.value).toBe(oldValue)
    expect(result.areEqual).toEqual(true)
  })
  it('different but similar value handled appropriately', () => {
    const result = JSXAttributeNestedObjectKeepDeepEqualityCall()(oldValue, newDifferentValue)
    expect(result.value.type).toBe(oldValue.type)
    expect(result.value.comments).toBe(oldValue.comments)
    expect(result.value.content[0].value.type).toBe(newDifferentValue.content[0].value.type)
    expect((result.value.content[0].value as JSExpressionValue<string>).value).toBe(
      (newDifferentValue.content[0].value as JSExpressionValue<string>).value,
    )
    expect((result.value.content[0].value as JSExpressionValue<string>).comments).toBe(
      (newDifferentValue.content[0].value as JSExpressionValue<string>).comments,
    )
    expect(result.value).toEqual(newDifferentValue)
    expect(result.areEqual).toEqual(false)
  })
})

describe('JSXAttributeFunctionCallKeepDeepEqualityCall', () => {
  const oldValue = jsExpressionFunctionCall(
    'old',
    [jsExpressionValue('old', emptyComments, 'old')],
    'oldcall',
  )
  const newSameValue = jsExpressionFunctionCall(
    'old',
    [jsExpressionValue('old', emptyComments, 'old')],
    'oldcall',
  )
  const newDifferentValue = jsExpressionFunctionCall(
    'new',
    [jsExpressionValue('old', emptyComments, 'old')],
    'newcall',
  )

  it('same reference returns the same reference', () => {
    const result = JSXAttributeFunctionCallKeepDeepEqualityCall()(oldValue, oldValue)
    expect(result.value).toBe(oldValue)
    expect(result.areEqual).toEqual(true)
  })
  it('same value returns the same reference', () => {
    const result = JSXAttributeFunctionCallKeepDeepEqualityCall()(oldValue, newSameValue)
    expect(result.value).toBe(oldValue)
    expect(result.areEqual).toEqual(true)
  })
  it('different but similar value handled appropriately', () => {
    const result = JSXAttributeFunctionCallKeepDeepEqualityCall()(oldValue, newDifferentValue)
    expect(result.value.type).toBe(oldValue.type)
    expect(result.value.functionName).toBe(newDifferentValue.functionName)
    expect(result.value.parameters).toBe(oldValue.parameters)
    expect(result.value).toEqual(newDifferentValue)
    expect(result.areEqual).toEqual(false)
  })
})

describe('JSXAttributeKeepDeepEqualityCall', () => {
  const oldValue = jsExpressionValue('old', emptyComments, 'old')
  const newSameValue = jsExpressionValue('old', emptyComments, 'old')
  const newDifferentValue = jsExpressionValue('new', emptyComments, 'new')

  it('same reference returns the same reference', () => {
    const result = JSXAttributeKeepDeepEqualityCall(oldValue, oldValue)
    expect(result.value).toBe(oldValue)
    expect(result.areEqual).toEqual(true)
  })
  it('same value returns the same reference', () => {
    const result = JSXAttributeKeepDeepEqualityCall(oldValue, newSameValue)
    expect(result.value).toBe(oldValue)
    expect(result.areEqual).toEqual(true)
  })
  it('different but similar value handled appropriately', () => {
    const result = JSXAttributeKeepDeepEqualityCall(oldValue, newDifferentValue)
    expect(result.value.type).toBe(oldValue.type)
    expect((result.value as JSExpressionValue<string>).value).toBe(
      (newDifferentValue as JSExpressionValue<string>).value,
    )
    expect((result.value as JSExpressionValue<string>).comments).toBe(
      (newDifferentValue as JSExpressionValue<string>).comments,
    )
    expect(result.value).toEqual(newDifferentValue)
    expect(result.areEqual).toEqual(false)
  })
})

describe('JSXAttributesEntryDeepEqualityCall', () => {
  const oldValue = jsxAttributesEntry(
    'key',
    jsExpressionValue('old', emptyComments, 'old'),
    emptyComments,
  )
  const newSameValue = jsxAttributesEntry(
    'key',
    jsExpressionValue('old', emptyComments, 'old'),
    emptyComments,
  )
  const newDifferentValue = jsxAttributesEntry(
    'key',
    jsExpressionValue('new', emptyComments, 'new'),
    emptyComments,
  )

  it('same reference returns the same reference', () => {
    const result = JSXAttributesEntryDeepEqualityCall()(oldValue, oldValue)
    expect(result.value).toBe(oldValue)
    expect(result.areEqual).toEqual(true)
  })
  it('same value returns the same reference', () => {
    const result = JSXAttributesEntryDeepEqualityCall()(oldValue, newSameValue)
    expect(result.value).toBe(oldValue)
    expect(result.areEqual).toEqual(true)
  })
  it('different but similar value handled appropriately', () => {
    const result = JSXAttributesEntryDeepEqualityCall()(oldValue, newDifferentValue)
    expect(result.value.type).toBe(oldValue.type)
    expect(result.value.key).toBe(oldValue.key)
    expect(result.value.comments).toBe(oldValue.comments)
    expect((result.value.value as JSExpressionValue<string>).value).toBe(
      (newDifferentValue.value as JSExpressionValue<string>).value,
    )
    expect((result.value.value as JSExpressionValue<string>).comments).toBe(
      (newDifferentValue.value as JSExpressionValue<string>).comments,
    )
    expect(result.value).toEqual(newDifferentValue)
    expect(result.areEqual).toEqual(false)
  })
})

describe('JSXAttributesSpreadDeepEqualityCall', () => {
  const oldValue = jsxAttributesSpread(
    jsExpressionValue('old', emptyComments, 'old'),
    emptyComments,
  )
  const newSameValue = jsxAttributesSpread(
    jsExpressionValue('old', emptyComments, 'old'),
    emptyComments,
  )
  const newDifferentValue = jsxAttributesSpread(
    jsExpressionValue('new', emptyComments, 'new'),
    emptyComments,
  )

  it('same reference returns the same reference', () => {
    const result = JSXAttributesSpreadDeepEqualityCall()(oldValue, oldValue)
    expect(result.value).toBe(oldValue)
    expect(result.areEqual).toEqual(true)
  })
  it('same value returns the same reference', () => {
    const result = JSXAttributesSpreadDeepEqualityCall()(oldValue, newSameValue)
    expect(result.value).toBe(oldValue)
    expect(result.areEqual).toEqual(true)
  })
  it('different but similar value handled appropriately', () => {
    const result = JSXAttributesSpreadDeepEqualityCall()(oldValue, newDifferentValue)
    expect(result.value.type).toBe(oldValue.type)
    expect(result.value.comments).toBe(oldValue.comments)
    expect((result.value.spreadValue as JSExpressionValue<string>).value).toBe(
      (newDifferentValue.spreadValue as JSExpressionValue<string>).value,
    )
    expect((result.value.spreadValue as JSExpressionValue<string>).comments).toBe(
      (newDifferentValue.spreadValue as JSExpressionValue<string>).comments,
    )
    expect(result.value).toEqual(newDifferentValue)
    expect(result.areEqual).toEqual(false)
  })
})

describe('JSXAttributesPartDeepEqualityCall', () => {
  const oldValue = jsxAttributesSpread(
    jsExpressionValue('old', emptyComments, 'old'),
    emptyComments,
  )
  const newSameValue = jsxAttributesSpread(
    jsExpressionValue('old', emptyComments, 'old'),
    emptyComments,
  )
  const newDifferentValue = jsxAttributesSpread(
    jsExpressionValue('new', emptyComments, 'new'),
    emptyComments,
  )

  it('same reference returns the same reference', () => {
    const result = JSXAttributesPartDeepEqualityCall()(oldValue, oldValue)
    expect(result.value).toBe(oldValue)
    expect(result.areEqual).toEqual(true)
  })
  it('same value returns the same reference', () => {
    const result = JSXAttributesPartDeepEqualityCall()(oldValue, newSameValue)
    expect(result.value).toBe(oldValue)
    expect(result.areEqual).toEqual(true)
  })
  it('different but similar value handled appropriately', () => {
    const result = JSXAttributesPartDeepEqualityCall()(oldValue, newDifferentValue)
    expect(result.value.type).toBe(oldValue.type)
    expect(result.value.comments).toBe(oldValue.comments)
    expect(result.value).toEqual(newDifferentValue)
    expect(result.areEqual).toEqual(false)
  })
})

describe('JSXAttributesKeepDeepEqualityCall', () => {
  const oldValue = [
    jsxAttributesSpread(jsExpressionValue('old', emptyComments, 'old'), emptyComments),
  ]
  const newSameValue = [
    jsxAttributesSpread(jsExpressionValue('old', emptyComments, 'old'), emptyComments),
  ]
  const newDifferentValue = [
    jsxAttributesSpread(jsExpressionValue('new', emptyComments, 'new'), emptyComments),
  ]

  it('same reference returns the same reference', () => {
    const result = JSXAttributesKeepDeepEqualityCall()(oldValue, oldValue)
    expect(result.value).toBe(oldValue)
    expect(result.areEqual).toEqual(true)
  })
  it('same value returns the same reference', () => {
    const result = JSXAttributesKeepDeepEqualityCall()(oldValue, newSameValue)
    expect(result.value).toBe(oldValue)
    expect(result.areEqual).toEqual(true)
  })
  it('different but similar value handled appropriately', () => {
    const result = JSXAttributesKeepDeepEqualityCall()(oldValue, newDifferentValue)
    expect(result.value[0].type).toBe(oldValue[0].type)
    expect(result.value[0].comments).toBe(oldValue[0].comments)
    expect(result.value).toEqual(newDifferentValue)
    expect(result.areEqual).toEqual(false)
  })
})
