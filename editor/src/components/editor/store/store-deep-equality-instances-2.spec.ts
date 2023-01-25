import {
  ArbitraryJSBlock,
  ElementsWithin,
  emptyComments,
  JSXArbitraryBlock,
  jsxAttributesSpread,
  jsxAttributeValue,
  JSXElement,
  JSXTextBlock,
  JSXFragment,
  RegularParam,
  DestructuredParamPart,
  DestructuredObject,
  DestructuredArrayPart,
  Param,
  DestructuredArray,
  BoundParam,
  UtopiaJSXComponent,
  jsxElementName,
} from '../../../core/shared/element-template'
import {
  ArbitraryJSBlockKeepDeepEquality,
  BoundParamKeepDeepEquality,
  DestructuredArrayKeepDeepEquality,
  DestructuredArrayPartKeepDeepEquality,
  DestructuredObjectParamKeepDeepEquality,
  DestructuredParamPartKeepDeepEquality,
  ElementsWithinKeepDeepEqualityCall,
  JSXArbitraryBlockKeepDeepEquality,
  JSXElementChildArrayKeepDeepEquality,
  JSXElementChildKeepDeepEquality,
  JSXElementKeepDeepEquality,
  JSXFragmentKeepDeepEquality,
  JSXTextBlockKeepDeepEquality,
  ParamKeepDeepEquality,
  RegularParamKeepDeepEquality,
  UtopiaJSXComponentKeepDeepEquality,
} from './store-deep-equality-instances'

describe('JSXElementKeepDeepEquality', () => {
  const oldValue: JSXElement = {
    type: 'JSX_ELEMENT',
    name: {
      baseVariable: 'React',
      propertyPath: {
        propertyElements: ['style', 'backgroundColor'],
      },
    },
    props: [jsxAttributesSpread(jsxAttributeValue('prop', emptyComments), emptyComments)],
    children: [
      {
        type: 'JSX_TEXT_BLOCK',
        text: 'some text',
        uniqueID: 'text-uid',
      },
    ],
    uid: 'uid',
  }
  const newSameValue: JSXElement = {
    type: 'JSX_ELEMENT',
    name: {
      baseVariable: 'React',
      propertyPath: {
        propertyElements: ['style', 'backgroundColor'],
      },
    },
    props: [jsxAttributesSpread(jsxAttributeValue('prop', emptyComments), emptyComments)],
    children: [
      {
        type: 'JSX_TEXT_BLOCK',
        text: 'some text',
        uniqueID: 'text-uid',
      },
    ],
    uid: 'uid',
  }
  const newDifferentValue: JSXElement = {
    type: 'JSX_ELEMENT',
    name: {
      baseVariable: 'React',
      propertyPath: {
        propertyElements: ['style', 'backgroundColor'],
      },
    },
    props: [jsxAttributesSpread(jsxAttributeValue('prop', emptyComments), emptyComments)],
    children: [
      {
        type: 'JSX_TEXT_BLOCK',
        text: 'some text',
        uniqueID: 'text-uid',
      },
    ],
    uid: 'new-uid',
  }

  it('same reference returns the same reference', () => {
    const result = JSXElementKeepDeepEquality(oldValue, oldValue)
    expect(result.value).toBe(oldValue)
    expect(result.areEqual).toEqual(true)
  })
  it('same value returns the same reference', () => {
    const result = JSXElementKeepDeepEquality(oldValue, newSameValue)
    expect(result.value).toBe(oldValue)
    expect(result.areEqual).toEqual(true)
  })
  it('different but similar value handled appropriately', () => {
    const result = JSXElementKeepDeepEquality(oldValue, newDifferentValue)
    expect(result.value.type).toBe(oldValue.type)
    expect(result.value.name).toBe(oldValue.name)
    expect(result.value.props).toBe(oldValue.props)
    expect(result.value.children).toBe(oldValue.children)
    expect(result.value.uid).toBe(newDifferentValue.uid)
    expect(result.value).toEqual(newDifferentValue)
    expect(result.areEqual).toEqual(false)
  })
})

describe('ElementsWithinKeepDeepEqualityCall', () => {
  const innerValue: JSXElement = {
    type: 'JSX_ELEMENT',
    name: {
      baseVariable: 'React',
      propertyPath: {
        propertyElements: ['style', 'backgroundColor'],
      },
    },
    props: [jsxAttributesSpread(jsxAttributeValue('prop', emptyComments), emptyComments)],
    children: [
      {
        type: 'JSX_TEXT_BLOCK',
        text: 'some text',
        uniqueID: 'text-uid',
      },
    ],
    uid: 'uid',
  }
  const oldValue: ElementsWithin = {
    uid: innerValue,
  }
  const newSameValue: ElementsWithin = {
    uid: innerValue,
  }
  const newDifferentValue: ElementsWithin = {
    'new-uid': innerValue,
  }

  it('same reference returns the same reference', () => {
    const result = ElementsWithinKeepDeepEqualityCall()(oldValue, oldValue)
    expect(result.value).toBe(oldValue)
    expect(result.areEqual).toEqual(true)
  })
  it('same value returns the same reference', () => {
    const result = ElementsWithinKeepDeepEqualityCall()(oldValue, newSameValue)
    expect(result.value).toBe(oldValue)
    expect(result.areEqual).toEqual(true)
  })
  it('different but similar value handled appropriately', () => {
    const result = ElementsWithinKeepDeepEqualityCall()(oldValue, newDifferentValue)
    expect(result.value['new-uid']).toBe(oldValue['uid'])
    expect(result.value).toEqual(newDifferentValue)
    expect(result.areEqual).toEqual(false)
  })
})

describe('JSXArbitraryBlockKeepDeepEquality', () => {
  const oldValue: JSXArbitraryBlock = {
    type: 'JSX_ARBITRARY_BLOCK',
    originalJavascript: 'old',
    javascript: 'old',
    transpiledJavascript: 'old',
    definedElsewhere: ['old'],
    sourceMap: { a: 1, b: [2] } as any,
    uniqueID: 'old',
    elementsWithin: {},
  }
  const newSameValue: JSXArbitraryBlock = {
    type: 'JSX_ARBITRARY_BLOCK',
    originalJavascript: 'old',
    javascript: 'old',
    transpiledJavascript: 'old',
    definedElsewhere: ['old'],
    sourceMap: { a: 1, b: [2] } as any,
    uniqueID: 'old',
    elementsWithin: {},
  }
  const newDifferentValue: JSXArbitraryBlock = {
    type: 'JSX_ARBITRARY_BLOCK',
    originalJavascript: 'new',
    javascript: 'old',
    transpiledJavascript: 'old',
    definedElsewhere: ['old'],
    sourceMap: { a: 1, b: [2] } as any,
    uniqueID: 'old',
    elementsWithin: {},
  }

  it('same reference returns the same reference', () => {
    const result = JSXArbitraryBlockKeepDeepEquality(oldValue, oldValue)
    expect(result.value).toBe(oldValue)
    expect(result.areEqual).toEqual(true)
  })
  it('same value returns the same reference', () => {
    const result = JSXArbitraryBlockKeepDeepEquality(oldValue, newSameValue)
    expect(result.value).toBe(oldValue)
    expect(result.areEqual).toEqual(true)
  })
  it('different but similar value handled appropriately', () => {
    const result = JSXArbitraryBlockKeepDeepEquality(oldValue, newDifferentValue)
    expect(result.value.type).toBe(oldValue.type)
    expect(result.value.originalJavascript).toBe(newDifferentValue.originalJavascript)
    expect(result.value.javascript).toBe(oldValue.javascript)
    expect(result.value.transpiledJavascript).toBe(oldValue.transpiledJavascript)
    expect(result.value.definedElsewhere).toBe(oldValue.definedElsewhere)
    expect(result.value.sourceMap).toBe(oldValue.sourceMap)
    expect(result.value.uniqueID).toBe(oldValue.uniqueID)
    expect(result.value.elementsWithin).toBe(oldValue.elementsWithin)
    expect(result.value).toEqual(newDifferentValue)
    expect(result.areEqual).toEqual(false)
  })
})

describe('ArbitraryJsBlockKeepDeepEquality', () => {
  const oldValue: ArbitraryJSBlock = {
    type: 'ARBITRARY_JS_BLOCK',
    javascript: 'old',
    transpiledJavascript: 'old',
    definedWithin: ['old'],
    definedElsewhere: ['old'],
    sourceMap: { a: 1, b: [2] } as any,
    uniqueID: 'old',
    elementsWithin: {},
  }
  const newSameValue: ArbitraryJSBlock = {
    type: 'ARBITRARY_JS_BLOCK',
    javascript: 'old',
    transpiledJavascript: 'old',
    definedWithin: ['old'],
    definedElsewhere: ['old'],
    sourceMap: { a: 1, b: [2] } as any,
    uniqueID: 'old',
    elementsWithin: {},
  }
  const newDifferentValue: ArbitraryJSBlock = {
    type: 'ARBITRARY_JS_BLOCK',
    javascript: 'new',
    transpiledJavascript: 'old',
    definedWithin: ['old'],
    definedElsewhere: ['old'],
    sourceMap: { a: 1, b: [2] } as any,
    uniqueID: 'old',
    elementsWithin: {},
  }

  it('same reference returns the same reference', () => {
    const result = ArbitraryJSBlockKeepDeepEquality()(oldValue, oldValue)
    expect(result.value).toBe(oldValue)
    expect(result.areEqual).toEqual(true)
  })
  it('same value returns the same reference', () => {
    const result = ArbitraryJSBlockKeepDeepEquality()(oldValue, newSameValue)
    expect(result.value).toBe(oldValue)
    expect(result.areEqual).toEqual(true)
  })
  it('different but similar value handled appropriately', () => {
    const result = ArbitraryJSBlockKeepDeepEquality()(oldValue, newDifferentValue)
    expect(result.value.type).toBe(oldValue.type)
    expect(result.value.javascript).toBe(newDifferentValue.javascript)
    expect(result.value.transpiledJavascript).toBe(oldValue.transpiledJavascript)
    expect(result.value.definedWithin).toBe(oldValue.definedWithin)
    expect(result.value.definedElsewhere).toBe(oldValue.definedElsewhere)
    expect(result.value.sourceMap).toBe(oldValue.sourceMap)
    expect(result.value.uniqueID).toBe(oldValue.uniqueID)
    expect(result.value.elementsWithin).toBe(oldValue.elementsWithin)
    expect(result.value).toEqual(newDifferentValue)
    expect(result.areEqual).toEqual(false)
  })
})

describe('JSXTextBlockKeepDeepEquality', () => {
  const oldValue: JSXTextBlock = {
    type: 'JSX_TEXT_BLOCK',
    text: 'old',
    uniqueID: 'uid',
  }
  const newSameValue: JSXTextBlock = {
    type: 'JSX_TEXT_BLOCK',
    text: 'old',
    uniqueID: 'uid',
  }
  const newDifferentValue: JSXTextBlock = {
    type: 'JSX_TEXT_BLOCK',
    text: 'new',
    uniqueID: 'uid',
  }

  it('same reference returns the same reference', () => {
    const result = JSXTextBlockKeepDeepEquality(oldValue, oldValue)
    expect(result.value).toBe(oldValue)
    expect(result.areEqual).toEqual(true)
  })
  it('same value returns the same reference', () => {
    const result = JSXTextBlockKeepDeepEquality(oldValue, newSameValue)
    expect(result.value).toBe(oldValue)
    expect(result.areEqual).toEqual(true)
  })
  it('different but similar value handled appropriately', () => {
    const result = JSXTextBlockKeepDeepEquality(oldValue, newDifferentValue)
    expect(result.value.type).toBe(oldValue.type)
    expect(result.value.text).toBe(newDifferentValue.text)
    expect(result.value.uniqueID).toBe(oldValue.uniqueID)
    expect(result.value).toEqual(newDifferentValue)
    expect(result.areEqual).toEqual(false)
  })
})

describe('JSXFragmentKeepDeepEquality', () => {
  const oldValue: JSXFragment = {
    type: 'JSX_FRAGMENT',
    children: [
      {
        type: 'JSX_TEXT_BLOCK',
        text: 'old',
        uniqueID: 'uid',
      },
    ],
    uid: 'uid',
  }
  const newSameValue: JSXFragment = {
    type: 'JSX_FRAGMENT',
    children: [
      {
        type: 'JSX_TEXT_BLOCK',
        text: 'old',
        uniqueID: 'uid',
      },
    ],
    uid: 'uid',
  }
  const newDifferentValue: JSXFragment = {
    type: 'JSX_FRAGMENT',
    children: [
      {
        type: 'JSX_TEXT_BLOCK',
        text: 'old',
        uniqueID: 'uid',
      },
    ],
    uid: 'new-uid',
  }

  it('same reference returns the same reference', () => {
    const result = JSXFragmentKeepDeepEquality(oldValue, oldValue)
    expect(result.value).toBe(oldValue)
    expect(result.areEqual).toEqual(true)
  })
  it('same value returns the same reference', () => {
    const result = JSXFragmentKeepDeepEquality(oldValue, newSameValue)
    expect(result.value).toBe(oldValue)
    expect(result.areEqual).toEqual(true)
  })
  it('different but similar value handled appropriately', () => {
    const result = JSXFragmentKeepDeepEquality(oldValue, newDifferentValue)
    expect(result.value.type).toBe(oldValue.type)
    expect(result.value.children).toBe(oldValue.children)
    expect(result.value.uid).toBe(newDifferentValue.uid)
    expect(result.value).toEqual(newDifferentValue)
    expect(result.areEqual).toEqual(false)
  })
})

describe('JSXElementChildKeepDeepEquality', () => {
  const oldValue: JSXTextBlock = {
    type: 'JSX_TEXT_BLOCK',
    text: 'old',
    uniqueID: 'uid',
  }
  const newSameValue: JSXTextBlock = {
    type: 'JSX_TEXT_BLOCK',
    text: 'old',
    uniqueID: 'uid',
  }
  const newDifferentValue: JSXTextBlock = {
    type: 'JSX_TEXT_BLOCK',
    text: 'new',
    uniqueID: 'uid',
  }

  it('same reference returns the same reference', () => {
    const result = JSXElementChildKeepDeepEquality()(oldValue, oldValue)
    expect(result.value).toBe(oldValue)
    expect(result.areEqual).toEqual(true)
  })
  it('same value returns the same reference', () => {
    const result = JSXElementChildKeepDeepEquality()(oldValue, newSameValue)
    expect(result.value).toBe(oldValue)
    expect(result.areEqual).toEqual(true)
  })
  it('different but similar value handled appropriately', () => {
    const result = JSXElementChildKeepDeepEquality()(oldValue, newDifferentValue)
    expect(result.value.type).toBe(oldValue.type)
    expect((result.value as JSXTextBlock).text).toBe(newDifferentValue.text)
    expect((result.value as JSXTextBlock).uniqueID).toBe(oldValue.uniqueID)
    expect(result.value).toEqual(newDifferentValue)
    expect(result.areEqual).toEqual(false)
  })
})

describe('JSXElementChildArrayKeepDeepEquality', () => {
  const oldValue: Array<JSXTextBlock> = [
    {
      type: 'JSX_TEXT_BLOCK',
      text: 'old',
      uniqueID: 'uid',
    },
  ]
  const newSameValue: Array<JSXTextBlock> = [
    {
      type: 'JSX_TEXT_BLOCK',
      text: 'old',
      uniqueID: 'uid',
    },
  ]
  const newDifferentValue: Array<JSXTextBlock> = [
    {
      type: 'JSX_TEXT_BLOCK',
      text: 'new',
      uniqueID: 'uid',
    },
  ]

  it('same reference returns the same reference', () => {
    const result = JSXElementChildArrayKeepDeepEquality(oldValue, oldValue)
    expect(result.value).toBe(oldValue)
    expect(result.areEqual).toEqual(true)
  })
  it('same value returns the same reference', () => {
    const result = JSXElementChildArrayKeepDeepEquality(oldValue, newSameValue)
    expect(result.value).toBe(oldValue)
    expect(result.areEqual).toEqual(true)
  })
  it('different but similar value handled appropriately', () => {
    const result = JSXElementChildArrayKeepDeepEquality(oldValue, newDifferentValue)
    expect(result.value[0].type).toBe(oldValue[0].type)
    expect((result.value[0] as JSXTextBlock).text).toBe(newDifferentValue[0].text)
    expect((result.value[0] as JSXTextBlock).uniqueID).toBe(oldValue[0].uniqueID)
    expect(result.value).toEqual(newDifferentValue)
    expect(result.areEqual).toEqual(false)
  })
})

describe('RegularParamKeepDeepEquality', () => {
  const oldValue: RegularParam = {
    type: 'REGULAR_PARAM',
    paramName: 'old',
    defaultExpression: null,
  }
  const newSameValue: RegularParam = {
    type: 'REGULAR_PARAM',
    paramName: 'old',
    defaultExpression: null,
  }
  const newDifferentValue: RegularParam = {
    type: 'REGULAR_PARAM',
    paramName: 'new',
    defaultExpression: null,
  }

  it('same reference returns the same reference', () => {
    const result = RegularParamKeepDeepEquality(oldValue, oldValue)
    expect(result.value).toBe(oldValue)
    expect(result.areEqual).toEqual(true)
  })
  it('same value returns the same reference', () => {
    const result = RegularParamKeepDeepEquality(oldValue, newSameValue)
    expect(result.value).toBe(oldValue)
    expect(result.areEqual).toEqual(true)
  })
  it('different but similar value handled appropriately', () => {
    const result = RegularParamKeepDeepEquality(oldValue, newDifferentValue)
    expect(result.value.type).toBe(oldValue.type)
    expect(result.value.paramName).toBe(newDifferentValue.paramName)
    expect(result.value.defaultExpression).toBe(oldValue.defaultExpression)
    expect(result.value).toEqual(newDifferentValue)
    expect(result.areEqual).toEqual(false)
  })
})

describe('DestructuredParamPartKeepDeepEquality', () => {
  const oldValue: DestructuredParamPart = {
    propertyName: 'old',
    param: {
      type: 'PARAM',
      dotDotDotToken: false,
      boundParam: {
        type: 'REGULAR_PARAM',
        paramName: 'param',
        defaultExpression: null,
      },
    },
    defaultExpression: null,
  }
  const newSameValue: DestructuredParamPart = {
    propertyName: 'old',
    param: {
      type: 'PARAM',
      dotDotDotToken: false,
      boundParam: {
        type: 'REGULAR_PARAM',
        paramName: 'param',
        defaultExpression: null,
      },
    },
    defaultExpression: null,
  }
  const newDifferentValue: DestructuredParamPart = {
    propertyName: 'new',
    param: {
      type: 'PARAM',
      dotDotDotToken: false,
      boundParam: {
        type: 'REGULAR_PARAM',
        paramName: 'param',
        defaultExpression: null,
      },
    },
    defaultExpression: null,
  }

  it('same reference returns the same reference', () => {
    const result = DestructuredParamPartKeepDeepEquality(oldValue, oldValue)
    expect(result.value).toBe(oldValue)
    expect(result.areEqual).toEqual(true)
  })
  it('same value returns the same reference', () => {
    const result = DestructuredParamPartKeepDeepEquality(oldValue, newSameValue)
    expect(result.value).toBe(oldValue)
    expect(result.areEqual).toEqual(true)
  })
  it('different but similar value handled appropriately', () => {
    const result = DestructuredParamPartKeepDeepEquality(oldValue, newDifferentValue)
    expect(result.value.propertyName).toBe(newDifferentValue.propertyName)
    expect(result.value.param).toBe(oldValue.param)
    expect(result.value.defaultExpression).toBe(oldValue.defaultExpression)
    expect(result.value).toEqual(newDifferentValue)
    expect(result.areEqual).toEqual(false)
  })
})

describe('DestructuredObjectParamKeepDeepEquality', () => {
  const oldValue: DestructuredObject = {
    type: 'DESTRUCTURED_OBJECT',
    parts: [
      {
        propertyName: 'old',
        param: {
          type: 'PARAM',
          dotDotDotToken: false,
          boundParam: {
            type: 'REGULAR_PARAM',
            paramName: 'param',
            defaultExpression: null,
          },
        },
        defaultExpression: null,
      },
    ],
  }
  const newSameValue: DestructuredObject = {
    type: 'DESTRUCTURED_OBJECT',
    parts: [
      {
        propertyName: 'old',
        param: {
          type: 'PARAM',
          dotDotDotToken: false,
          boundParam: {
            type: 'REGULAR_PARAM',
            paramName: 'param',
            defaultExpression: null,
          },
        },
        defaultExpression: null,
      },
    ],
  }
  const newDifferentValue: DestructuredObject = {
    type: 'DESTRUCTURED_OBJECT',
    parts: [
      {
        propertyName: 'new',
        param: {
          type: 'PARAM',
          dotDotDotToken: false,
          boundParam: {
            type: 'REGULAR_PARAM',
            paramName: 'param',
            defaultExpression: null,
          },
        },
        defaultExpression: null,
      },
    ],
  }

  it('same reference returns the same reference', () => {
    const result = DestructuredObjectParamKeepDeepEquality(oldValue, oldValue)
    expect(result.value).toBe(oldValue)
    expect(result.areEqual).toEqual(true)
  })
  it('same value returns the same reference', () => {
    const result = DestructuredObjectParamKeepDeepEquality(oldValue, newSameValue)
    expect(result.value).toBe(oldValue)
    expect(result.areEqual).toEqual(true)
  })
  it('different but similar value handled appropriately', () => {
    const result = DestructuredObjectParamKeepDeepEquality(oldValue, newDifferentValue)
    expect(result.value.type).toBe(oldValue.type)
    expect(result.value.parts[0].propertyName).toBe(newDifferentValue.parts[0].propertyName)
    expect(result.value.parts[0].param).toBe(oldValue.parts[0].param)
    expect(result.value.parts[0].defaultExpression).toBe(oldValue.parts[0].defaultExpression)
    expect(result.value).toEqual(newDifferentValue)
    expect(result.areEqual).toEqual(false)
  })
})

describe('DestructuredArrayPartKeepDeepEquality', () => {
  const oldValue: DestructuredArrayPart = {
    type: 'PARAM',
    dotDotDotToken: false,
    boundParam: {
      type: 'REGULAR_PARAM',
      paramName: 'param',
      defaultExpression: null,
    },
  }
  const newSameValue: DestructuredArrayPart = {
    type: 'PARAM',
    dotDotDotToken: false,
    boundParam: {
      type: 'REGULAR_PARAM',
      paramName: 'param',
      defaultExpression: null,
    },
  }
  const newDifferentValue: DestructuredArrayPart = {
    type: 'PARAM',
    dotDotDotToken: true,
    boundParam: {
      type: 'REGULAR_PARAM',
      paramName: 'param',
      defaultExpression: null,
    },
  }

  it('same reference returns the same reference', () => {
    const result = DestructuredArrayPartKeepDeepEquality(oldValue, oldValue)
    expect(result.value).toBe(oldValue)
    expect(result.areEqual).toEqual(true)
  })
  it('same value returns the same reference', () => {
    const result = DestructuredArrayPartKeepDeepEquality(oldValue, newSameValue)
    expect(result.value).toBe(oldValue)
    expect(result.areEqual).toEqual(true)
  })
  it('different but similar value handled appropriately', () => {
    const result = DestructuredArrayPartKeepDeepEquality(oldValue, newDifferentValue)
    expect(result.value.type).toBe(oldValue.type)
    expect((result.value as Param).dotDotDotToken).toBe(newDifferentValue.dotDotDotToken)
    expect((result.value as Param).boundParam).toBe(oldValue.boundParam)
    expect(result.value).toEqual(newDifferentValue)
    expect(result.areEqual).toEqual(false)
  })
})

describe('DestructuredArrayKeepDeepEquality', () => {
  const oldValue: DestructuredArray = {
    type: 'DESTRUCTURED_ARRAY',
    parts: [
      {
        type: 'PARAM',
        dotDotDotToken: false,
        boundParam: {
          type: 'REGULAR_PARAM',
          paramName: 'param',
          defaultExpression: null,
        },
      },
    ],
  }
  const newSameValue: DestructuredArray = {
    type: 'DESTRUCTURED_ARRAY',
    parts: [
      {
        type: 'PARAM',
        dotDotDotToken: false,
        boundParam: {
          type: 'REGULAR_PARAM',
          paramName: 'param',
          defaultExpression: null,
        },
      },
    ],
  }
  const newDifferentValue: DestructuredArray = {
    type: 'DESTRUCTURED_ARRAY',
    parts: [
      {
        type: 'PARAM',
        dotDotDotToken: true,
        boundParam: {
          type: 'REGULAR_PARAM',
          paramName: 'param',
          defaultExpression: null,
        },
      },
    ],
  }

  it('same reference returns the same reference', () => {
    const result = DestructuredArrayKeepDeepEquality(oldValue, oldValue)
    expect(result.value).toBe(oldValue)
    expect(result.areEqual).toEqual(true)
  })
  it('same value returns the same reference', () => {
    const result = DestructuredArrayKeepDeepEquality(oldValue, newSameValue)
    expect(result.value).toBe(oldValue)
    expect(result.areEqual).toEqual(true)
  })
  it('different but similar value handled appropriately', () => {
    const result = DestructuredArrayKeepDeepEquality(oldValue, newDifferentValue)
    expect(result.value.type).toBe(oldValue.type)
    expect((result.value.parts[0] as Param).dotDotDotToken).toBe(
      (newDifferentValue.parts[0] as Param).dotDotDotToken,
    )
    expect((result.value.parts[0] as Param).boundParam).toBe(
      (oldValue.parts[0] as Param).boundParam,
    )
    expect(result.value).toEqual(newDifferentValue)
    expect(result.areEqual).toEqual(false)
  })
})

describe('BoundParamKeepDeepEquality', () => {
  const oldValue: RegularParam = {
    type: 'REGULAR_PARAM',
    paramName: 'param',
    defaultExpression: null,
  }
  const newSameValue: RegularParam = {
    type: 'REGULAR_PARAM',
    paramName: 'param',
    defaultExpression: null,
  }
  const newDifferentValue: RegularParam = {
    type: 'REGULAR_PARAM',
    paramName: 'new-param',
    defaultExpression: null,
  }

  it('same reference returns the same reference', () => {
    const result = BoundParamKeepDeepEquality()(oldValue, oldValue)
    expect(result.value).toBe(oldValue)
    expect(result.areEqual).toEqual(true)
  })
  it('same value returns the same reference', () => {
    const result = BoundParamKeepDeepEquality()(oldValue, newSameValue)
    expect(result.value).toBe(oldValue)
    expect(result.areEqual).toEqual(true)
  })
  it('different but similar value handled appropriately', () => {
    const result = BoundParamKeepDeepEquality()(oldValue, newDifferentValue)
    expect(result.value.type).toBe(oldValue.type)
    expect((result.value as RegularParam).paramName).toBe(newDifferentValue.paramName)
    expect((result.value as RegularParam).defaultExpression).toBe(oldValue.defaultExpression)
    expect(result.value).toEqual(newDifferentValue)
    expect(result.areEqual).toEqual(false)
  })
})

describe('ParamKeepDeepEquality', () => {
  const oldValue: Param = {
    type: 'PARAM',
    dotDotDotToken: false,
    boundParam: {
      type: 'REGULAR_PARAM',
      paramName: 'param',
      defaultExpression: null,
    },
  }
  const newSameValue: Param = {
    type: 'PARAM',
    dotDotDotToken: false,
    boundParam: {
      type: 'REGULAR_PARAM',
      paramName: 'param',
      defaultExpression: null,
    },
  }
  const newDifferentValue: Param = {
    type: 'PARAM',
    dotDotDotToken: true,
    boundParam: {
      type: 'REGULAR_PARAM',
      paramName: 'param',
      defaultExpression: null,
    },
  }

  it('same reference returns the same reference', () => {
    const result = ParamKeepDeepEquality()(oldValue, oldValue)
    expect(result.value).toBe(oldValue)
    expect(result.areEqual).toEqual(true)
  })
  it('same value returns the same reference', () => {
    const result = ParamKeepDeepEquality()(oldValue, newSameValue)
    expect(result.value).toBe(oldValue)
    expect(result.areEqual).toEqual(true)
  })
  it('different but similar value handled appropriately', () => {
    const result = ParamKeepDeepEquality()(oldValue, newDifferentValue)
    expect(result.value.type).toBe(oldValue.type)
    expect((result.value as Param).dotDotDotToken).toBe((newDifferentValue as Param).dotDotDotToken)
    expect((result.value as Param).boundParam).toBe((oldValue as Param).boundParam)
    expect(result.value).toEqual(newDifferentValue)
    expect(result.areEqual).toEqual(false)
  })
})

describe('UtopiaJSXComponentKeepDeepEquality', () => {
  const oldValue: UtopiaJSXComponent = {
    type: 'UTOPIA_JSX_COMPONENT',
    name: 'old',
    isFunction: true,
    declarationSyntax: 'const',
    blockOrExpression: 'block',
    param: {
      type: 'PARAM',
      dotDotDotToken: false,
      boundParam: {
        type: 'REGULAR_PARAM',
        paramName: 'props',
        defaultExpression: null,
      },
    },
    propsUsed: [],
    rootElement: {
      type: 'JSX_ELEMENT',
      name: {
        baseVariable: 'React',
        propertyPath: {
          propertyElements: ['style', 'backgroundColor'],
        },
      },
      props: [jsxAttributesSpread(jsxAttributeValue('prop', emptyComments), emptyComments)],
      children: [
        {
          type: 'JSX_TEXT_BLOCK',
          text: 'some text',
          uniqueID: 'text-uid',
        },
      ],
      uid: 'uid',
    },
    arbitraryJSBlock: {
      type: 'ARBITRARY_JS_BLOCK',
      javascript: 'old',
      transpiledJavascript: 'old',
      definedWithin: ['old'],
      definedElsewhere: ['old'],
      sourceMap: { a: 1, b: [2] } as any,
      uniqueID: 'old',
      elementsWithin: {},
    },
    usedInReactDOMRender: false,
    returnStatementComments: emptyComments,
  }
  const newSameValue: UtopiaJSXComponent = {
    type: 'UTOPIA_JSX_COMPONENT',
    name: 'old',
    isFunction: true,
    declarationSyntax: 'const',
    blockOrExpression: 'block',
    param: {
      type: 'PARAM',
      dotDotDotToken: false,
      boundParam: {
        type: 'REGULAR_PARAM',
        paramName: 'props',
        defaultExpression: null,
      },
    },
    propsUsed: [],
    rootElement: {
      type: 'JSX_ELEMENT',
      name: {
        baseVariable: 'React',
        propertyPath: {
          propertyElements: ['style', 'backgroundColor'],
        },
      },
      props: [jsxAttributesSpread(jsxAttributeValue('prop', emptyComments), emptyComments)],
      children: [
        {
          type: 'JSX_TEXT_BLOCK',
          text: 'some text',
          uniqueID: 'text-uid',
        },
      ],
      uid: 'uid',
    },
    arbitraryJSBlock: {
      type: 'ARBITRARY_JS_BLOCK',
      javascript: 'old',
      transpiledJavascript: 'old',
      definedWithin: ['old'],
      definedElsewhere: ['old'],
      sourceMap: { a: 1, b: [2] } as any,
      uniqueID: 'old',
      elementsWithin: {},
    },
    usedInReactDOMRender: false,
    returnStatementComments: emptyComments,
  }
  const newDifferentValue: UtopiaJSXComponent = {
    type: 'UTOPIA_JSX_COMPONENT',
    name: 'new',
    isFunction: true,
    declarationSyntax: 'const',
    blockOrExpression: 'block',
    param: {
      type: 'PARAM',
      dotDotDotToken: false,
      boundParam: {
        type: 'REGULAR_PARAM',
        paramName: 'props',
        defaultExpression: null,
      },
    },
    propsUsed: [],
    rootElement: {
      type: 'JSX_ELEMENT',
      name: {
        baseVariable: 'React',
        propertyPath: {
          propertyElements: ['style', 'backgroundColor'],
        },
      },
      props: [jsxAttributesSpread(jsxAttributeValue('prop', emptyComments), emptyComments)],
      children: [
        {
          type: 'JSX_TEXT_BLOCK',
          text: 'some text',
          uniqueID: 'text-uid',
        },
      ],
      uid: 'uid',
    },
    arbitraryJSBlock: {
      type: 'ARBITRARY_JS_BLOCK',
      javascript: 'old',
      transpiledJavascript: 'old',
      definedWithin: ['old'],
      definedElsewhere: ['old'],
      sourceMap: { a: 1, b: [2] } as any,
      uniqueID: 'old',
      elementsWithin: {},
    },
    usedInReactDOMRender: false,
    returnStatementComments: emptyComments,
  }

  it('same reference returns the same reference', () => {
    const result = UtopiaJSXComponentKeepDeepEquality(oldValue, oldValue)
    expect(result.value).toBe(oldValue)
    expect(result.areEqual).toEqual(true)
  })
  it('same value returns the same reference', () => {
    const result = UtopiaJSXComponentKeepDeepEquality(oldValue, newSameValue)
    expect(result.value).toBe(oldValue)
    expect(result.areEqual).toEqual(true)
  })
  it('different but similar value handled appropriately', () => {
    const result = UtopiaJSXComponentKeepDeepEquality(oldValue, newDifferentValue)
    expect(result.value.type).toBe(oldValue.type)
    expect(result.value.name).toBe(newDifferentValue.name)
    expect(result.value.isFunction).toBe(oldValue.isFunction)
    expect(result.value.declarationSyntax).toBe(oldValue.declarationSyntax)
    expect(result.value.blockOrExpression).toBe(oldValue.blockOrExpression)
    expect(result.value.param).toBe(oldValue.param)
    expect(result.value.propsUsed).toBe(oldValue.propsUsed)
    expect(result.value.rootElement).toBe(oldValue.rootElement)
    expect(result.value.arbitraryJSBlock).toBe(oldValue.arbitraryJSBlock)
    expect(result.value.usedInReactDOMRender).toBe(oldValue.usedInReactDOMRender)
    expect(result.value.returnStatementComments).toBe(oldValue.returnStatementComments)
    expect(result.value).toEqual(newDifferentValue)
    expect(result.areEqual).toEqual(false)
  })
})
