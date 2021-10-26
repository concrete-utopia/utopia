import {
  ControlDescription,
  BooleanControlDescription,
  ColorControlDescription,
  EnumControlDescription,
  IgnoreControlDescription,
  NumberControlDescription,
  OptionsControlDescription,
  PopUpListControlDescription,
  StringControlDescription,
  ArrayControlDescription,
  ObjectControlDescription,
  UnionControlDescription,
  RegularControlDescription,
  RawJSControlDescription,
} from 'utopia-api'
import {
  JSXAttribute,
  jsxArrayValue,
  jsxAttributeValue,
  jsxAttributeOtherJavaScript,
  isJSXAttributeOtherJavaScript,
  clearAttributeUniqueIDs,
  jsxAttributeNestedArray,
  jsxAttributeNestedObject,
  jsxPropertyAssignment,
  emptyComments,
} from '../shared/element-template'
import {
  unwrapperAndParserForPropertyControl,
  printerForPropertyControl,
} from './property-control-values'
import { right, isLeft } from '../shared/either'
import { fastForEach } from '../shared/utils'
import { cssColor } from '../../components/inspector/common/css-utils'

function runBaseTestSuite<T>(
  validValue: T,
  wrappedValidValue: JSXAttribute,
  invalidWrappedValidValues: JSXAttribute[],
  control: RegularControlDescription,
) {
  it('Unwraps and parses a valid wrapped value', () => {
    const unwrapAndParse = unwrapperAndParserForPropertyControl(control)
    const unwrappedValue = unwrapAndParse(right(wrappedValidValue), undefined)
    expect(unwrappedValue).toEqual(right(validValue))
  })

  it('Prints a valid value', () => {
    const print = printerForPropertyControl(control)
    const printedValue = clearAttributeUniqueIDs(print(validValue))
    expect(printedValue).toEqual(clearAttributeUniqueIDs(wrappedValidValue))
  })

  it('Fails to unwrap and parse an invalid value', () => {
    fastForEach(invalidWrappedValidValues, (invalidWrappedValidValue) => {
      const unwrapAndParse = unwrapperAndParserForPropertyControl(control)
      const unwrappedValue = unwrapAndParse(right(invalidWrappedValidValue), undefined)
      expect(isLeft(unwrappedValue)).toBeTruthy()
    })
  })

  it('Prints and unwraps a valid value', () => {
    const print = printerForPropertyControl(control)
    const printedValue = print(validValue)
    const unwrapAndParse = unwrapperAndParserForPropertyControl(control)
    const unwrappedValue = unwrapAndParse(right(printedValue), undefined)
    expect(unwrappedValue).toEqual(right(validValue))
  })
}

describe('BooleanControlDescription', () => {
  const booleanControlDescriptionValue: BooleanControlDescription = {
    type: 'boolean',
  }

  const validValue = true
  const wrappedValidValue = jsxAttributeValue(validValue, emptyComments)
  const wrappedInvalidValues = [
    jsxAttributeValue('hat', emptyComments),
    jsxAttributeValue(0, emptyComments),
  ]

  runBaseTestSuite(
    validValue,
    wrappedValidValue,
    wrappedInvalidValues,
    booleanControlDescriptionValue,
  )
})

describe('ColorControlDescription', () => {
  const colorControlDescriptionValue: ColorControlDescription = {
    type: 'color',
  }

  const validValueAsString = '#FFFFFFFF'
  const validValue = cssColor(validValueAsString)
  const wrappedValidValue = jsxAttributeValue(validValueAsString, emptyComments)
  const wrappedInvalidValues = [
    jsxAttributeValue('hat', emptyComments),
    jsxAttributeValue(0, emptyComments),
  ]

  runBaseTestSuite(
    validValue,
    wrappedValidValue,
    wrappedInvalidValues,
    colorControlDescriptionValue,
  )
})

describe('RawJSControlDescription', () => {
  const rawJSControlDescriptionValue: RawJSControlDescription = {
    type: 'rawjs',
  }

  const validValue = 'Cake'
  const wrappedValidValue = jsxAttributeOtherJavaScript(validValue, ``, [], null, {})

  runBaseTestSuite(validValue, wrappedValidValue, [], rawJSControlDescriptionValue)
})

describe('EnumControlDescription', () => {
  const validValue = 'Cake'

  const enumControlDescriptionValue: EnumControlDescription = {
    type: 'enum',
    options: [validValue],
  }

  const wrappedValidValue = jsxAttributeValue(validValue, emptyComments)
  const wrappedInvalidValues = [
    jsxAttributeValue('hat', emptyComments),
    jsxAttributeValue(0, emptyComments),
  ]

  runBaseTestSuite(validValue, wrappedValidValue, wrappedInvalidValues, enumControlDescriptionValue)
})

describe('IgnoreControlDescription', () => {
  const IgnoreControlDescriptionValue: IgnoreControlDescription = {
    type: 'ignore',
  }

  const validValue = 'Cake'
  const wrappedValidValue = jsxAttributeValue(validValue, emptyComments)

  runBaseTestSuite(validValue, wrappedValidValue, [], IgnoreControlDescriptionValue)
})

describe('NumberControlDescription', () => {
  const numberControlDescriptionValue: NumberControlDescription = {
    type: 'number',
  }

  const validValue = 0
  const wrappedValidValue = jsxAttributeValue(validValue, emptyComments)
  const wrappedInvalidValues = [jsxAttributeValue('hat', emptyComments)]

  runBaseTestSuite(
    validValue,
    wrappedValidValue,
    wrappedInvalidValues,
    numberControlDescriptionValue,
  )
})

describe('OptionsControlDescription', () => {
  const validValue = 'selected'

  const optionsControlDescriptionValue: OptionsControlDescription = {
    type: 'options',
    options: [
      {
        value: validValue,
        label: 'Label',
      },
    ],
  }

  const wrappedValidValue = jsxAttributeValue(validValue, emptyComments)

  runBaseTestSuite(validValue, wrappedValidValue, [], optionsControlDescriptionValue)
})

describe('PopupListControlDescription', () => {
  const validValue = 'selected'

  const PopupListControlDescriptionValue: PopUpListControlDescription = {
    type: 'popuplist',
    options: [
      {
        value: validValue,
        label: 'Label',
      },
    ],
  }

  const wrappedValidValue = jsxAttributeValue(validValue, emptyComments)

  runBaseTestSuite(validValue, wrappedValidValue, [], PopupListControlDescriptionValue)
})

describe('StringControlDescription', () => {
  const stringControlDescriptionValue: StringControlDescription = {
    type: 'string',
  }

  const validValue = 'hat'
  const wrappedValidValue = jsxAttributeValue(validValue, emptyComments)
  const wrappedInvalidValues = [jsxAttributeValue(0, emptyComments)]

  runBaseTestSuite(
    validValue,
    wrappedValidValue,
    wrappedInvalidValues,
    stringControlDescriptionValue,
  )
})

describe('StringControlDescription', () => {
  const stringControlDescriptionValue: StringControlDescription = {
    type: 'string',
  }

  const validValue = 'hat'
  const wrappedValidValue = jsxAttributeValue(validValue, emptyComments)
  const wrappedInvalidValues = [jsxAttributeValue(0, emptyComments)]

  runBaseTestSuite(
    validValue,
    wrappedValidValue,
    wrappedInvalidValues,
    stringControlDescriptionValue,
  )
})

describe('ArrayControlDescription', () => {
  const simpleArrayControlDescriptionValue: ArrayControlDescription = {
    type: 'array',
    propertyControl: {
      type: 'string',
    },
  }

  const simpleValidContents = 'hat'
  const simpleValidValue = [simpleValidContents]
  const simpleWrappedValidValue = jsxAttributeNestedArray(
    [jsxArrayValue(jsxAttributeValue(simpleValidContents, emptyComments), emptyComments)],
    emptyComments,
  )
  const simpleWrappedInvalidValues = [
    jsxAttributeNestedArray(
      [jsxArrayValue(jsxAttributeValue(0, emptyComments), emptyComments)],
      emptyComments,
    ),
  ]

  runBaseTestSuite(
    simpleValidValue,
    simpleWrappedValidValue,
    simpleWrappedInvalidValues,
    simpleArrayControlDescriptionValue,
  )

  const complexArrayControlDescriptionValue: ArrayControlDescription = {
    type: 'array',
    propertyControl: {
      type: 'array',
      propertyControl: {
        type: 'string',
      },
    },
  }

  const complexValidContents = 'hat'
  const complexValidValue = [[complexValidContents]]
  const complexWrappedValidValue = jsxAttributeNestedArray(
    [
      jsxArrayValue(
        jsxAttributeNestedArray(
          [jsxArrayValue(jsxAttributeValue(complexValidContents, emptyComments), emptyComments)],
          emptyComments,
        ),
        emptyComments,
      ),
    ],
    emptyComments,
  )
  const complexWrappedInvalidValues = [
    jsxAttributeNestedArray(
      [
        jsxArrayValue(
          jsxAttributeNestedArray(
            [jsxArrayValue(jsxAttributeValue(0, emptyComments), emptyComments)],
            emptyComments,
          ),
          emptyComments,
        ),
      ],
      emptyComments,
    ),
  ]

  runBaseTestSuite(
    complexValidValue,
    complexWrappedValidValue,
    complexWrappedInvalidValues,
    complexArrayControlDescriptionValue,
  )
})

describe('ObjectControlDescription', () => {
  const simpleValidKey = 'simple'
  const simpleObjectControlDescriptionValue: ObjectControlDescription = {
    type: 'object',
    object: {
      [simpleValidKey]: {
        type: 'string',
      },
    },
  }

  const simpleValidContents = 'hat'
  const simpleValidValue = { [simpleValidKey]: simpleValidContents }
  const simpleWrappedValidValue = jsxAttributeNestedObject(
    [
      jsxPropertyAssignment(
        simpleValidKey,
        jsxAttributeValue(simpleValidContents, emptyComments),
        emptyComments,
        emptyComments,
      ),
    ],
    emptyComments,
  )
  const simpleWrappedInvalidValues = [
    jsxAttributeNestedObject(
      [
        jsxPropertyAssignment(
          simpleValidKey,
          jsxAttributeValue(0, emptyComments),
          emptyComments,
          emptyComments,
        ),
      ],
      emptyComments,
    ),
  ]

  runBaseTestSuite(
    simpleValidValue,
    simpleWrappedValidValue,
    simpleWrappedInvalidValues,
    simpleObjectControlDescriptionValue,
  )

  const complexValidKey = 'complexValidKey'
  const complexObjectControlDescriptionValue: ObjectControlDescription = {
    type: 'object',
    object: {
      [complexValidKey]: {
        type: 'object',
        object: {
          [simpleValidKey]: {
            type: 'string',
          },
        },
      },
    },
  }

  const complexValidValue = { [complexValidKey]: { [simpleValidKey]: simpleValidContents } }
  const complexWrappedValidValue = jsxAttributeNestedObject(
    [
      jsxPropertyAssignment(
        complexValidKey,
        jsxAttributeNestedObject(
          [
            jsxPropertyAssignment(
              simpleValidKey,
              jsxAttributeValue(simpleValidContents, emptyComments),
              emptyComments,
              emptyComments,
            ),
          ],
          emptyComments,
        ),
        emptyComments,
        emptyComments,
      ),
    ],
    emptyComments,
  )
  const complexWrappedInvalidValues = [
    jsxAttributeNestedObject(
      [
        jsxPropertyAssignment(
          complexValidKey,
          jsxAttributeNestedObject(
            [
              jsxPropertyAssignment(
                simpleValidKey,
                jsxAttributeValue(0, emptyComments),
                emptyComments,
                emptyComments,
              ),
            ],
            emptyComments,
          ),
          emptyComments,
          emptyComments,
        ),
      ],
      emptyComments,
    ),
  ]

  runBaseTestSuite(
    complexValidValue,
    complexWrappedValidValue,
    complexWrappedInvalidValues,
    complexObjectControlDescriptionValue,
  )
})

describe('UnionControlDescription', () => {
  const simpleUnionControlDescriptionValue: UnionControlDescription = {
    type: 'union',
    controls: [
      {
        type: 'string',
      },
      {
        type: 'number',
      },
    ],
  }

  const simpleValidValue = 10
  const simpleWrappedValidValue = jsxAttributeValue(simpleValidValue, emptyComments)
  const simpleWrappedInvalidValues = [jsxAttributeValue(false, emptyComments)]

  runBaseTestSuite(
    simpleValidValue,
    simpleWrappedValidValue,
    simpleWrappedInvalidValues,
    simpleUnionControlDescriptionValue,
  )

  const complexUnionControlDescriptionValue: UnionControlDescription = {
    type: 'union',
    controls: [
      {
        type: 'string',
      },
      {
        type: 'array',
        propertyControl: {
          type: 'string',
        },
      },
    ],
  }

  const complexValidContents = 'hat'
  const complexValidValue = [complexValidContents]
  const complexWrappedValidValue = jsxAttributeNestedArray(
    [jsxArrayValue(jsxAttributeValue(complexValidContents, emptyComments), emptyComments)],
    emptyComments,
  )
  const complexWrappedInvalidValues = [
    jsxAttributeNestedArray(
      [jsxArrayValue(jsxAttributeValue(0, emptyComments), emptyComments)],
      emptyComments,
    ),
  ]

  runBaseTestSuite(
    complexValidValue,
    complexWrappedValidValue,
    complexWrappedInvalidValues,
    complexUnionControlDescriptionValue,
  )
})
