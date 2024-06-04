import type {
  CheckboxControlDescription,
  ColorControlDescription,
  NoneControlDescription,
  NumberInputControlDescription,
  RadioControlDescription,
  PopUpListControlDescription,
  StringInputControlDescription,
  ArrayControlDescription,
  ObjectControlDescription,
  UnionControlDescription,
  RegularControlDescription,
  ExpressionInputControlDescription,
} from '../../components/custom-code/internal-property-controls'
import type { JSExpression } from '../shared/element-template'
import {
  jsxArrayValue,
  jsExpressionValue,
  jsExpressionOtherJavaScript,
  modifiableAttributeIsAttributeOtherJavaScript,
  clearExpressionUniqueIDs,
  jsExpressionNestedArray,
  jsExpressionNestedObject,
  jsxPropertyAssignment,
  emptyComments,
  jsOpaqueArbitraryStatement,
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
  wrappedValidValue: JSExpression,
  invalidWrappedValidValues: JSExpression[],
  control: RegularControlDescription,
) {
  it('Unwraps and parses a valid wrapped value', () => {
    const unwrapAndParse = unwrapperAndParserForPropertyControl(control)
    const unwrappedValue = unwrapAndParse(right(wrappedValidValue), undefined)
    expect(unwrappedValue).toEqual(right(validValue))
  })

  it('Prints a valid value', () => {
    const print = printerForPropertyControl(control)
    const printedValue = clearExpressionUniqueIDs(print(validValue))
    expect(printedValue).toEqual(clearExpressionUniqueIDs(wrappedValidValue))
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

describe('CheckboxControlDescription', () => {
  const checkboxControlDescriptionValue: CheckboxControlDescription = {
    control: 'checkbox',
  }

  const validValue = true
  const wrappedValidValue = jsExpressionValue(validValue, emptyComments)
  const wrappedInvalidValues = [
    jsExpressionValue('hat', emptyComments),
    jsExpressionValue(0, emptyComments),
  ]

  runBaseTestSuite(
    validValue,
    wrappedValidValue,
    wrappedInvalidValues,
    checkboxControlDescriptionValue,
  )
})

describe('ColorControlDescription', () => {
  const colorControlDescriptionValue: ColorControlDescription = {
    control: 'color',
  }

  const validValueAsString = '#FFFFFFFF'
  const validValue = cssColor(validValueAsString)
  const wrappedValidValue = jsExpressionValue(validValueAsString, emptyComments)
  const wrappedInvalidValues = [
    jsExpressionValue('hat', emptyComments),
    jsExpressionValue(0, emptyComments),
  ]

  runBaseTestSuite(
    validValue,
    wrappedValidValue,
    wrappedInvalidValues,
    colorControlDescriptionValue,
  )
})

describe('ExpressionInputControlDescription', () => {
  const expressionInputControlDescriptionValue: ExpressionInputControlDescription = {
    control: 'expression-input',
  }

  const validValue = 'Cake'
  const wrappedValidValue = jsExpressionOtherJavaScript(
    [],
    validValue,
    validValue,
    ``,
    [],
    null,
    {},
    emptyComments,
  )

  runBaseTestSuite(validValue, wrappedValidValue, [], expressionInputControlDescriptionValue)
})

describe('PopUpListControlDescription', () => {
  const validValue = 'Cake'

  const popUpListControlDescriptionValue: PopUpListControlDescription = {
    control: 'popuplist',
    options: [validValue],
  }

  const wrappedValidValue = jsExpressionValue(validValue, emptyComments)

  runBaseTestSuite(validValue, wrappedValidValue, [], popUpListControlDescriptionValue)
})

describe('NoneControlDescription', () => {
  const noneControlDescriptionValue: NoneControlDescription = {
    control: 'none',
  }

  const validValue = 'Cake'
  const wrappedValidValue = jsExpressionValue(validValue, emptyComments)

  runBaseTestSuite(validValue, wrappedValidValue, [], noneControlDescriptionValue)
})

describe('NumberInputControlDescription', () => {
  const numberInputControlDescriptionValue: NumberInputControlDescription = {
    control: 'number-input',
  }

  const validValue = 0
  const wrappedValidValue = jsExpressionValue(validValue, emptyComments)
  const wrappedInvalidValues = [jsExpressionValue('hat', emptyComments)]

  runBaseTestSuite(
    validValue,
    wrappedValidValue,
    wrappedInvalidValues,
    numberInputControlDescriptionValue,
  )
})

describe('RadioControlDescription', () => {
  const validValue = 'selected'

  const radioControlDescriptionValue: RadioControlDescription = {
    control: 'radio',
    options: [
      {
        type: 'control-option-with-icon',
        option: {
          value: validValue,
          label: 'Label',
          icon: null,
        },
      },
    ],
  }

  const wrappedValidValue = jsExpressionValue(validValue, emptyComments)

  runBaseTestSuite(validValue, wrappedValidValue, [], radioControlDescriptionValue)
})

describe('StringInputControlDescription', () => {
  const stringInputControlDescriptionValue: StringInputControlDescription = {
    control: 'string-input',
  }

  const validValue = 'hat'
  const wrappedValidValue = jsExpressionValue(validValue, emptyComments)
  const wrappedInvalidValues = [jsExpressionValue(0, emptyComments)]

  runBaseTestSuite(
    validValue,
    wrappedValidValue,
    wrappedInvalidValues,
    stringInputControlDescriptionValue,
  )
})

describe('ArrayControlDescription - simple', () => {
  const simpleArrayControlDescriptionValue: ArrayControlDescription = {
    control: 'array',
    propertyControl: {
      control: 'string-input',
    },
  }

  const simpleValidContents = 'hat'
  const simpleValidValue = [simpleValidContents]
  const simpleWrappedValidValue = jsExpressionNestedArray(
    [jsxArrayValue(jsExpressionValue(simpleValidContents, emptyComments), emptyComments)],
    emptyComments,
  )
  const simpleWrappedInvalidValues = [
    jsExpressionNestedArray(
      [jsxArrayValue(jsExpressionValue(0, emptyComments), emptyComments)],
      emptyComments,
    ),
  ]

  runBaseTestSuite(
    simpleValidValue,
    simpleWrappedValidValue,
    simpleWrappedInvalidValues,
    simpleArrayControlDescriptionValue,
  )
})

describe('ArrayControlDescription - complex', () => {
  const complexArrayControlDescriptionValue: ArrayControlDescription = {
    control: 'array',
    propertyControl: {
      control: 'array',
      propertyControl: {
        control: 'string-input',
      },
    },
  }

  const complexValidContents = 'hat'
  const complexValidValue = [[complexValidContents]]
  const complexWrappedValidValue = jsExpressionNestedArray(
    [
      jsxArrayValue(
        jsExpressionNestedArray(
          [jsxArrayValue(jsExpressionValue(complexValidContents, emptyComments), emptyComments)],
          emptyComments,
        ),
        emptyComments,
      ),
    ],
    emptyComments,
  )
  const complexWrappedInvalidValues = [
    jsExpressionNestedArray(
      [
        jsxArrayValue(
          jsExpressionNestedArray(
            [jsxArrayValue(jsExpressionValue(0, emptyComments), emptyComments)],
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

describe('ObjectControlDescription - simple', () => {
  const simpleValidKey = 'simple'
  const simpleObjectControlDescriptionValue: ObjectControlDescription = {
    control: 'object',
    object: {
      [simpleValidKey]: {
        control: 'string-input',
      },
    },
  }

  const simpleValidContents = 'hat'
  const simpleValidValue = { [simpleValidKey]: simpleValidContents }
  const simpleWrappedValidValue = jsExpressionNestedObject(
    [
      jsxPropertyAssignment(
        simpleValidKey,
        jsExpressionValue(simpleValidContents, emptyComments),
        emptyComments,
        emptyComments,
      ),
    ],
    emptyComments,
  )
  const simpleWrappedInvalidValues = [
    jsExpressionNestedObject(
      [
        jsxPropertyAssignment(
          simpleValidKey,
          jsExpressionValue(0, emptyComments),
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
})

describe('ObjectControlDescription - complex', () => {
  const simpleValidKey = 'simple'
  const simpleValidContents = 'hat'
  const complexValidKey = 'complexValidKey'
  const complexObjectControlDescriptionValue: ObjectControlDescription = {
    control: 'object',
    object: {
      [complexValidKey]: {
        control: 'object',
        object: {
          [simpleValidKey]: {
            control: 'string-input',
          },
        },
      },
    },
  }

  const complexValidValue = { [complexValidKey]: { [simpleValidKey]: simpleValidContents } }
  const complexWrappedValidValue = jsExpressionNestedObject(
    [
      jsxPropertyAssignment(
        complexValidKey,
        jsExpressionNestedObject(
          [
            jsxPropertyAssignment(
              simpleValidKey,
              jsExpressionValue(simpleValidContents, emptyComments),
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
    jsExpressionNestedObject(
      [
        jsxPropertyAssignment(
          complexValidKey,
          jsExpressionNestedObject(
            [
              jsxPropertyAssignment(
                simpleValidKey,
                jsExpressionValue(0, emptyComments),
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

describe('UnionControlDescription - simple', () => {
  const simpleUnionControlDescriptionValue: UnionControlDescription = {
    control: 'union',
    controls: [
      {
        control: 'string-input',
      },
      {
        control: 'number-input',
      },
    ],
  }

  const simpleValidValue = 10
  const simpleWrappedValidValue = jsExpressionValue(simpleValidValue, emptyComments)
  const simpleWrappedInvalidValues = [jsExpressionValue(false, emptyComments)]

  runBaseTestSuite(
    simpleValidValue,
    simpleWrappedValidValue,
    simpleWrappedInvalidValues,
    simpleUnionControlDescriptionValue,
  )
})

describe('UnionControlDescription - complex', () => {
  const complexUnionControlDescriptionValue: UnionControlDescription = {
    control: 'union',
    controls: [
      {
        control: 'string-input',
      },
      {
        control: 'array',
        propertyControl: {
          control: 'string-input',
        },
      },
    ],
  }

  const complexValidContents = 'hat'
  const complexValidValue = [complexValidContents]
  const complexWrappedValidValue = jsExpressionNestedArray(
    [jsxArrayValue(jsExpressionValue(complexValidContents, emptyComments), emptyComments)],
    emptyComments,
  )
  const complexWrappedInvalidValues = [
    jsExpressionNestedArray(
      [jsxArrayValue(jsExpressionValue(0, emptyComments), emptyComments)],
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
