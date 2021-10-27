import {
  ControlDescription,
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

describe('CheckboxControlDescription', () => {
  const checkboxControlDescriptionValue: CheckboxControlDescription = {
    control: 'checkbox',
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
    checkboxControlDescriptionValue,
  )
})

describe('ColorControlDescription', () => {
  const colorControlDescriptionValue: ColorControlDescription = {
    control: 'color',
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

describe('ExpressionInputControlDescription', () => {
  const expressionInputControlDescriptionValue: ExpressionInputControlDescription = {
    control: 'expression-input',
  }

  const validValue = 'Cake'
  const wrappedValidValue = jsxAttributeOtherJavaScript(validValue, ``, [], null, {})

  runBaseTestSuite(validValue, wrappedValidValue, [], expressionInputControlDescriptionValue)
})

describe('PopUpListControlDescription', () => {
  const validValue = 'Cake'

  const popUpListControlDescriptionValue: PopUpListControlDescription = {
    control: 'popuplist',
    options: [validValue],
  }

  const wrappedValidValue = jsxAttributeValue(validValue, emptyComments)
  const wrappedInvalidValues = [
    jsxAttributeValue('hat', emptyComments),
    jsxAttributeValue(0, emptyComments),
  ]

  runBaseTestSuite(
    validValue,
    wrappedValidValue,
    wrappedInvalidValues,
    popUpListControlDescriptionValue,
  )
})

describe('NoneControlDescription', () => {
  const noneControlDescriptionValue: NoneControlDescription = {
    control: 'none',
  }

  const validValue = 'Cake'
  const wrappedValidValue = jsxAttributeValue(validValue, emptyComments)

  runBaseTestSuite(validValue, wrappedValidValue, [], noneControlDescriptionValue)
})

describe('NumberInputControlDescription', () => {
  const numberInputControlDescriptionValue: NumberInputControlDescription = {
    control: 'number-input',
  }

  const validValue = 0
  const wrappedValidValue = jsxAttributeValue(validValue, emptyComments)
  const wrappedInvalidValues = [jsxAttributeValue('hat', emptyComments)]

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
        value: validValue,
        label: 'Label',
      },
    ],
  }

  const wrappedValidValue = jsxAttributeValue(validValue, emptyComments)

  runBaseTestSuite(validValue, wrappedValidValue, [], radioControlDescriptionValue)
})

describe('StringInputControlDescription', () => {
  const stringInputControlDescriptionValue: StringInputControlDescription = {
    control: 'string-input',
  }

  const validValue = 'hat'
  const wrappedValidValue = jsxAttributeValue(validValue, emptyComments)
  const wrappedInvalidValues = [jsxAttributeValue(0, emptyComments)]

  runBaseTestSuite(
    validValue,
    wrappedValidValue,
    wrappedInvalidValues,
    stringInputControlDescriptionValue,
  )
})

describe('ArrayControlDescription', () => {
  const simpleArrayControlDescriptionValue: ArrayControlDescription = {
    control: 'array',
    propertyControl: {
      control: 'string-input',
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
    control: 'object',
    object: {
      [simpleValidKey]: {
        control: 'string-input',
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
  const simpleWrappedValidValue = jsxAttributeValue(simpleValidValue, emptyComments)
  const simpleWrappedInvalidValues = [jsxAttributeValue(false, emptyComments)]

  runBaseTestSuite(
    simpleValidValue,
    simpleWrappedValidValue,
    simpleWrappedInvalidValues,
    simpleUnionControlDescriptionValue,
  )

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
