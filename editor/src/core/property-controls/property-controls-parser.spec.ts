import {
  NumberInputControlDescription,
  CheckboxControlDescription,
  StringInputControlDescription,
  PopUpListControlDescription,
  RadioControlDescription,
  ColorControlDescription,
  NoneControlDescription,
  StyleControlsControlDescription,
  FolderControlDescription,
  ExpressionInputControlDescription,
} from 'utopia-api'
import {
  parseNumberInputControlDescription,
  parseCheckboxControlDescription,
  parseStringInputControlDescription,
  parsePopUpListControlDescription,
  parseRadioControlDescription,
  parseColorControlDescription,
  ParsedPropertyControls,
  parsePropertyControls,
  parseNoneControlDescription,
  parseStyleControlsControlDescription,
  parseFolderControlDescription,
  parseControlDescription,
  parseExpressionInputControlDescription,
} from './property-controls-parser'
import { right, left, isLeft } from '../shared/either'
import {
  objectFieldParseError,
  descriptionParseError,
  arrayIndexParseError,
  ParseResult,
  ParseError,
} from '../../utils/value-parser-utils'
import { pick } from '../shared/object-utils'
import { fastForEach } from '../shared/utils'

function runBaseTestSuite<T>(
  validObject: T,
  requiredFields: Array<keyof T>,
  invalidDefaults: unknown[],
  defaultAllowed: boolean,
  parseFn: (value: unknown) => ParseResult<T>,
) {
  it('parses a full value correctly', () => {
    expect(parseFn(validObject)).toEqual(right(validObject))
  })
  it('parses a minimal value correctly', () => {
    const value = pick(requiredFields, validObject)
    expect(parseFn(value)).toEqual(right(value))
  })
  it('fails on an invalid label', () => {
    const value = {
      ...validObject,
      label: true,
    }
    expect(parseFn(value)).toEqual(
      left(objectFieldParseError('label', descriptionParseError('Not a string.'))),
    )
  })
  it('fails on an invalid type', () => {
    const value = {
      ...validObject,
      control: 'ham sandwich',
    }
    expect(parseFn(value)).toEqual(
      left(objectFieldParseError('control', descriptionParseError('Not a member of an enum.'))),
    )
  })

  if (defaultAllowed) {
    it('fails on an invalid default', () => {
      fastForEach(invalidDefaults, (invalidDefault) => {
        const value = {
          ...validObject,
          defaultValue: invalidDefault,
        }
        expect(isLeft(parseFn(value))).toBeTruthy()
      })
    })
  } else {
    it('ignores a default value', () => {
      const value = {
        ...validObject,
        defaultValue: 'anything really',
      }

      expect(parseFn(value)).toEqual(right(validObject))
    })
  }
}

const validCheckboxControlDescriptionValue: CheckboxControlDescription = {
  label: 'Checkbox Control',
  control: 'checkbox',
  defaultValue: true,
  disabledTitle: 'Not set.',
  enabledTitle: 'Value is set',
}

describe('parseCheckboxControlDescription', () => {
  runBaseTestSuite(
    validCheckboxControlDescriptionValue,
    ['control'],
    ['hat'],
    true,
    parseCheckboxControlDescription,
  )
})

const validColorControlDescriptionValue: ColorControlDescription = {
  label: 'Slider Control',
  control: 'color',
  defaultValue: '#FFFFFF',
}

describe('parseColorControlDescription', () => {
  runBaseTestSuite(
    validColorControlDescriptionValue,
    ['control'],
    ['hat', 9],
    true,
    parseColorControlDescription,
  )
})

const validExpressionInputControlDescriptionValue: ExpressionInputControlDescription = {
  label: 'Expression Input Control',
  control: 'expressioninput',
}

describe('parseExpressionInputControlDescription', () => {
  runBaseTestSuite(
    validExpressionInputControlDescriptionValue,
    ['control'],
    [],
    false,
    parseExpressionInputControlDescription,
  )
})

const validPopUpListControlDescriptionValue: PopUpListControlDescription = {
  label: 'PopUpList Control',
  control: 'popuplist',
  defaultValue: 5,
  options: ['hat', 5, true, undefined, null],
  optionTitles: ['first title', 'second title'],
  displaySegmentedControl: true,
}

describe('parsePopUpListControlDescription', () => {
  runBaseTestSuite(
    validPopUpListControlDescriptionValue,
    ['control', 'options'],
    [['hat']],
    true,
    parsePopUpListControlDescription,
  )
})

const validNoneControlDescriptionValue: NoneControlDescription = {
  label: 'None Description',
  control: 'none',
}

describe('parseNoneControlDescription', () => {
  runBaseTestSuite(
    validNoneControlDescriptionValue,
    ['control'],
    [],
    false,
    parseNoneControlDescription,
  )
})

const validNumberInputControlDescriptionValue: NumberInputControlDescription = {
  label: 'NumberInput Control',
  control: 'numberinput',
  defaultValue: 5,
  max: 10,
  min: 2,
  unit: 'Some Unit',
  step: 1,
  displayStepper: true,
}

describe('parseNumberInputControlDescription', () => {
  runBaseTestSuite(
    validNumberInputControlDescriptionValue,
    ['control'],
    ['hat'],
    true,
    parseNumberInputControlDescription,
  )
})

const validRadioControlDescriptionValue: RadioControlDescription = {
  label: 'Radio Control',
  control: 'radio',
  defaultValue: 5,
  options: [
    { value: 5, label: 'Five' },
    { value: 8, label: 'Eight' },
  ],
}

describe('parseRadioControlDescription', () => {
  runBaseTestSuite(
    validRadioControlDescriptionValue,
    ['control', 'options'],
    [],
    true,
    parseRadioControlDescription,
  )

  it('fails on an invalid option', () => {
    const value = {
      ...validRadioControlDescriptionValue,
      options: ['error'],
    }
    expect(parseRadioControlDescription(value)).toEqual(
      left(
        objectFieldParseError(
          'options',
          arrayIndexParseError(0, descriptionParseError('Not an object.')),
        ),
      ),
    )
  })
})

const validStringInputControlDescriptionValue: StringInputControlDescription = {
  label: 'String Input Control',
  control: 'stringinput',
  defaultValue: 'Some text',
  placeholder: 'Enter text',
  obscured: true,
}

describe('parseStringInputControlDescription', () => {
  runBaseTestSuite(
    validStringInputControlDescriptionValue,
    ['control'],
    [9],
    true,
    parseStringInputControlDescription,
  )
})

const validStyleControlsControlDescriptionValue: StyleControlsControlDescription = {
  label: 'Style Controls Control',
  control: 'stylecontrols',
  defaultValue: { width: 100 },
  placeholder: { height: 100 },
}

describe('parseStyleControlsControlDescription', () => {
  runBaseTestSuite(
    validStyleControlsControlDescriptionValue,
    ['control'],
    ['hat', 9],
    true,
    parseStyleControlsControlDescription,
  )
})

const validFolderControlDescriptionValue: FolderControlDescription = {
  control: 'folder',
  controls: {
    style: validStyleControlsControlDescriptionValue,
    someSlider: validNumberInputControlDescriptionValue,
  },
}

describe('parseControlDescription', () => {
  it('parses a number input description correctly', () => {
    expect(
      parseControlDescription(
        validNumberInputControlDescriptionValue,
        'testPropName',
        'includeSpecialProps',
      ),
    ).toEqual(right(validNumberInputControlDescriptionValue))
  })
  it('parses a checkbox description correctly', () => {
    expect(
      parseControlDescription(
        validCheckboxControlDescriptionValue,
        'testPropName',
        'includeSpecialProps',
      ),
    ).toEqual(right(validCheckboxControlDescriptionValue))
  })
  it('parses a string input description correctly', () => {
    expect(
      parseControlDescription(
        validStringInputControlDescriptionValue,
        'testPropName',
        'includeSpecialProps',
      ),
    ).toEqual(right(validStringInputControlDescriptionValue))
  })
  it('parses a popup list description correctly', () => {
    expect(
      parseControlDescription(
        validPopUpListControlDescriptionValue,
        'testPropName',
        'includeSpecialProps',
      ),
    ).toEqual(right(validPopUpListControlDescriptionValue))
  })
  it('parses a radio description correctly', () => {
    expect(
      parseControlDescription(
        validRadioControlDescriptionValue,
        'testPropName',
        'includeSpecialProps',
      ),
    ).toEqual(right(validRadioControlDescriptionValue))
  })
  it('parses a color description correctly', () => {
    expect(
      parseControlDescription(
        validColorControlDescriptionValue,
        'testPropName',
        'includeSpecialProps',
      ),
    ).toEqual(right(validColorControlDescriptionValue))
  })
  it('parses an expression input control description correctly', () => {
    expect(
      parseControlDescription(
        validExpressionInputControlDescriptionValue,
        'testPropName',
        'includeSpecialProps',
      ),
    ).toEqual(right(validExpressionInputControlDescriptionValue))
  })
  it('parses a none description correctly', () => {
    expect(
      parseControlDescription(
        validNoneControlDescriptionValue,
        'testPropName',
        'includeSpecialProps',
      ),
    ).toEqual(right(validNoneControlDescriptionValue))
  })
  it('parses a folder instance description correctly', () => {
    expect(
      parseControlDescription(
        validFolderControlDescriptionValue,
        'testPropName',
        'includeSpecialProps',
      ),
    ).toEqual(right(validFolderControlDescriptionValue))
  })
  it('fails on a value that is not an object', () => {
    expect(parseControlDescription('hat', 'testPropName', 'includeSpecialProps')).toEqual(
      left(descriptionParseError('Not an object.')),
    )
  })
  it('fails on a value that is an invalid case of one of the descriptions', () => {
    const value = {
      ...validRadioControlDescriptionValue,
      label: true,
    }
    expect(parseControlDescription(value, 'testPropName', 'includeSpecialProps')).toEqual(
      left(objectFieldParseError('label', descriptionParseError('Not a string.'))),
    )
  })
})

describe('parsePropertyControls', () => {
  it('returns the property controls fully parsed when they are all valid', () => {
    const propertyControlsValue = {
      width: validNumberInputControlDescriptionValue,
      height: validNumberInputControlDescriptionValue,
    }
    const expectedResult: ParseResult<ParsedPropertyControls> = right({
      width: right(validNumberInputControlDescriptionValue),
      height: right(validNumberInputControlDescriptionValue),
    })
    expect(parsePropertyControls(propertyControlsValue, 'includeSpecialProps')).toEqual(
      expectedResult,
    )
  })
  it('returns the property controls fully parsed when some are invalid', () => {
    const propertyControlsValue = {
      width: validNumberInputControlDescriptionValue,
      height: {
        ...validNumberInputControlDescriptionValue,
        defaultValue: 'hat',
      },
    }
    const expectedResult: ParseResult<ParsedPropertyControls> = right({
      width: right(validNumberInputControlDescriptionValue),
      height: left(objectFieldParseError('defaultValue', descriptionParseError('Not a number.'))),
    })
    expect(parsePropertyControls(propertyControlsValue, 'includeSpecialProps')).toEqual(
      expectedResult,
    )
  })
  it('gives an error if the entire value is invalid', () => {
    const propertyControlsValue = 5
    const expectedResult: ParseResult<ParsedPropertyControls> = left(
      descriptionParseError('Not an object.'),
    )
    expect(parsePropertyControls(propertyControlsValue, 'includeSpecialProps')).toEqual(
      expectedResult,
    )
  })
})
