import type {
  NumberInputControlDescription,
  CheckboxControlDescription,
  StringInputControlDescription,
  PopUpListControlDescription,
  RadioControlDescription,
  ColorControlDescription,
  NoneControlDescription,
  StyleControlsControlDescription,
  ExpressionInputControlDescription,
  Vector2ControlDescription,
  Vector3ControlDescription,
  Vector4ControlDescription,
  EulerControlDescription,
  Matrix3ControlDescription,
  Matrix4ControlDescription,
  ExpressionPopUpListControlDescription,
  ArrayControlDescription,
  ObjectControlDescription,
  TupleControlDescription,
} from 'utopia-api/core'
import type { ParsedPropertyControls } from './property-controls-parser'
import {
  parseNumberInputControlDescription,
  parseCheckboxControlDescription,
  parseStringInputControlDescription,
  parsePopUpListControlDescription,
  parseRadioControlDescription,
  parseColorControlDescription,
  parsePropertyControls,
  parseNoneControlDescription,
  parseStyleControlsControlDescription,
  parseControlDescription,
  parseExpressionInputControlDescription,
  parseVector2ControlDescription,
  parseVector3ControlDescription,
  parseVector4ControlDescription,
  parseEulerControlDescription,
  parseMatrix3ControlDescription,
  parseMatrix4ControlDescription,
  parseExpressionPopUpListControlDescription,
  parseArrayControlDescription,
  parseObjectControlDescription,
  parseTupleControlDescription,
} from './property-controls-parser'
import { right, left, isLeft } from '../shared/either'
import type { ParseResult } from '../../utils/value-parser-utils'
import {
  objectFieldParseError,
  descriptionParseError,
  arrayIndexParseError,
  ParseError,
} from '../../utils/value-parser-utils'
import { pick } from '../shared/object-utils'
import { fastForEach } from '../shared/utils'
import type { MapLike } from 'typescript'

function runBaseTestSuite<T extends MapLike<any>>(
  validObject: T,
  requiredFields: Array<keyof T>,
  parseFn: (value: unknown) => ParseResult<T>,
  parseErrorDescription: string,
  supportsRequired: 'supports-required' | 'does-not-support-required',
  defaultValues: Array<unknown>,
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
      left(objectFieldParseError('control', descriptionParseError(parseErrorDescription))),
    )
  })
  if (supportsRequired === 'supports-required') {
    it('fails on an invalid value for the required flag', () => {
      const value = {
        ...validObject,
        required: 'honk',
      }
      expect(parseFn(value)).toEqual(
        left(objectFieldParseError('required', descriptionParseError('Not a boolean.'))),
      )
    })
    it('succeeds on a true for the required flag', () => {
      const value = {
        ...validObject,
        required: true,
      }
      expect(parseFn(value)).toEqual(right(value))
    })
    it('succeeds on a false for the required flag', () => {
      const value = {
        ...validObject,
        required: false,
      }
      expect(parseFn(value)).toEqual(right(value))
    })
  }
  for (const defaultValue of defaultValues) {
    it(`parses with a defaultValue of ${JSON.stringify(defaultValue)}`, () => {
      const value = {
        ...validObject,
        defaultValue: defaultValue,
      }
      expect(parseFn(value)).toEqual(right(value))
    })
  }
}

const validCheckboxControlDescriptionValue: CheckboxControlDescription = {
  label: 'Checkbox Control',
  control: 'checkbox',
  disabledTitle: 'Not set.',
  enabledTitle: 'Value is set',
  visibleByDefault: true,
}

describe('parseCheckboxControlDescription', () => {
  runBaseTestSuite(
    validCheckboxControlDescriptionValue,
    ['control'],
    parseCheckboxControlDescription,
    'Value was not checkbox.',
    'supports-required',
    [true, false],
  )
})

const validColorControlDescriptionValue: ColorControlDescription = {
  label: 'Slider Control',
  control: 'color',
  visibleByDefault: true,
}

describe('parseColorControlDescription', () => {
  runBaseTestSuite(
    validColorControlDescriptionValue,
    ['control'],
    parseColorControlDescription,
    'Value was not color.',
    'supports-required',
    ['blue', '#aabbcc'],
  )
})

const validExpressionInputControlDescriptionValue: ExpressionInputControlDescription = {
  label: 'Expression Input Control',
  control: 'expression-input',
  visibleByDefault: true,
}

describe('parseExpressionInputControlDescription', () => {
  runBaseTestSuite(
    validExpressionInputControlDescriptionValue,
    ['control'],
    parseExpressionInputControlDescription,
    'Value was not expression-input.',
    'supports-required',
    ['something'],
  )
})

const validPopUpListControlDescriptionValue: PopUpListControlDescription = {
  label: 'PopUpList Control',
  control: 'popuplist',
  options: [
    {
      value: 'hat',
      label: 'Hat',
    },
    {
      value: 5,
      label: 'Five',
    },
    {
      value: true,
      label: 'True',
    },
    {
      value: undefined,
      label: 'Undefined',
    },
    {
      value: null,
      label: 'Null',
    },
  ],
  visibleByDefault: true,
}

describe('parsePopUpListControlDescription', () => {
  runBaseTestSuite(
    validPopUpListControlDescriptionValue,
    ['control', 'options'],
    parsePopUpListControlDescription,
    'Value was not popuplist.',
    'supports-required',
    [
      'something',
      true,
      false,
      100,
      undefined,
      null,
      {
        value: 1,
        label: 'Option',
      },
    ],
  )
})

const validExpressionPopUpListControlDescriptionValue: ExpressionPopUpListControlDescription = {
  label: 'Expression Popuplist Control',
  control: 'expression-popuplist',
  options: [
    {
      value: 0,
      expression: 'THREE.Multiply',
      label: 'Multiply',
      requiredImport: {
        source: 'three',
        name: 'THREE',
        type: 'star',
      },
    },
    {
      value: 1,
      expression: 'THREE.MixOperation',
      label: 'MixOperation',
      requiredImport: {
        source: 'three',
        name: 'THREE',
        type: 'star',
      },
    },
    {
      value: 2,
      expression: 'THREE.AddOperation',
      label: 'Multiply',
      requiredImport: {
        source: 'three',
        name: 'THREE',
        type: 'star',
      },
    },
  ],
  visibleByDefault: true,
}

describe('parseExpressionPopUpListControlDescription', () => {
  runBaseTestSuite(
    validExpressionPopUpListControlDescriptionValue,
    ['control', 'options'],
    parseExpressionPopUpListControlDescription,
    'Value was not expression-popuplist.',
    'supports-required',
    [
      'something',
      true,
      false,
      100,
      undefined,
      null,
      {
        value: 1,
        label: 'Option',
      },
    ],
  )
})

const validNoneControlDescriptionValue: NoneControlDescription = {
  label: 'None Description',
  control: 'none',
  visibleByDefault: true,
}

describe('parseNoneControlDescription', () => {
  runBaseTestSuite(
    validNoneControlDescriptionValue,
    ['control'],
    parseNoneControlDescription,
    'Value was not none.',
    'supports-required',
    ['something', true, false, 100, undefined, null],
  )
})

const validNumberInputControlDescriptionValue: NumberInputControlDescription = {
  label: 'NumberInput Control',
  control: 'number-input',
  max: 10,
  min: 2,
  unit: 'Some Unit',
  step: 1,
  displayStepper: true,
  visibleByDefault: true,
}

describe('parseNumberInputControlDescription', () => {
  runBaseTestSuite(
    validNumberInputControlDescriptionValue,
    ['control'],
    parseNumberInputControlDescription,
    'Value was not number-input.',
    'supports-required',
    [-100, 0, 100],
  )
})

const validRadioControlDescriptionValue: RadioControlDescription = {
  label: 'Radio Control',
  control: 'radio',
  options: [
    { value: 5, label: 'Five' },
    { value: 8, label: 'Eight' },
  ],
  visibleByDefault: true,
}

describe('parseRadioControlDescription', () => {
  runBaseTestSuite(
    validRadioControlDescriptionValue,
    ['control', 'options'],
    parseRadioControlDescription,
    'Value was not radio.',
    'supports-required',
    [
      'something',
      true,
      false,
      100,
      undefined,
      null,
      {
        value: 1,
        label: 'Option',
      },
    ],
  )
})

const validStringInputControlDescriptionValue: StringInputControlDescription = {
  label: 'String Input Control',
  control: 'string-input',
  placeholder: 'Enter text',
  obscured: true,
  visibleByDefault: true,
}

describe('parseStringInputControlDescription', () => {
  runBaseTestSuite(
    validStringInputControlDescriptionValue,
    ['control'],
    parseStringInputControlDescription,
    'Value was not string-input.',
    'supports-required',
    ['something'],
  )
})

const validStyleControlsControlDescriptionValue: StyleControlsControlDescription = {
  label: 'Style Controls Control',
  control: 'style-controls',
  placeholder: { height: 100 },
  visibleByDefault: true,
}

describe('parseStyleControlsControlDescription', () => {
  runBaseTestSuite(
    validStyleControlsControlDescriptionValue,
    ['control'],
    parseStyleControlsControlDescription,
    'Value was not style-controls.',
    'supports-required',
    [{ backgroundColor: 'red' }],
  )
})

const validVector2ControlDescriptionValue: Vector2ControlDescription = {
  label: 'Vector2 Control',
  control: 'vector2',
  visibleByDefault: true,
}

describe('parseVector2ControlDescription', () => {
  runBaseTestSuite(
    validVector2ControlDescriptionValue,
    ['control'],
    parseVector2ControlDescription,
    'Value was not vector2.',
    'supports-required',
    [[100, 200]],
  )
})

const validVector3ControlDescriptionValue: Vector3ControlDescription = {
  label: 'Vector3 Control',
  control: 'vector3',
  visibleByDefault: true,
}

describe('parseVector3ControlDescription', () => {
  runBaseTestSuite(
    validVector3ControlDescriptionValue,
    ['control'],
    parseVector3ControlDescription,
    'Value was not vector3.',
    'supports-required',
    [[100, 200, 300]],
  )
})

const validVector4ControlDescriptionValue: Vector4ControlDescription = {
  label: 'Vector4 Control',
  control: 'vector4',
  visibleByDefault: true,
}

describe('parseVector4ControlDescription', () => {
  runBaseTestSuite(
    validVector4ControlDescriptionValue,
    ['control'],
    parseVector4ControlDescription,
    'Value was not vector4.',
    'supports-required',
    [[100, 200, 300, 400]],
  )
})

const validEulerControlDescriptionValue: EulerControlDescription = {
  label: 'Euler Control',
  control: 'euler',
  visibleByDefault: true,
}

describe('parseEulerControlDescription', () => {
  runBaseTestSuite(
    validEulerControlDescriptionValue,
    ['control'],
    parseEulerControlDescription,
    'Value was not euler.',
    'supports-required',
    [[100, 200, 300, 'XYZ']],
  )
})

const validMatrix3ControlDescriptionValue: Matrix3ControlDescription = {
  label: 'Matrix3 Control',
  control: 'matrix3',
  visibleByDefault: true,
}

describe('parseMatrix3ControlDescription', () => {
  runBaseTestSuite(
    validMatrix3ControlDescriptionValue,
    ['control'],
    parseMatrix3ControlDescription,
    'Value was not matrix3.',
    'supports-required',
    [[1, 2, 3, 4, 5, 6, 7, 8, 9]],
  )
})

const validMatrix4ControlDescriptionValue: Matrix4ControlDescription = {
  label: 'Matrix4 Control',
  control: 'matrix4',
  visibleByDefault: true,
}

describe('parseMatrix4ControlDescription', () => {
  runBaseTestSuite(
    validMatrix4ControlDescriptionValue,
    ['control'],
    parseMatrix4ControlDescription,
    'Value was not matrix4.',
    'supports-required',
    [[1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16]],
  )
})

const validArrayControlDescriptionValue: ArrayControlDescription = {
  label: 'Array Control',
  control: 'array',
  propertyControl: {
    control: 'string-input',
  },
}

describe('parseArrayControlDescription', () => {
  runBaseTestSuite(
    validArrayControlDescriptionValue,
    ['control', 'propertyControl'],
    parseArrayControlDescription,
    'Value was not array.',
    'supports-required',
    [
      ['a', 'b', 'c'],
      [1, 2, 3],
    ],
  )
})

const validObjectControlDescriptionValue: ObjectControlDescription = {
  label: 'Object Control',
  control: 'object',
  object: {
    cat: {
      control: 'string-input',
    },
  },
}

describe('parseObjectControlDescription', () => {
  runBaseTestSuite(
    validObjectControlDescriptionValue,
    ['control', 'object'],
    parseObjectControlDescription,
    'Value was not object.',
    'supports-required',
    [{ a: 1, b: 2 }],
  )
})

const validTupleControlDescriptionValue: TupleControlDescription = {
  control: 'tuple',
  propertyControls: [
    { control: 'string-input' },
    { control: 'number-input' },
    { control: 'string-input' },
  ],
}

describe('parseTupleControlDescription', () => {
  runBaseTestSuite(
    validTupleControlDescriptionValue,
    ['control', 'propertyControls'],
    parseTupleControlDescription,
    'Value was not tuple.',
    'supports-required',
    [
      ['a', 'b', 'c'],
      [1, 2, 3],
    ],
  )
})

describe('parseControlDescription', () => {
  it('parses a number input description correctly', () => {
    expect(parseControlDescription(validNumberInputControlDescriptionValue)).toEqual(
      right(validNumberInputControlDescriptionValue),
    )
  })
  it('parses a checkbox description correctly', () => {
    expect(parseControlDescription(validCheckboxControlDescriptionValue)).toEqual(
      right(validCheckboxControlDescriptionValue),
    )
  })
  it('parses a string input description correctly', () => {
    expect(parseControlDescription(validStringInputControlDescriptionValue)).toEqual(
      right(validStringInputControlDescriptionValue),
    )
  })
  it('parses a popup list description correctly', () => {
    expect(parseControlDescription(validPopUpListControlDescriptionValue)).toEqual(
      right(validPopUpListControlDescriptionValue),
    )
  })
  it('parses an expression popup list description correctly', () => {
    expect(parseControlDescription(validExpressionPopUpListControlDescriptionValue)).toEqual(
      right(validExpressionPopUpListControlDescriptionValue),
    )
  })
  it('parses a radio description correctly', () => {
    expect(parseControlDescription(validRadioControlDescriptionValue)).toEqual(
      right(validRadioControlDescriptionValue),
    )
  })
  it('parses a color description correctly', () => {
    expect(parseControlDescription(validColorControlDescriptionValue)).toEqual(
      right(validColorControlDescriptionValue),
    )
  })
  it('parses an expression input control description correctly', () => {
    expect(parseControlDescription(validExpressionInputControlDescriptionValue)).toEqual(
      right(validExpressionInputControlDescriptionValue),
    )
  })
  it('parses a none description correctly', () => {
    expect(parseControlDescription(validNoneControlDescriptionValue)).toEqual(
      right(validNoneControlDescriptionValue),
    )
  })
  it('parses an array description correctly', () => {
    expect(parseControlDescription(validArrayControlDescriptionValue)).toEqual(
      right(validArrayControlDescriptionValue),
    )
  })
  it('parses an object description correctly', () => {
    expect(parseControlDescription(validObjectControlDescriptionValue)).toEqual(
      right(validObjectControlDescriptionValue),
    )
  })
  it('parses a tuple description correctly', () => {
    expect(parseControlDescription(validTupleControlDescriptionValue)).toEqual(
      right(validTupleControlDescriptionValue),
    )
  })
  it('parses a vector2 description correctly', () => {
    expect(parseControlDescription(validVector2ControlDescriptionValue)).toEqual(
      right(validVector2ControlDescriptionValue),
    )
  })
  it('parses a vector3 description correctly', () => {
    expect(parseControlDescription(validVector3ControlDescriptionValue)).toEqual(
      right(validVector3ControlDescriptionValue),
    )
  })
  it('parses a vector4 description correctly', () => {
    expect(parseControlDescription(validVector4ControlDescriptionValue)).toEqual(
      right(validVector4ControlDescriptionValue),
    )
  })
  it('parses a euler description correctly', () => {
    expect(parseControlDescription(validEulerControlDescriptionValue)).toEqual(
      right(validEulerControlDescriptionValue),
    )
  })
  it('parses a matrix3 description correctly', () => {
    expect(parseControlDescription(validMatrix3ControlDescriptionValue)).toEqual(
      right(validMatrix3ControlDescriptionValue),
    )
  })
  it('parses a matrix4 description correctly', () => {
    expect(parseControlDescription(validMatrix4ControlDescriptionValue)).toEqual(
      right(validMatrix4ControlDescriptionValue),
    )
  })
  it('fails on a value that is not an object', () => {
    expect(parseControlDescription('hat')).toEqual(left(descriptionParseError('Not an object.')))
  })
  it('fails on a value that is an invalid case of one of the descriptions', () => {
    const value = {
      ...validRadioControlDescriptionValue,
      label: true,
    }
    expect(parseControlDescription(value)).toEqual(
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
    expect(parsePropertyControls(propertyControlsValue)).toEqual(expectedResult)
  })
  it('returns the property controls fully parsed when some are invalid', () => {
    const propertyControlsValue = {
      width: validNumberInputControlDescriptionValue,
      height: {
        ...validNumberInputControlDescriptionValue,
        max: 'hat',
      },
    }
    const expectedResult: ParseResult<ParsedPropertyControls> = right({
      width: right(validNumberInputControlDescriptionValue),
      height: left(objectFieldParseError('max', descriptionParseError('Not a number.'))),
    })
    expect(parsePropertyControls(propertyControlsValue)).toEqual(expectedResult)
  })
  it('gives an error if the entire value is invalid', () => {
    const propertyControlsValue = 5
    const expectedResult: ParseResult<ParsedPropertyControls> = left(
      descriptionParseError('Not an object.'),
    )
    expect(parsePropertyControls(propertyControlsValue)).toEqual(expectedResult)
  })
})
