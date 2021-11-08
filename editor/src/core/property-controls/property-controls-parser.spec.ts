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
  visibleByDefault: true,
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
  visibleByDefault: true,
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
  control: 'expression-input',
  visibleByDefault: true,
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
    [],
    true,
    parsePopUpListControlDescription,
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
  defaultValue: {
    value: 0,
    expression: 'THREE.Multiply',
    label: 'Multiply',
    requiredImport: {
      source: 'three',
      name: 'THREE',
      type: 'star',
    },
  },
  visibleByDefault: true,
}

describe('parseExpressionPopUpListControlDescription', () => {
  runBaseTestSuite(
    validExpressionPopUpListControlDescriptionValue,
    ['control', 'options'],
    [],
    true,
    parseExpressionPopUpListControlDescription,
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
    [],
    false,
    parseNoneControlDescription,
  )
})

const validNumberInputControlDescriptionValue: NumberInputControlDescription = {
  label: 'NumberInput Control',
  control: 'number-input',
  defaultValue: 5,
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
  visibleByDefault: true,
}

describe('parseRadioControlDescription', () => {
  runBaseTestSuite(
    validRadioControlDescriptionValue,
    ['control', 'options'],
    [],
    true,
    parseRadioControlDescription,
  )
})

const validStringInputControlDescriptionValue: StringInputControlDescription = {
  label: 'String Input Control',
  control: 'string-input',
  defaultValue: 'Some text',
  placeholder: 'Enter text',
  obscured: true,
  visibleByDefault: true,
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
  control: 'style-controls',
  defaultValue: { width: 100 },
  placeholder: { height: 100 },
  visibleByDefault: true,
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

const validVector2ControlDescriptionValue: Vector2ControlDescription = {
  label: 'Vector2 Control',
  control: 'vector2',
  defaultValue: [10, 20],
  visibleByDefault: true,
}

describe('parseVector2ControlDescription', () => {
  runBaseTestSuite(
    validVector2ControlDescriptionValue,
    ['control'],
    ['hat', 9],
    true,
    parseVector2ControlDescription,
  )
})

const validVector3ControlDescriptionValue: Vector3ControlDescription = {
  label: 'Vector3 Control',
  control: 'vector3',
  defaultValue: [10, 20, 30],
  visibleByDefault: true,
}

describe('parseVector3ControlDescription', () => {
  runBaseTestSuite(
    validVector3ControlDescriptionValue,
    ['control'],
    ['hat', 9, true],
    true,
    parseVector3ControlDescription,
  )
})

const validVector4ControlDescriptionValue: Vector4ControlDescription = {
  label: 'Vector4 Control',
  control: 'vector4',
  defaultValue: [10, 20, 30, 40],
  visibleByDefault: true,
}

describe('parseVector4ControlDescription', () => {
  runBaseTestSuite(
    validVector4ControlDescriptionValue,
    ['control'],
    ['hat', 9, true, 'bananas'],
    true,
    parseVector4ControlDescription,
  )
})

const validEulerControlDescriptionValue: EulerControlDescription = {
  label: 'Euler Control',
  control: 'euler',
  defaultValue: [10, 20, 30, 'XYZ'],
  visibleByDefault: true,
}

describe('parseEulerControlDescription', () => {
  runBaseTestSuite(
    validEulerControlDescriptionValue,
    ['control'],
    ['hat', 9, true, 'bananas'],
    true,
    parseEulerControlDescription,
  )
})

const validMatrix3ControlDescriptionValue: Matrix3ControlDescription = {
  label: 'Matrix3 Control',
  control: 'matrix3',
  // prettier-ignore
  defaultValue: [
    10, 20, 30,
    40, 50, 60,
    70, 80, 90,
  ],
  visibleByDefault: true,
}

describe('parseMatrix3ControlDescription', () => {
  runBaseTestSuite(
    validMatrix3ControlDescriptionValue,
    ['control'],
    ['hat', 9, true, 'bananas'],
    true,
    parseMatrix3ControlDescription,
  )
})

const validMatrix4ControlDescriptionValue: Matrix4ControlDescription = {
  label: 'Matrix4 Control',
  control: 'matrix4',
  // prettier-ignore
  defaultValue: [
    10, 20, 30, 40,
    50, 60, 70, 80,
    11, 21, 31, 41,
    51, 61, 71, 81,
  ],
  visibleByDefault: true,
}

describe('parseMatrix4ControlDescription', () => {
  runBaseTestSuite(
    validMatrix4ControlDescriptionValue,
    ['control'],
    ['hat', 9, true, 'bananas'],
    true,
    parseMatrix4ControlDescription,
  )
})

const validArrayControlDescriptionValue: ArrayControlDescription = {
  label: 'Array Control',
  control: 'array',
  defaultValue: ['cat', 'dog'],
  propertyControl: {
    control: 'string-input',
  },
}

describe('parseArrayControlDescription', () => {
  runBaseTestSuite(
    validArrayControlDescriptionValue,
    ['control', 'propertyControl'],
    ['hat', 9, true, 'bananas'],
    true,
    parseArrayControlDescription,
  )
})

const validObjectControlDescriptionValue: ObjectControlDescription = {
  label: 'Object Control',
  control: 'object',
  defaultValue: {
    cat: 'dog',
  },
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
    ['hat', 9, true, 'bananas'],
    true,
    parseObjectControlDescription,
  )
})

const validTupleControlDescriptionValue: TupleControlDescription = {
  control: 'tuple',
  defaultValue: ['cat', 1, 'dog'],
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
    ['hat', 9, true, 'bananas'],
    true,
    parseTupleControlDescription,
  )
})

const validFolderControlDescriptionValue: FolderControlDescription = {
  control: 'folder',
  controls: {
    style: validStyleControlsControlDescriptionValue,
    someSlider: validNumberInputControlDescriptionValue,
  },
}

describe('parseFolderControlDescription', () => {
  runBaseTestSuite(
    validFolderControlDescriptionValue,
    ['control', 'controls'],
    [],
    false,
    (value: unknown) => parseFolderControlDescription(value, 'includeSpecialProps'),
  )
})

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
  it('parses an expression popup list description correctly', () => {
    expect(
      parseControlDescription(
        validExpressionPopUpListControlDescriptionValue,
        'testPropName',
        'includeSpecialProps',
      ),
    ).toEqual(right(validExpressionPopUpListControlDescriptionValue))
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
  it('parses a vector2 description correctly', () => {
    expect(
      parseControlDescription(
        validVector2ControlDescriptionValue,
        'testPropName',
        'includeSpecialProps',
      ),
    ).toEqual(right(validVector2ControlDescriptionValue))
  })
  it('parses a vector3 description correctly', () => {
    expect(
      parseControlDescription(
        validVector3ControlDescriptionValue,
        'testPropName',
        'includeSpecialProps',
      ),
    ).toEqual(right(validVector3ControlDescriptionValue))
  })
  it('parses a vector4 description correctly', () => {
    expect(
      parseControlDescription(
        validVector4ControlDescriptionValue,
        'testPropName',
        'includeSpecialProps',
      ),
    ).toEqual(right(validVector4ControlDescriptionValue))
  })
  it('parses a euler description correctly', () => {
    expect(
      parseControlDescription(
        validEulerControlDescriptionValue,
        'testPropName',
        'includeSpecialProps',
      ),
    ).toEqual(right(validEulerControlDescriptionValue))
  })
  it('parses a matrix3 description correctly', () => {
    expect(
      parseControlDescription(
        validMatrix3ControlDescriptionValue,
        'testPropName',
        'includeSpecialProps',
      ),
    ).toEqual(right(validMatrix3ControlDescriptionValue))
  })
  it('parses a matrix4 description correctly', () => {
    expect(
      parseControlDescription(
        validMatrix4ControlDescriptionValue,
        'testPropName',
        'includeSpecialProps',
      ),
    ).toEqual(right(validMatrix4ControlDescriptionValue))
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
