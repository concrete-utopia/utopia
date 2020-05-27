import {
  ControlType,
  NumberControlDescription,
  EnumControlDescription,
  BooleanControlDescription,
  StringControlDescription,
  SliderControlDescription,
  PopUpListControlDescription,
  OptionsControlDescription,
  ColorControlDescription,
  ComponentInstanceDescription,
  IgnoreControlDescription,
} from 'utopia-api'
import {
  parseNumberControlDescription,
  parseEnumControlDescription,
  parseBooleanControlDescription,
  parseStringControlDescription,
  parseSliderControlDescription,
  parsePopUpListControlDescription,
  parseOptionsControlDescription,
  parseColorControlDescription,
  parseComponentInstanceControlDescription,
  parseControlDescription,
  ParsedPropertyControls,
  parsePropertyControls,
  parseIgnoreControlDescription,
} from './property-controls-parser'
import { right, left } from '../shared/either'
import {
  objectFieldParseError,
  descriptionParseError,
  arrayIndexParseError,
  ParseResult,
  ParseError,
} from '../../utils/value-parser-utils'

const validNumberControlDescriptionValue: NumberControlDescription<any> = {
  title: 'Number Title',
  type: ControlType.Number,
  defaultValue: 5,
  max: 10,
  min: 2,
  unit: 'Some Unit',
  step: 1,
  displayStepper: true,
}

describe('parseNumberControlDescription', () => {
  it('parses a full value correctly', () => {
    expect(parseNumberControlDescription(validNumberControlDescriptionValue)).toEqual(
      right(validNumberControlDescriptionValue),
    )
  })
  it('parses a minimal value correctly', () => {
    const value = {
      type: ControlType.Number,
    }
    expect(parseNumberControlDescription(value)).toEqual(right(value))
  })
  it('fails on an invalid title', () => {
    const value = {
      ...validNumberControlDescriptionValue,
      title: true,
    }
    expect(parseNumberControlDescription(value)).toEqual(
      left(objectFieldParseError('title', descriptionParseError('Value is not a string.'))),
    )
  })
  it('fails on an invalid default value', () => {
    const value = {
      ...validNumberControlDescriptionValue,
      defaultValue: 'hat',
    }
    expect(parseNumberControlDescription(value)).toEqual(
      left(objectFieldParseError('defaultValue', descriptionParseError('Value is not a number.'))),
    )
  })
  it('fails on an invalid type', () => {
    const value = {
      ...validNumberControlDescriptionValue,
      type: 'ham sandwich',
    }
    expect(parseNumberControlDescription(value)).toEqual(
      left(
        objectFieldParseError('type', descriptionParseError('Value is not a member of an enum.')),
      ),
    )
  })
})

const validEnumControlDescriptionValue: EnumControlDescription<any> = {
  title: 'Enum Control',
  type: ControlType.Enum,
  defaultValue: 5,
  options: ['hat', 5, true, undefined, null],
  optionTitles: ['first title', 'second title'],
  displaySegmentedControl: true,
}

describe('parseEnumControlDescription', () => {
  it('parses a full value correctly', () => {
    expect(parseEnumControlDescription(validEnumControlDescriptionValue)).toEqual(
      right(validEnumControlDescriptionValue),
    )
  })
  it('parses a minimal value correctly', () => {
    const value = {
      type: ControlType.Enum,
      options: ['hat', 5, true, undefined, null],
    }
    expect(parseEnumControlDescription(value)).toEqual(right(value))
  })
  it('fails on an invalid title', () => {
    const value = {
      ...validEnumControlDescriptionValue,
      title: true,
    }
    expect(parseEnumControlDescription(value)).toEqual(
      left(objectFieldParseError('title', descriptionParseError('Value is not a string.'))),
    )
  })
  it('fails on an invalid default value', () => {
    const value = {
      ...validEnumControlDescriptionValue,
      defaultValue: ['hat'],
    }
    expect(parseEnumControlDescription(value)).toEqual(
      left(
        objectFieldParseError(
          'defaultValue',
          descriptionParseError('Value is not a string/boolean/number/undefined/null.'),
        ),
      ),
    )
  })
  it('fails on an invalid type', () => {
    const value = {
      ...validEnumControlDescriptionValue,
      type: 'ham sandwich',
    }
    expect(parseEnumControlDescription(value)).toEqual(
      left(
        objectFieldParseError('type', descriptionParseError('Value is not a member of an enum.')),
      ),
    )
  })
})

const validBooleanControlDescriptionValue: BooleanControlDescription<any> = {
  title: 'Boolean Control',
  type: ControlType.Boolean,
  defaultValue: true,
  disabledTitle: 'Value is not set.',
  enabledTitle: 'Value is set',
}

describe('parseBooleanControlDescription', () => {
  it('parses a full value correctly', () => {
    expect(parseBooleanControlDescription(validBooleanControlDescriptionValue)).toEqual(
      right(validBooleanControlDescriptionValue),
    )
  })
  it('parses a minimal value correctly', () => {
    const value = {
      type: ControlType.Boolean,
    }
    expect(parseBooleanControlDescription(value)).toEqual(right(value))
  })
  it('fails on an invalid title', () => {
    const value = {
      ...validBooleanControlDescriptionValue,
      title: true,
    }
    expect(parseBooleanControlDescription(value)).toEqual(
      left(objectFieldParseError('title', descriptionParseError('Value is not a string.'))),
    )
  })
  it('fails on an invalid default value', () => {
    const value = {
      ...validBooleanControlDescriptionValue,
      defaultValue: 'hat',
    }
    expect(parseBooleanControlDescription(value)).toEqual(
      left(objectFieldParseError('defaultValue', descriptionParseError('Value is not a boolean.'))),
    )
  })
  it('fails on an invalid type', () => {
    const value = {
      ...validBooleanControlDescriptionValue,
      type: 'ham sandwich',
    }
    expect(parseBooleanControlDescription(value)).toEqual(
      left(
        objectFieldParseError('type', descriptionParseError('Value is not a member of an enum.')),
      ),
    )
  })
})

const validStringControlDescriptionValue: StringControlDescription<any> = {
  title: 'String Control',
  type: ControlType.String,
  defaultValue: 'Some text',
  placeholder: 'Enter text',
  obscured: true,
}

describe('parseStringControlDescription', () => {
  it('parses a full value correctly', () => {
    expect(parseStringControlDescription(validStringControlDescriptionValue)).toEqual(
      right(validStringControlDescriptionValue),
    )
  })
  it('parses a minimal value correctly', () => {
    const value = {
      type: ControlType.String,
    }
    expect(parseStringControlDescription(value)).toEqual(right(value))
  })
  it('fails on an invalid title', () => {
    const value = {
      ...validStringControlDescriptionValue,
      title: true,
    }
    expect(parseStringControlDescription(value)).toEqual(
      left(objectFieldParseError('title', descriptionParseError('Value is not a string.'))),
    )
  })
  it('fails on an invalid default value', () => {
    const value = {
      ...validStringControlDescriptionValue,
      defaultValue: 9,
    }
    expect(parseStringControlDescription(value)).toEqual(
      left(objectFieldParseError('defaultValue', descriptionParseError('Value is not a string.'))),
    )
  })
  it('fails on an invalid type', () => {
    const value = {
      ...validStringControlDescriptionValue,
      type: 'ham sandwich',
    }
    expect(parseStringControlDescription(value)).toEqual(
      left(
        objectFieldParseError('type', descriptionParseError('Value is not a member of an enum.')),
      ),
    )
  })
})

const validSliderControlDescriptionValue: SliderControlDescription<any> = {
  title: 'Slider Control',
  type: ControlType.Slider,
  defaultValue: 5,
  min: 2,
  max: 10,
  step: 1,
}

describe('parseSliderControlDescription', () => {
  it('parses a full value correctly', () => {
    expect(parseSliderControlDescription(validSliderControlDescriptionValue)).toEqual(
      right(validSliderControlDescriptionValue),
    )
  })
  it('parses a minimal value correctly', () => {
    const value = {
      type: ControlType.Slider,
      min: 2,
      max: 10,
      step: 1,
    }
    expect(parseSliderControlDescription(value)).toEqual(right(value))
  })
  it('fails on an invalid title', () => {
    const value = {
      ...validSliderControlDescriptionValue,
      title: true,
    }
    expect(parseSliderControlDescription(value)).toEqual(
      left(objectFieldParseError('title', descriptionParseError('Value is not a string.'))),
    )
  })
  it('fails on an invalid default value', () => {
    const value = {
      ...validSliderControlDescriptionValue,
      defaultValue: 'hat',
    }
    expect(parseSliderControlDescription(value)).toEqual(
      left(objectFieldParseError('defaultValue', descriptionParseError('Value is not a number.'))),
    )
  })
  it('fails on an invalid type', () => {
    const value = {
      ...validSliderControlDescriptionValue,
      type: 'ham sandwich',
    }
    expect(parseSliderControlDescription(value)).toEqual(
      left(
        objectFieldParseError('type', descriptionParseError('Value is not a member of an enum.')),
      ),
    )
  })
})

const validPopUpListControlDescriptionValue: PopUpListControlDescription<any> = {
  title: 'Pop Up List Control',
  type: ControlType.PopUpList,
  defaultValue: 5,
  options: [
    { value: 5, label: 'Five' },
    { value: 8, label: 'Eight' },
  ],
}

describe('parsePopUpListControlDescription', () => {
  it('parses a full value correctly', () => {
    expect(parsePopUpListControlDescription(validPopUpListControlDescriptionValue)).toEqual(
      right(validPopUpListControlDescriptionValue),
    )
  })
  it('parses a minimal value correctly', () => {
    const value = {
      type: ControlType.PopUpList,
      options: [],
    }
    expect(parsePopUpListControlDescription(value)).toEqual(right(value))
  })
  it('fails on an invalid title', () => {
    const value = {
      ...validPopUpListControlDescriptionValue,
      title: true,
    }
    expect(parsePopUpListControlDescription(value)).toEqual(
      left(objectFieldParseError('title', descriptionParseError('Value is not a string.'))),
    )
  })
  it('fails on an invalid option', () => {
    const value = {
      ...validPopUpListControlDescriptionValue,
      options: ['error'],
    }
    expect(parsePopUpListControlDescription(value)).toEqual(
      left(
        objectFieldParseError(
          'options',
          arrayIndexParseError(0, descriptionParseError('Value is not an object.')),
        ),
      ),
    )
  })
  it('fails on an invalid type', () => {
    const value = {
      ...validPopUpListControlDescriptionValue,
      type: 'ham sandwich',
    }
    expect(parsePopUpListControlDescription(value)).toEqual(
      left(
        objectFieldParseError('type', descriptionParseError('Value is not a member of an enum.')),
      ),
    )
  })
})

const validOptionsControlDescriptionValue: OptionsControlDescription<any> = {
  title: 'Pop Up List Control',
  type: ControlType.Options,
  defaultValue: 5,
  options: [
    { value: 5, label: 'Five' },
    { value: 8, label: 'Eight' },
  ],
}

describe('parseOptionsControlDescription', () => {
  it('parses a full value correctly', () => {
    expect(parseOptionsControlDescription(validOptionsControlDescriptionValue)).toEqual(
      right(validOptionsControlDescriptionValue),
    )
  })
  it('parses a minimal value correctly', () => {
    const value = {
      type: ControlType.Options,
      options: [],
    }
    expect(parseOptionsControlDescription(value)).toEqual(right(value))
  })
  it('fails on an invalid title', () => {
    const value = {
      ...validOptionsControlDescriptionValue,
      title: true,
    }
    expect(parseOptionsControlDescription(value)).toEqual(
      left(objectFieldParseError('title', descriptionParseError('Value is not a string.'))),
    )
  })
  it('fails on an invalid option', () => {
    const value = {
      ...validOptionsControlDescriptionValue,
      options: ['error'],
    }
    expect(parseOptionsControlDescription(value)).toEqual(
      left(
        objectFieldParseError(
          'options',
          arrayIndexParseError(0, descriptionParseError('Value is not an object.')),
        ),
      ),
    )
  })
  it('fails on an invalid type', () => {
    const value = {
      ...validOptionsControlDescriptionValue,
      type: 'ham sandwich',
    }
    expect(parseOptionsControlDescription(value)).toEqual(
      left(
        objectFieldParseError('type', descriptionParseError('Value is not a member of an enum.')),
      ),
    )
  })
})

const validColorControlDescriptionValue: ColorControlDescription<any> = {
  title: 'Slider Control',
  type: ControlType.Color,
  defaultValue: '#FFFFFF',
}

describe('parseColorControlDescription', () => {
  it('parses a full value correctly', () => {
    expect(parseColorControlDescription(validColorControlDescriptionValue)).toEqual(
      right(validColorControlDescriptionValue),
    )
  })
  it('parses a minimal value correctly', () => {
    const value = {
      type: ControlType.Color,
    }
    expect(parseColorControlDescription(value)).toEqual(right(value))
  })
  it('fails on an invalid title', () => {
    const value = {
      ...validColorControlDescriptionValue,
      title: true,
    }
    expect(parseColorControlDescription(value)).toEqual(
      left(objectFieldParseError('title', descriptionParseError('Value is not a string.'))),
    )
  })
  it('fails on an invalid default value which is a string', () => {
    const value = {
      ...validColorControlDescriptionValue,
      defaultValue: 'hat',
    }
    expect(parseColorControlDescription(value)).toEqual(
      left(
        objectFieldParseError(
          'defaultValue',
          descriptionParseError('Value is not a valid color string.'),
        ),
      ),
    )
  })
  it('fails on an invalid default value which is not a string', () => {
    const value = {
      ...validColorControlDescriptionValue,
      defaultValue: 9,
    }
    expect(parseColorControlDescription(value)).toEqual(
      left(
        objectFieldParseError(
          'defaultValue',
          descriptionParseError('Value is not a valid color string.'),
        ),
      ),
    )
  })
  it('fails on an invalid type', () => {
    const value = {
      ...validColorControlDescriptionValue,
      type: 'ham sandwich',
    }
    expect(parseColorControlDescription(value)).toEqual(
      left(
        objectFieldParseError('type', descriptionParseError('Value is not a member of an enum.')),
      ),
    )
  })
})

const validComponentInstanceControlDescriptionValue: ComponentInstanceDescription<any> = {
  title: 'Component Instance Control',
  type: ControlType.ComponentInstance,
}

describe('parseComponentInstanceControlDescription', () => {
  it('parses a full value correctly', () => {
    expect(
      parseComponentInstanceControlDescription(validComponentInstanceControlDescriptionValue),
    ).toEqual(right(validComponentInstanceControlDescriptionValue))
  })
  it('parses a minimal value correctly', () => {
    const value = {
      type: ControlType.ComponentInstance,
    }
    expect(parseComponentInstanceControlDescription(value)).toEqual(right(value))
  })
  it('fails on an invalid title', () => {
    const value = {
      ...validComponentInstanceControlDescriptionValue,
      title: true,
    }
    expect(parseComponentInstanceControlDescription(value)).toEqual(
      left(objectFieldParseError('title', descriptionParseError('Value is not a string.'))),
    )
  })
  it('fails on an invalid type', () => {
    const value = {
      ...validComponentInstanceControlDescriptionValue,
      type: 'ham sandwich',
    }
    expect(parseComponentInstanceControlDescription(value)).toEqual(
      left(
        objectFieldParseError('type', descriptionParseError('Value is not a member of an enum.')),
      ),
    )
  })
})

const validIgnoreControlDescriptionValue: IgnoreControlDescription<any> = {
  title: 'Ignore Description',
  type: ControlType.Ignore,
}

describe('parseIgnoreControlDescription', () => {
  it('parses a full value correctly', () => {
    expect(parseIgnoreControlDescription(validIgnoreControlDescriptionValue)).toEqual(
      right(validIgnoreControlDescriptionValue),
    )
  })
  it('parses a minimal value correctly', () => {
    const value = {
      type: ControlType.Ignore,
    }
    expect(parseIgnoreControlDescription(value)).toEqual(right(value))
  })
  it('fails on an invalid title', () => {
    const value = {
      ...validIgnoreControlDescriptionValue,
      title: true,
    }
    expect(parseIgnoreControlDescription(value)).toEqual(
      left(objectFieldParseError('title', descriptionParseError('Value is not a string.'))),
    )
  })
  it('fails on an invalid type', () => {
    const value = {
      ...validIgnoreControlDescriptionValue,
      type: 'ham sandwich',
    }
    expect(parseIgnoreControlDescription(value)).toEqual(
      left(
        objectFieldParseError('type', descriptionParseError('Value is not a member of an enum.')),
      ),
    )
  })
})

describe('parseControlDescription', () => {
  it('parses a number description correctly', () => {
    expect(parseControlDescription(validNumberControlDescriptionValue)).toEqual(
      right(validNumberControlDescriptionValue),
    )
  })
  it('parses an enum description correctly', () => {
    expect(parseControlDescription(validEnumControlDescriptionValue)).toEqual(
      right(validEnumControlDescriptionValue),
    )
  })
  it('parses a boolean description correctly', () => {
    expect(parseControlDescription(validBooleanControlDescriptionValue)).toEqual(
      right(validBooleanControlDescriptionValue),
    )
  })
  it('parses a string description correctly', () => {
    expect(parseControlDescription(validStringControlDescriptionValue)).toEqual(
      right(validStringControlDescriptionValue),
    )
  })
  it('parses a slider description correctly', () => {
    expect(parseControlDescription(validSliderControlDescriptionValue)).toEqual(
      right(validSliderControlDescriptionValue),
    )
  })
  it('parses a popup list description correctly', () => {
    expect(parseControlDescription(validPopUpListControlDescriptionValue)).toEqual(
      right(validPopUpListControlDescriptionValue),
    )
  })
  it('parses a options list description correctly', () => {
    expect(parseControlDescription(validOptionsControlDescriptionValue)).toEqual(
      right(validOptionsControlDescriptionValue),
    )
  })
  it('parses a color description correctly', () => {
    expect(parseControlDescription(validColorControlDescriptionValue)).toEqual(
      right(validColorControlDescriptionValue),
    )
  })
  it('parses a component instance description correctly', () => {
    expect(parseControlDescription(validComponentInstanceControlDescriptionValue)).toEqual(
      right(validComponentInstanceControlDescriptionValue),
    )
  })
  it('parses an ignore description correctly', () => {
    expect(parseControlDescription(validIgnoreControlDescriptionValue)).toEqual(
      right(validIgnoreControlDescriptionValue),
    )
  })
  it('fails on a value that is not an object', () => {
    expect(parseControlDescription('hat')).toEqual(
      left(descriptionParseError('Value is not an object.')),
    )
  })
  it('fails on a value that is an invalid case of one of the descriptions', () => {
    const value = {
      ...validOptionsControlDescriptionValue,
      title: true,
    }
    expect(parseControlDescription(value)).toEqual(
      left(objectFieldParseError('title', descriptionParseError('Value is not a string.'))),
    )
  })
})

describe('parsePropertyControls', () => {
  it('returns the property controls fully parsed when they are all valid', () => {
    const propertyControlsValue = {
      width: validNumberControlDescriptionValue,
      height: validSliderControlDescriptionValue,
    }
    const expectedResult: ParseResult<ParsedPropertyControls> = right({
      width: right(validNumberControlDescriptionValue),
      height: right(validSliderControlDescriptionValue),
    })
    expect(parsePropertyControls(propertyControlsValue)).toEqual(expectedResult)
  })
  it('returns the property controls fully parsed when some are invalid', () => {
    const propertyControlsValue = {
      width: validNumberControlDescriptionValue,
      height: {
        ...validSliderControlDescriptionValue,
        defaultValue: 'hat',
      },
    }
    const expectedResult: ParseResult<ParsedPropertyControls> = right({
      width: right(validNumberControlDescriptionValue),
      height: left(
        objectFieldParseError('defaultValue', descriptionParseError('Value is not a number.')),
      ),
    })
    expect(parsePropertyControls(propertyControlsValue)).toEqual(expectedResult)
  })
  it('gives an error if the entire value is invalid', () => {
    const propertyControlsValue = 5
    const expectedResult: ParseResult<ParsedPropertyControls> = left(
      descriptionParseError('Property controls are not an object.'),
    )
    expect(parsePropertyControls(propertyControlsValue)).toEqual(expectedResult)
  })
})
