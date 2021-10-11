import {
  NumberControlDescription,
  EnumControlDescription,
  BooleanControlDescription,
  StringControlDescription,
  ColorControlDescription,
  ImageControlDescription,
  ComponentInstanceDescription,
  ArrayControlDescription,
  EventHandlerControlDescription,
  PopUpListControlDescription,
  OptionsControlDescription,
} from 'utopia-api'
import { getDescriptionUnsetOptionalFields } from './property-controls-utils'

function checkSameArrayElementsAndLength<T>(value: Array<T>, checkAgainst: Array<T>): void {
  expect(value).toEqual(expect.arrayContaining(checkAgainst))
  expect(value).toHaveLength(checkAgainst.length)
}

describe('getDescriptionUnsetOptionalFields', () => {
  it('handles number descriptions', () => {
    const numberDescription: NumberControlDescription = {
      type: 'number',
      defaultValue: 9,
      step: 5,
    }
    checkSameArrayElementsAndLength(getDescriptionUnsetOptionalFields(numberDescription), [
      'min',
      'title',
      'max',
      'unit',
      'displayStepper',
    ])
  })
  it('handles enum descriptions', () => {
    const enumDescription: EnumControlDescription = {
      type: 'enum',
      options: ['1', true, 5],
    }
    checkSameArrayElementsAndLength(getDescriptionUnsetOptionalFields(enumDescription), [
      'title',
      'defaultValue',
      'optionTitles',
      'displaySegmentedControl',
    ])
  })
  it('handles boolean descriptions', () => {
    const booleanDescription: BooleanControlDescription = {
      type: 'boolean',
      enabledTitle: 'yesss',
    }
    checkSameArrayElementsAndLength(getDescriptionUnsetOptionalFields(booleanDescription), [
      'title',
      'defaultValue',
      'disabledTitle',
    ])
  })
  it('handles string descriptions', () => {
    const stringDescription: StringControlDescription = {
      type: 'string',
      placeholder: 'Enter text',
    }
    checkSameArrayElementsAndLength(getDescriptionUnsetOptionalFields(stringDescription), [
      'title',
      'defaultValue',
      'obscured',
    ])
  })
  it('handles color descriptions', () => {
    const colorDescription: ColorControlDescription = {
      type: 'color',
    }
    checkSameArrayElementsAndLength(getDescriptionUnsetOptionalFields(colorDescription), [
      'title',
      'defaultValue',
    ])
  })
  it('handles image descriptions', () => {
    const imageDescription: ImageControlDescription = {
      type: 'image',
    }
    checkSameArrayElementsAndLength(getDescriptionUnsetOptionalFields(imageDescription), ['title'])
  })
  it('handles componentinstance descriptions', () => {
    const componentInstanceDescription: ComponentInstanceDescription = {
      type: 'componentinstance',
    }
    checkSameArrayElementsAndLength(
      getDescriptionUnsetOptionalFields(componentInstanceDescription),
      ['title'],
    )
  })
  it('handles array descriptions', () => {
    const arrayDescription: ArrayControlDescription = {
      type: 'array',
      maxCount: 10,
      propertyControl: {
        type: 'string',
      },
    }
    checkSameArrayElementsAndLength(getDescriptionUnsetOptionalFields(arrayDescription), [
      'title',
      'defaultValue',
    ])
  })
  it('handles eventhandler descriptions', () => {
    const eventHandlerDescription: EventHandlerControlDescription = {
      type: 'eventhandler',
    }
    checkSameArrayElementsAndLength(getDescriptionUnsetOptionalFields(eventHandlerDescription), [
      'title',
    ])
  })
  it('handles popuplist descriptions', () => {
    const popupListDescription: PopUpListControlDescription = {
      type: 'popuplist',
      defaultValue: 1,
      options: [
        {
          label: 'First',
          value: 1,
        },
        {
          label: 'Second',
          value: 2,
        },
      ],
    }
    checkSameArrayElementsAndLength(getDescriptionUnsetOptionalFields(popupListDescription), [
      'title',
    ])
  })
  it('handles options descriptions', () => {
    const optionsDescription: OptionsControlDescription = {
      type: 'options',
      title: 'Options',
      options: [
        {
          label: 'First',
          value: 1,
        },
        {
          label: 'Second',
          value: 2,
        },
      ],
    }
    checkSameArrayElementsAndLength(getDescriptionUnsetOptionalFields(optionsDescription), [
      'defaultValue',
    ])
  })
})
