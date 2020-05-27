import {
  NumberControlDescription,
  EnumControlDescription,
  BooleanControlDescription,
  StringControlDescription,
  ColorControlDescription,
  FusedNumberControlDescription,
  ImageControlDescription,
  FileControlDescription,
  ComponentInstanceDescription,
  ArrayControlDescription,
  EventHandlerControlDescription,
  SliderControlDescription,
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
  it('handles fusednumber descriptions', () => {
    const fusedNumberDescription: FusedNumberControlDescription = {
      type: 'fusednumber',
      toggleKey: 'toggle',
      toggleTitles: ['on', 'off'],
      valueKeys: ['1', '2', '3', '4'],
      valueLabels: ['1', '2', '3', '4'],
      defaultValue: 1,
    }
    checkSameArrayElementsAndLength(getDescriptionUnsetOptionalFields(fusedNumberDescription), [
      'min',
      'title',
    ])
  })
  it('handles image descriptions', () => {
    const imageDescription: ImageControlDescription = {
      type: 'image',
    }
    checkSameArrayElementsAndLength(getDescriptionUnsetOptionalFields(imageDescription), ['title'])
  })
  it('handles file descriptions', () => {
    const fileDescription: FileControlDescription = {
      type: 'file',
      allowedFileTypes: ['.hs'],
    }
    checkSameArrayElementsAndLength(getDescriptionUnsetOptionalFields(fileDescription), ['title'])
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
  it('handles slider descriptions', () => {
    const sliderDescription: SliderControlDescription = {
      type: 'slider',
      min: 1,
      max: 1,
      step: 1,
    }
    checkSameArrayElementsAndLength(getDescriptionUnsetOptionalFields(sliderDescription), [
      'title',
      'defaultValue',
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
