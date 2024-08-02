import * as OPI from 'object-path-immutable'
import React from 'react'
import type { SpringValue } from 'react-spring'
import { animated } from 'react-spring'
import utils from '../../../../../utils/utils'
import {
  FlexRow,
  SquareButton,
  Icn,
  UtopiaTheme,
  InspectorSubsectionHeader,
  Icons,
} from '../../../../../uuiui'
import {
  NumberInput,
  useWrappedSubmitFactoryEmptyOrUnknownOnSubmitValue,
} from '../../../../../uuiui/inputs/number-input'
import { InspectorContextMenuWrapper } from '../../../../context-menu-wrapper'
import { addOnUnsetValues } from '../../../common/context-menu-items'
import type { ControlStatus } from '../../../common/control-status'
import { LabelBelowNumberTextStyles, type ControlStyles } from '../../../common/control-styles'
import type {
  CSSDefault,
  CSSNumber,
  CSSNumberType,
  CSSNumberUnit,
  CSSTransformDoubleLengthItem,
  CSSTransformItem,
  CSSTransforms,
  CSSTransformSingleLengthItem,
  CSSTransformSupportedType,
  EmptyInputValue,
} from '../../../common/css-utils'
import {
  cssTransformSupportedValidFunctionNames,
  cssUnknownArrayItem,
  defaultTransformRotate,
  defaultTransformRotateX,
  defaultTransformRotateY,
  defaultTransformRotateZ,
  defaultTransformScale,
  defaultTransformScaleX,
  defaultTransformScaleY,
  defaultTransformScaleZ,
  defaultTransformSkew,
  defaultTransformSkewX,
  defaultTransformSkewY,
  defaultTransformTranslate,
  defaultTransformTranslateX,
  defaultTransformTranslateY,
  defaultTransformTranslateZ,
  fallbackOnEmptyInputValueToCSSDefaultEmptyValue,
  fallbackOnEmptyInputValueToCSSEmptyValue,
  isCSSDefault,
  isCSSTransformDoubleItem,
  isCSSTransformSingleItem,
} from '../../../common/css-utils'
import {
  RemovePropertyButton,
  getIndexedSpliceArrayItem,
  stopPropagation,
  useGetSubsectionHeaderStyle,
} from '../../../common/inspector-utils'
import type { UseSubmitValueFactory } from '../../../common/property-path-hooks'
import { useInspectorStyleInfo, useIsSubSectionVisible } from '../../../common/property-path-hooks'
import { useArraySuperControl } from '../../../controls/array-supercontrol'
import { BooleanControl } from '../../../controls/boolean-control'
import type { OnSubmitValue } from '../../../controls/control'
import { LightSelectControl } from '../../../controls/lightselect-control'
import type { SelectOption } from '../../../controls/select-control'
import { FakeUnknownArrayItem, UnknownArrayItem } from '../../../controls/unknown-array-item'
import { PropertyRow } from '../../../widgets/property-row'

const ObjectPathImmutable: any = OPI

interface TransformItemProps {
  index: number
  useSubmitValueFactory: UseSubmitValueFactory<CSSTransforms>
  controlStatus: ControlStatus
  controlStyles: ControlStyles
}

function getIndexedToggleTransformItemEnabled(index: number) {
  return function indexedToggleTransformItemEnabled(
    enabled: boolean,
    oldValue: CSSTransforms,
  ): CSSTransforms {
    let newCSSTransforms = [...oldValue]
    newCSSTransforms[index].enabled = enabled
    return newCSSTransforms
  }
}

interface TransformSelectOption extends SelectOption {
  value: CSSTransformSupportedType
}

interface TransformItemControlMetadata {
  emptyValue: CSSTransformItem
  prettyName: string
  minimum?: number
  maximum?: number
  stepSize?: number
  labelBelow?: [string, string?]
  numberType: CSSNumberType
  defaultUnitToHide: CSSNumberUnit | null
}

const transformItemControlMetadatas: {
  [key in CSSTransformItem['type']]: TransformItemControlMetadata
} = {
  rotate: {
    prettyName: 'Rotate',
    numberType: 'Angle',
    emptyValue: defaultTransformRotate,
    defaultUnitToHide: 'deg',
  },
  rotateX: {
    prettyName: 'Rotate X',
    numberType: 'Angle',
    emptyValue: defaultTransformRotateX,
    defaultUnitToHide: 'deg',
  },
  rotateY: {
    prettyName: 'Rotate Y',
    numberType: 'Angle',
    emptyValue: defaultTransformRotateY,
    defaultUnitToHide: 'deg',
  },
  rotateZ: {
    prettyName: 'Rotate Z',
    numberType: 'Angle',
    emptyValue: defaultTransformRotateZ,
    defaultUnitToHide: 'deg',
  },
  scale: {
    prettyName: 'Scale',
    stepSize: 0.01,
    numberType: 'Unitless',
    labelBelow: ['X', 'Y'],
    emptyValue: defaultTransformScale,
    defaultUnitToHide: null,
  },
  scaleX: {
    prettyName: 'Scale X',
    stepSize: 0.01,
    numberType: 'Unitless',
    emptyValue: defaultTransformScaleX,
    defaultUnitToHide: null,
  },
  scaleY: {
    prettyName: 'Scale Y',
    stepSize: 0.01,
    numberType: 'Unitless',
    emptyValue: defaultTransformScaleY,
    defaultUnitToHide: null,
  },
  scaleZ: {
    prettyName: 'Scale Z',
    stepSize: 0.01,
    numberType: 'Unitless',
    emptyValue: defaultTransformScaleZ,
    defaultUnitToHide: null,
  },
  skew: {
    prettyName: 'Skew',
    labelBelow: ['X', 'Y'],
    numberType: 'Angle',
    emptyValue: defaultTransformSkew,
    defaultUnitToHide: 'deg',
  },
  skewX: {
    prettyName: 'Skew X',
    numberType: 'Angle',
    emptyValue: defaultTransformSkewX,
    defaultUnitToHide: 'deg',
  },
  skewY: {
    prettyName: 'Skew Y',
    numberType: 'Angle',
    emptyValue: defaultTransformSkewY,
    defaultUnitToHide: 'deg',
  },
  translate: {
    prettyName: 'Translate',
    labelBelow: ['X', 'Y'],
    numberType: 'LengthPercent',
    emptyValue: defaultTransformTranslate,
    defaultUnitToHide: 'px',
  },
  translateX: {
    prettyName: 'Translate X',
    numberType: 'LengthPercent',
    emptyValue: defaultTransformTranslateX,
    defaultUnitToHide: 'px',
  },
  translateY: {
    prettyName: 'Translate Y',
    numberType: 'LengthPercent',
    emptyValue: defaultTransformTranslateY,
    defaultUnitToHide: 'px',
  },
  translateZ: {
    prettyName: 'Translate Z',
    numberType: 'LengthPercent',
    emptyValue: defaultTransformTranslateZ,
    defaultUnitToHide: 'px',
  },
  'unknown-array-item': {
    prettyName: 'Unknown',
    numberType: 'AnyValid',
    emptyValue: cssUnknownArrayItem(''),
    defaultUnitToHide: null,
  },
}

const transformSelectOptions: Array<TransformSelectOption> =
  cssTransformSupportedValidFunctionNames.map((supportedTransformFunctionName) => ({
    value: supportedTransformFunctionName,
    label: transformItemControlMetadatas[supportedTransformFunctionName].prettyName,
  }))

export function getIndexedOnTransformTypeSelectSubmitValue(transformIndex: number) {
  return function onTransformTypeSelectSubmitValue(
    newTransformType: CSSTransformSupportedType,
    oldValue: CSSTransforms,
  ): CSSTransforms {
    let newCSSTransforms = [...oldValue]
    newCSSTransforms[transformIndex] = {
      ...transformItemControlMetadatas[newTransformType].emptyValue,
    }
    return newCSSTransforms
  }
}

interface SingleLengthItemProps extends TransformItemProps {
  value: CSSTransformSingleLengthItem
  emptyValue: CSSTransformSingleLengthItem
}

const transformsToInsert: Array<CSSTransformSupportedType> = [
  'rotate',
  'scale',
  'skew',
  'translate',
]

function insertCSSTransform(
  oldValue: CSSTransforms,
  onSubmitValue: OnSubmitValue<CSSTransforms>,
): void {
  let submitted = false
  for (const transformItemToCheck of transformsToInsert) {
    if (
      oldValue.every((oldValueTransformItem) => oldValueTransformItem.type !== transformItemToCheck)
    ) {
      onSubmitValue([
        ...oldValue,
        { ...transformItemControlMetadatas[transformItemToCheck].emptyValue },
      ])
      submitted = true
      break
    }
  }
  if (!submitted) {
    onSubmitValue([...oldValue, { ...defaultTransformTranslate }])
  }
}

function getIndexedUpdateSingleLengthValue(
  index: number,
  emptyValue: CSSTransformSingleLengthItem,
) {
  return function indexedUpdateSingleLengthValueEnabled(
    value: CSSNumber | EmptyInputValue,
    oldValue: CSSTransforms,
  ): CSSTransforms {
    let newCSSTransforms = [...oldValue]
    let newTransformValue: CSSTransformItem = { ...newCSSTransforms[index] }
    if (isCSSTransformSingleItem(newTransformValue)) {
      newTransformValue.value = fallbackOnEmptyInputValueToCSSEmptyValue(emptyValue.value, value)
      newCSSTransforms[index] = newTransformValue
    }
    return newCSSTransforms
  }
}

const SingleLengthItem = React.memo<SingleLengthItemProps>((props) => {
  const [enabledSubmitValue] = props.useSubmitValueFactory(
    getIndexedToggleTransformItemEnabled(props.index),
  )
  const [onTransformTypeSubmitValue] = props.useSubmitValueFactory(
    getIndexedOnTransformTypeSelectSubmitValue(props.index),
  )
  const [singleLengthItemSubmitValue, singleLengthItemTransientSubmitValue] =
    useWrappedSubmitFactoryEmptyOrUnknownOnSubmitValue(
      props.useSubmitValueFactory(getIndexedUpdateSingleLengthValue(props.index, props.emptyValue)),
    )

  const [deleteTransformItemSubmitValue] = props.useSubmitValueFactory(
    getIndexedSpliceArrayItem(props.index),
  )

  const removeTransformItem = React.useCallback(
    (e: React.MouseEvent<HTMLImageElement>) => {
      e.stopPropagation()
      deleteTransformItemSubmitValue(null)
    },
    [deleteTransformItemSubmitValue],
  )

  const controlMetadata = transformItemControlMetadatas[props.value.type]

  return (
    <PropertyRow
      key={props.index}
      style={{
        gridTemplateColumns: '12px 1fr 46px 12px',
        gridColumnGap: 8,
      }}
    >
      <BooleanControl
        style={{ gridColumn: '1 / span 1' }}
        id={`transform-${props.index}-${props.value.type}-enable-disable`}
        key={`transform-${props.index}-${props.value.type}-enable-disable`}
        testId={`transform-${props.index}-${props.value.type}-enable-disable`}
        value={props.value.enabled}
        onSubmitValue={enabledSubmitValue}
        controlStatus={props.controlStatus}
        controlStyles={props.controlStyles}
        onMouseDown={stopPropagation}
      />
      <FlexRow style={{ alignItems: 'start' }} onMouseDown={stopPropagation}>
        <LightSelectControl
          id={`transform-${props.index}-transform-type`}
          key={`transform-${props.index}-transform-type`}
          testId={`transform-${props.index}-transform-type`}
          value={props.value.type}
          options={transformSelectOptions}
          onSubmitValue={onTransformTypeSubmitValue}
          controlStatus={props.controlStatus}
          controlStyles={props.controlStyles}
        />
      </FlexRow>
      <NumberInput
        style={{
          gridColumn: '3 / span 1',
          width: '100%',
        }}
        id={`transforms.${props.value.type}`}
        testId={`transforms.${props.value.type}`}
        key={`transforms.${props.value.type}`}
        value={props.value.value}
        stepSize={controlMetadata.stepSize}
        minimum={controlMetadata.minimum}
        maximum={controlMetadata.maximum}
        numberType={controlMetadata.numberType}
        onSubmitValue={singleLengthItemSubmitValue}
        onTransientSubmitValue={singleLengthItemTransientSubmitValue}
        controlStatus={props.controlStatus}
        defaultUnitToHide={controlMetadata.defaultUnitToHide}
        incrementControls={false}
      />
      <SquareButton
        highlightOnHover
        onMouseDown={removeTransformItem}
        style={{ marginTop: 1, width: 12 }}
      >
        <Icons.Minus width={12} height={12} />
      </SquareButton>
    </PropertyRow>
  )
})

function getIndexedUpdateDoubleLengthValue(
  itemIndex: number,
  lengthIndex: number,
  emptyValue: CSSNumber | CSSDefault<CSSNumber>,
) {
  return function indexedUpdateDoubleLengthFirstValueEnabled(
    value: CSSNumber | EmptyInputValue,
    oldValue: CSSTransforms,
  ): CSSTransforms {
    let newCSSTransforms = [...oldValue]
    let newTransformValue: CSSTransformItem = { ...newCSSTransforms[itemIndex] }
    if (isCSSTransformDoubleItem(newTransformValue)) {
      let indexedValue = { ...newTransformValue.values[lengthIndex] }
      if (isCSSDefault(indexedValue) && isCSSDefault(emptyValue)) {
        indexedValue = fallbackOnEmptyInputValueToCSSDefaultEmptyValue(emptyValue, value)
      } else {
        indexedValue = fallbackOnEmptyInputValueToCSSEmptyValue(emptyValue, value)
      }
      newTransformValue.values[lengthIndex] = indexedValue
      let newFirstTransformValue = { ...newTransformValue.values[1] }
      if (isCSSDefault(newFirstTransformValue)) {
        newFirstTransformValue.default = false
      }
      newTransformValue.values[1] = newFirstTransformValue
      newCSSTransforms[itemIndex] = newTransformValue
    }
    return newCSSTransforms
  }
}

interface DoubleLengthItemProps extends TransformItemProps {
  value: CSSTransformDoubleLengthItem
  emptyValue: CSSTransformDoubleLengthItem
}

const DoubleLengthItem = React.memo<DoubleLengthItemProps>((props) => {
  const [enabledSubmitValue] = props.useSubmitValueFactory(
    getIndexedToggleTransformItemEnabled(props.index),
  )
  const [onTransformTypeSubmitValue] = props.useSubmitValueFactory(
    getIndexedOnTransformTypeSelectSubmitValue(props.index),
  )
  const [doubleLengthZeroethItemSubmitValue, doubleLengthZeroethItemTransientSubmitValue] =
    useWrappedSubmitFactoryEmptyOrUnknownOnSubmitValue(
      props.useSubmitValueFactory(
        getIndexedUpdateDoubleLengthValue(props.index, 0, props.emptyValue.values[0]),
      ),
    )
  const [doubleLengthFirstItemSubmitValue, doubleLengthFirstItemTransientSubmitValue] =
    useWrappedSubmitFactoryEmptyOrUnknownOnSubmitValue(
      props.useSubmitValueFactory(
        getIndexedUpdateDoubleLengthValue(props.index, 1, props.emptyValue.values[1]),
      ),
    )

  const [deleteTransformItemSubmitValue] = props.useSubmitValueFactory(
    getIndexedSpliceArrayItem(props.index),
  )

  const removeTransformItem = React.useCallback(
    (e: React.MouseEvent<HTMLImageElement>) => {
      e.stopPropagation()
      deleteTransformItemSubmitValue(null)
    },
    [deleteTransformItemSubmitValue],
  )

  const controlMetadata = transformItemControlMetadatas[props.value.type]

  const firstLabel = controlMetadata.labelBelow?.at(0)
  const secondLabel = controlMetadata.labelBelow?.at(1)

  return (
    <PropertyRow
      key={props.index}
      style={{
        gridTemplateColumns: '12px 1fr 46px 46px 12px',
        gridColumnGap: 8,
      }}
    >
      <BooleanControl
        style={{ gridColumn: '1 / span 1' }}
        id={`transform-${props.index}-rotateZ-enable-disable`}
        key={`transform-${props.index}-rotateZ-enable-disable`}
        testId={`transform-${props.index}-rotateZ-enable-disable`}
        value={props.value.enabled}
        onSubmitValue={enabledSubmitValue}
        controlStatus={props.controlStatus}
        controlStyles={props.controlStyles}
        onMouseDown={stopPropagation}
      />
      <FlexRow style={{ alignItems: 'start' }} onMouseDown={stopPropagation}>
        <LightSelectControl
          id={`transform-${props.index}-transform-type`}
          key={`transform-${props.index}-transform-type`}
          testId={`transform-${props.index}-transform-type`}
          value={props.value.type}
          options={transformSelectOptions}
          onSubmitValue={onTransformTypeSubmitValue}
          controlStatus={props.controlStatus}
          controlStyles={props.controlStyles}
        />
      </FlexRow>
      <NumberInput
        style={{
          gridColumn: '3 / span 1',
          width: '100%',
        }}
        id={`transform-${props.value.type}-0th-value`}
        testId={`transform-${props.value.type}-0th-value`}
        key={`transform-${props.value.type}-0th-value`}
        value={props.value.values[0]}
        stepSize={controlMetadata.stepSize}
        minimum={controlMetadata.minimum}
        maximum={controlMetadata.maximum}
        numberType={controlMetadata.numberType}
        DEPRECATED_labelBelow={firstLabel}
        labelBelowStyle={firstLabel == null ? LabelBelowNumberTextStyles : undefined}
        onSubmitValue={doubleLengthZeroethItemSubmitValue}
        onTransientSubmitValue={doubleLengthZeroethItemTransientSubmitValue}
        controlStatus={props.controlStatus}
        defaultUnitToHide={controlMetadata.defaultUnitToHide}
        incrementControls={false}
      />
      <NumberInput
        style={{
          gridColumn: '4 / span 1',
          width: '100%',
        }}
        id={`transform-${props.value.type}-1st-value`}
        testId={`transform-${props.value.type}-1st-value`}
        key={`transform-${props.value.type}-1st-value`}
        value={props.value.values[1].value}
        stepSize={controlMetadata.stepSize}
        minimum={controlMetadata.minimum}
        maximum={controlMetadata.maximum}
        numberType={controlMetadata.numberType}
        DEPRECATED_labelBelow={secondLabel}
        labelBelowStyle={secondLabel == null ? LabelBelowNumberTextStyles : undefined}
        onSubmitValue={doubleLengthFirstItemSubmitValue}
        onTransientSubmitValue={doubleLengthFirstItemTransientSubmitValue}
        controlStatus={props.controlStatus}
        defaultUnitToHide={controlMetadata.defaultUnitToHide}
        incrementControls={false}
      />
      <SquareButton
        highlightOnHover
        onMouseDown={removeTransformItem}
        style={{ marginTop: 1, width: 12 }}
      >
        <Icons.Minus width={12} height={12} />
      </SquareButton>
    </PropertyRow>
  )
})

const CSSTransformsToTransform = (transformedType: CSSTransforms) => {
  if (Array.isArray(transformedType) && transformedType.length !== 0) {
    return {
      transform: transformedType,
    }
  } else {
    return {}
  }
}

const rowHeight = UtopiaTheme.layout.rowHeight.normal

export const TransformSubsection = React.memo(() => {
  const {
    value,
    onSubmitValue,
    onUnsetValues,
    controlStatus,
    controlStyles,
    useSubmitValueFactory,
    propertyStatus,
  } = useInspectorStyleInfo('transform', undefined, CSSTransformsToTransform)

  const transformContextMenuItems = utils.stripNulls([
    controlStyles.unsettable ? addOnUnsetValues(['transform'], onUnsetValues) : null,
  ])

  const { springs, bind } = useArraySuperControl(value, onSubmitValue, rowHeight)

  const insertCSSTransformMouseDown = React.useCallback(() => {
    insertCSSTransform(value, onSubmitValue)
  }, [onSubmitValue, value])

  const removeAllTransformProperties = React.useCallback(() => {
    onUnsetValues()
  }, [onUnsetValues])

  const headerStyle = useGetSubsectionHeaderStyle(controlStatus)

  const isVisible = useIsSubSectionVisible('transform')
  if (!isVisible) {
    return null
  } else {
    return (
      <InspectorContextMenuWrapper
        id='transform-subsection-context-menu'
        items={transformContextMenuItems}
        data={null}
      >
        <InspectorSubsectionHeader style={headerStyle}>
          <FlexRow style={{ flexGrow: 1, gap: 8 }}>
            <span>Transforms</span>
          </FlexRow>
          {propertyStatus.overwritable ? (
            <FlexRow style={{ gap: 4 }}>
              <RemovePropertyButton
                testId='inspector-transform-remove-all'
                onUnsetValues={removeAllTransformProperties}
                propertySet={propertyStatus.set}
              />
              <SquareButton
                highlightOnHover
                onMouseDown={insertCSSTransformMouseDown}
                style={{ width: 12 }}
              >
                <Icn category='semantic' type='plus' width={12} height={12} />
              </SquareButton>
            </FlexRow>
          ) : null}
        </InspectorSubsectionHeader>
        {controlStyles.unknown ? (
          <FakeUnknownArrayItem controlStatus={controlStatus} />
        ) : (
          <div
            style={{
              height: rowHeight * springs.length,
            }}
          >
            {springs.map((springStyle: { [x: string]: SpringValue<any> }, index: number) => {
              const transformItem = value[index]
              if (transformItem != null) {
                let item: JSX.Element
                if (isCSSTransformSingleItem(transformItem)) {
                  item = (
                    <SingleLengthItem
                      key={index}
                      value={transformItem}
                      emptyValue={
                        transformItemControlMetadatas[transformItem.type]
                          .emptyValue as CSSTransformSingleLengthItem
                      }
                      index={index}
                      controlStatus={controlStatus}
                      controlStyles={controlStyles}
                      useSubmitValueFactory={useSubmitValueFactory}
                    />
                  )
                } else if (isCSSTransformDoubleItem(transformItem)) {
                  item = (
                    <DoubleLengthItem
                      key={index}
                      value={transformItem}
                      emptyValue={
                        transformItemControlMetadatas[transformItem.type]
                          .emptyValue as CSSTransformDoubleLengthItem
                      }
                      index={index}
                      controlStatus={controlStatus}
                      controlStyles={controlStyles}
                      useSubmitValueFactory={useSubmitValueFactory}
                    />
                  )
                } else {
                  item = (
                    <UnknownArrayItem
                      index={index}
                      key={`unknown-${index}`}
                      useSubmitTransformedValuesFactory={useSubmitValueFactory}
                      value={transformItem}
                      controlStatus={controlStatus}
                      controlStyles={controlStyles}
                    />
                  )
                }
                return (
                  <animated.div
                    {...bind(index)}
                    key={index}
                    style={{
                      ...springStyle,
                      width: '100%',
                      position: 'absolute',

                      height: rowHeight,
                    }}
                  >
                    {item}
                  </animated.div>
                )
              } else {
                throw new Error(`No transform item exists at index ${index}`)
              }
            })}
          </div>
        )}
      </InspectorContextMenuWrapper>
    )
  }
})
TransformSubsection.displayName = 'TransformSubsection'
