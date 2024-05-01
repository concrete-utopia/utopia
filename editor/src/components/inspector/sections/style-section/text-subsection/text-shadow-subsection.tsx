import React from 'react'
import type { SpringValue } from 'react-spring'
import { animated } from 'react-spring'
import { BooleanControl } from '../../../controls/boolean-control'
import { ColorControl } from '../../../controls/color-control'
import type { ControlStatus } from '../../../common/control-status'
import type { ControlStyles } from '../../../common/control-styles'
import { PropertyRow, PropertyRowHeightWithLabel } from '../../../widgets/property-row'
import { useArraySuperControl } from '../../../controls/array-supercontrol'
import type {
  CSSColor,
  CSSTextShadow,
  CSSTextShadows,
  CSSNumber,
  EmptyInputValue,
} from '../../../common/css-utils'
import {
  defaultTextShadow,
  cssNumber,
  fallbackOnEmptyInputValueToCSSEmptyValue,
  fallbackOnEmptyInputValueToCSSDefaultEmptyValue,
} from '../../../common/css-utils'
import { useInspectorStyleInfo, useIsSubSectionVisible } from '../../../common/property-path-hooks'
import { stopPropagation, useGetSubsectionHeaderStyle } from '../../../common/inspector-utils'
import { FakeUnknownArrayItem } from '../../../controls/unknown-array-item'
import { InspectorContextMenuWrapper } from '../../../../context-menu-wrapper'
import { addOnUnsetValues } from '../../../common/context-menu-items'
import utils from '../../../../../utils/utils'
import {
  useWrappedSubmitFactoryEmptyOrUnknownOnSubmitValue,
  NumberInput,
  SquareButton,
  InspectorSubsectionHeader,
  FlexRow,
  Icons,
  Icn,
} from '../../../../../uuiui'

function getIndexedToggleTextShadowEnabled(index: number) {
  return function indexedToggleTextShadowEnabled(
    enabled: boolean,
    cssTextShadows: CSSTextShadows,
  ): CSSTextShadows {
    let newTextShadow = [...cssTextShadows]
    newTextShadow[index].enabled = enabled
    return newTextShadow
  }
}

function getIndexedUpdateTextShadowColor(index: number) {
  return function updateTextShadowColor(
    newColor: CSSColor,
    cssTextShadows: CSSTextShadows,
  ): CSSTextShadows {
    let newTextShadows = [...cssTextShadows]
    let newTextShadow: CSSTextShadow = { ...newTextShadows[index] }
    newTextShadow.color = newColor
    newTextShadows[index] = newTextShadow
    return newTextShadows
  }
}

function getIndexedUpdateTextShadowOffsetX(index: number) {
  return function updateTextShadowOffsetX(
    newOffsetX: CSSNumber | EmptyInputValue,
    cssTextShadows: CSSTextShadows,
  ): CSSTextShadows {
    let newTextShadows = [...cssTextShadows]
    let newTextShadow: CSSTextShadow = { ...newTextShadows[index] }
    newTextShadow.offsetX = fallbackOnEmptyInputValueToCSSEmptyValue(
      {
        ...defaultTextShadow.offsetX,
      },
      newOffsetX,
    )
    newTextShadows[index] = newTextShadow
    return newTextShadows
  }
}

function getIndexedUpdateTextShadowOffsetY(index: number) {
  return function updateTextShadowOffsetY(
    newOffsetY: CSSNumber | EmptyInputValue,
    cssTextShadows: CSSTextShadows,
  ): CSSTextShadows {
    let newTextShadows = [...cssTextShadows]
    let newTextShadow: CSSTextShadow = { ...newTextShadows[index] }
    newTextShadow.offsetY = fallbackOnEmptyInputValueToCSSEmptyValue(
      {
        ...defaultTextShadow.offsetY,
      },
      newOffsetY,
    )
    newTextShadows[index] = newTextShadow
    return newTextShadows
  }
}

function getIndexedUpdateTextShadowBlurRadius(index: number) {
  return function updateTextShadowBlurRadius(
    newBlurRadius: CSSNumber | EmptyInputValue,
    cssTextShadows: CSSTextShadows,
  ): CSSTextShadows {
    let newTextShadows = [...cssTextShadows]
    let newTextShadow: CSSTextShadow = { ...newTextShadows[index] }
    newTextShadow.blurRadius = fallbackOnEmptyInputValueToCSSDefaultEmptyValue(
      defaultTextShadow.blurRadius,
      newBlurRadius,
    )
    newTextShadows[index] = newTextShadow
    return newTextShadows
  }
}

function getIndexedSpliceTextShadow(index: number) {
  return function spliceTextShadow(_: any, cssTextShadows: CSSTextShadows): CSSTextShadows {
    let result = [...cssTextShadows]
    return result.splice(index, 1)
  }
}

function insertTextShadow(_: any, cssTextShadows: CSSTextShadows): CSSTextShadows {
  return [...cssTextShadows, defaultTextShadow]
}

type TextShadowItemProps = {
  value: CSSTextShadow
  index: number
  textShadowsLength: number
  controlStatus: ControlStatus
  controlStyles: ControlStyles
  useSubmitValueFactory: <NT>(
    transform: (newValue: NT, oldValue: CSSTextShadows) => CSSTextShadows,
  ) => [(newValue: NT) => void, (newValue: NT) => void]
}

const zeroBlurRadius = cssNumber(0, 'px')

const TextShadowItem = React.memo<TextShadowItemProps>((props) => {
  const [enabledSubmitValue] = props.useSubmitValueFactory(
    getIndexedToggleTextShadowEnabled(props.index),
  )
  const [colorSubmitValue, colorTransientSubmitValue] = props.useSubmitValueFactory(
    getIndexedUpdateTextShadowColor(props.index),
  )
  const [offsetXSubmitValue, offsetXTransientSubmitValue] =
    useWrappedSubmitFactoryEmptyOrUnknownOnSubmitValue(
      props.useSubmitValueFactory(getIndexedUpdateTextShadowOffsetX(props.index)),
    )
  const [offsetYSubmitValue, offsetYTransientSubmitValue] =
    useWrappedSubmitFactoryEmptyOrUnknownOnSubmitValue(
      props.useSubmitValueFactory(getIndexedUpdateTextShadowOffsetY(props.index)),
    )
  const [blurRadiusSubmitValue, blurRadiusTransientSubmitValue] =
    useWrappedSubmitFactoryEmptyOrUnknownOnSubmitValue(
      props.useSubmitValueFactory(getIndexedUpdateTextShadowBlurRadius(props.index)),
    )
  const [deleteTextShadowItemSubmitValue] = useWrappedSubmitFactoryEmptyOrUnknownOnSubmitValue(
    props.useSubmitValueFactory(getIndexedSpliceTextShadow(props.index)),
  )

  const removeShadow = React.useCallback(
    (e: React.MouseEvent<HTMLImageElement>) => {
      e.stopPropagation()
      deleteTextShadowItemSubmitValue(null)
    },
    [deleteTextShadowItemSubmitValue],
  )

  return (
    <PropertyRow
      key={props.index}
      style={{
        gridTemplateColumns: '12px 28px repeat(3, 1fr) 20px',
        gridColumnGap: 8,
      }}
    >
      <BooleanControl
        style={{ gridColumn: '1 / span 1' }}
        id='textShadow-enable-disable'
        key='textShadow-enable-disable'
        testId='textShadow-enable-disable'
        value={props.value.enabled}
        onSubmitValue={enabledSubmitValue}
        controlStatus={props.controlStatus}
        controlStyles={props.controlStyles}
        onMouseDown={stopPropagation}
      />
      <ColorControl
        style={{ gridColumn: '2 / span 1' }}
        id='textShadow-Color'
        key='textShadow-Color'
        testId='textShadow-Color'
        value={props.value.color}
        onSubmitValue={colorSubmitValue}
        onTransientSubmitValue={colorTransientSubmitValue}
        pickerOffset={{ x: -45, y: 0 }}
        controlStatus={props.controlStatus}
        controlStyles={props.controlStyles}
      />
      <NumberInput
        style={{ gridColumn: '3 / span 1' }}
        value={props.value.offsetX}
        DEPRECATED_labelBelow='x'
        id={`textShadow-offsetX-${props.index}`}
        testId={`textShadow-offsetX-${props.index}`}
        onSubmitValue={offsetXSubmitValue}
        onTransientSubmitValue={offsetXTransientSubmitValue}
        controlStatus={props.controlStatus}
        inputProps={{ onMouseDown: stopPropagation }}
        numberType='Length'
        defaultUnitToHide={'px'}
      />
      <NumberInput
        style={{ gridColumn: '4 / span 1' }}
        value={props.value.offsetY}
        DEPRECATED_labelBelow='y'
        id={`textShadow-offsetY-${props.index}`}
        testId={`textShadow-offsetY-${props.index}`}
        onSubmitValue={offsetYSubmitValue}
        onTransientSubmitValue={offsetYTransientSubmitValue}
        controlStatus={props.controlStatus}
        inputProps={{ onMouseDown: stopPropagation }}
        numberType='Length'
        defaultUnitToHide={'px'}
      />
      <NumberInput
        style={{ gridColumn: '5 / span 1' }}
        value={props.value.blurRadius == null ? zeroBlurRadius : props.value.blurRadius.value}
        DEPRECATED_labelBelow='blur'
        id={`textShadow-blurRadius-${props.index}`}
        testId={`textShadow-blurRadius-${props.index}`}
        onSubmitValue={blurRadiusSubmitValue}
        onTransientSubmitValue={blurRadiusTransientSubmitValue}
        controlStatus={props.controlStatus}
        inputProps={{ onMouseDown: stopPropagation }}
        numberType='Length'
        defaultUnitToHide={'px'}
      />
      <SquareButton highlight onMouseDown={removeShadow} style={{ marginTop: 1 }}>
        <Icons.Minus />
      </SquareButton>
    </PropertyRow>
  )
})

export const TextShadowSubsection = React.memo(() => {
  const {
    value: textShadowsValue,
    controlStatus,
    controlStyles,
    onUnsetValues,
    onSubmitValue,
    useSubmitValueFactory,
    propertyStatus,
  } = useInspectorStyleInfo('textShadow', undefined, (transformedType: CSSTextShadows) => {
    if (Array.isArray(transformedType) && transformedType.length === 0) {
      return {}
    } else {
      return {
        textShadow: transformedType,
      }
    }
  })

  const [insertTextShadowItemSubmitValue] = useSubmitValueFactory(insertTextShadow)

  const isVisible = useIsSubSectionVisible('textShadow')

  const { springs, bind } = useArraySuperControl(
    textShadowsValue,
    onSubmitValue,
    PropertyRowHeightWithLabel,
  )

  const contextMenuItems = utils.stripNulls([
    textShadowsValue.length > 0 ? addOnUnsetValues(['textShadow'], onUnsetValues) : null,
  ])

  const length = textShadowsValue.length

  const insertShadow = React.useCallback(() => {
    insertTextShadowItemSubmitValue(null)
  }, [insertTextShadowItemSubmitValue])

  const headerStyle = useGetSubsectionHeaderStyle(controlStatus)

  if (isVisible) {
    return (
      <InspectorContextMenuWrapper
        id='text-shadow-subsection-context-menu'
        items={contextMenuItems}
        data={null}
      >
        <InspectorSubsectionHeader style={headerStyle}>
          <FlexRow
            style={{
              flexGrow: 1,
              gap: 8,
            }}
          >
            <span>Text Shadow</span>
          </FlexRow>
          {propertyStatus.overwritable ? (
            <FlexRow style={{ gap: 4 }}>
              <SquareButton
                highlight
                onMouseDown={onUnsetValues}
                data-testid={'inspector-text-shadow-remove-all'}
                style={{ width: 12 }}
              >
                <Icn category='semantic' type='cross' width={12} height={12} />
              </SquareButton>
              <SquareButton highlight onMouseDown={insertShadow} style={{ width: 12 }}>
                <Icn category='semantic' type='plus' width={12} height={12} />
              </SquareButton>
            </FlexRow>
          ) : null}
        </InspectorSubsectionHeader>
        <div
          style={{
            height: PropertyRowHeightWithLabel * springs.length,
          }}
        >
          {controlStyles.unknown ? (
            <FakeUnknownArrayItem controlStatus={controlStatus} />
          ) : (
            springs.map((springStyle: { [x: string]: SpringValue<any> }, index: number) => {
              const value = textShadowsValue[index]
              if (value != null) {
                return (
                  <animated.div
                    {...bind(index)}
                    key={index}
                    style={{
                      ...springStyle,
                      position: 'absolute',
                      height: PropertyRowHeightWithLabel,
                    }}
                  >
                    <TextShadowItem
                      key={index}
                      value={value}
                      textShadowsLength={length}
                      index={index}
                      controlStatus={controlStatus}
                      controlStyles={controlStyles}
                      useSubmitValueFactory={useSubmitValueFactory}
                    />
                  </animated.div>
                )
              } else {
                throw new Error(`No text shadow exists at index ${index}`)
              }
            })
          )}
        </div>
      </InspectorContextMenuWrapper>
    )
  } else {
    return null
  }
})
TextShadowSubsection.displayName = 'TextShadowSubsection'
