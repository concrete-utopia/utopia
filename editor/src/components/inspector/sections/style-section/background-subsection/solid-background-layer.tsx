import React from 'react'
import { isRight } from '../../../../../core/shared/either'
import { InspectorContextMenuWrapper } from '../../../../context-menu-wrapper'
import { removeRow } from '../../../common/context-menu-items'
import type {
  CSSBackgroundLayer,
  CSSBackgroundLayers,
  CSSSolidBackgroundLayer,
  CSSUnknownArrayItem,
  EmptyInputValue,
} from '../../../common/css-utils'
import {
  fallbackOnEmptyInputValueToCSSEmptyValue,
  giveCSSColorNewAlpha,
  parseAlphaFromCSSColor,
  parseColor,
  isCSSSolidBackgroundLayer,
} from '../../../common/css-utils'
import { getIndexedSpliceArrayItem } from '../../../common/inspector-utils'
import { stopPropagation } from '../../../common/inspector-utils'
import { UIGridRow } from '../../../widgets/ui-grid-row'
import type {
  BackgroundLayerProps,
  UseSubmitTransformedValuesFactory,
} from './background-layer-helpers'
import { getIndexedUpdateEnabled } from './background-layer-helpers'
import {
  BackgroundSolidOrGradientThumbnailControl,
  StringBackgroundColorControl,
} from '../../../controls/background-solid-or-gradient-thumbnail-control'
import { CheckboxInput, SimplePercentInput } from '../../../../../uuiui'

function getIndexedUpdateStringCSSBackgroundLayerSolidColor(index: number) {
  return function indexedUpdateStringCSSBackgroundLayerSolidColor(
    newValue: string,
    oldValue: CSSBackgroundLayers,
  ): CSSBackgroundLayers {
    const parsedColor = parseColor(newValue, 'hex-hash-optional')
    let newCSSBackgroundLayers = [...oldValue]
    if (isRight(parsedColor)) {
      const oldIndexedValue = newCSSBackgroundLayers[index]
      if (isCSSSolidBackgroundLayer(oldIndexedValue)) {
        newCSSBackgroundLayers[index] = {
          ...oldIndexedValue,
          color: { ...parsedColor.value },
        }
      }
    }
    return newCSSBackgroundLayers
  }
}

function getIndexedUpdateNewAlpha(index: number) {
  return function updateNewAlpha(
    newValue: number | EmptyInputValue,
    oldValue: CSSBackgroundLayers,
  ): CSSBackgroundLayers {
    const oldColor = oldValue[index]
    if (oldColor != null) {
      let newCSSBackgroundLayers = [...oldValue]
      if (oldColor.type === 'solid-background-layer') {
        newCSSBackgroundLayers[index] = {
          ...oldColor,
          color: giveCSSColorNewAlpha(
            fallbackOnEmptyInputValueToCSSEmptyValue(100, newValue),
            oldColor.color,
          ),
        }
      }
      return newCSSBackgroundLayers
    }
    throw new Error(`No background layer exists at index ${index}`)
  }
}

interface SolidBackgroundLayerProps extends BackgroundLayerProps {
  value: CSSSolidBackgroundLayer
  useSubmitTransformedValuesFactory: UseSubmitTransformedValuesFactory
}

export const SolidBackgroundLayer = React.memo<SolidBackgroundLayerProps>((props) => {
  const [onCheckboxSubmitValue] = props.useSubmitTransformedValuesFactory(
    getIndexedUpdateEnabled(props.index),
  )
  const [onAlphaSubmitValue, onAlphaTransientSubmitValue] = props.useSubmitTransformedValuesFactory(
    getIndexedUpdateNewAlpha(props.index),
  )
  const [onStringSubmitValue] = props.useSubmitTransformedValuesFactory(
    getIndexedUpdateStringCSSBackgroundLayerSolidColor(props.index),
  )

  const toggleCheckbox = React.useCallback(
    () => onCheckboxSubmitValue(!props.value.enabled),
    [onCheckboxSubmitValue, props],
  )
  const [onRemoveRowSubmit] = props.useSubmitTransformedValuesFactory(
    getIndexedSpliceArrayItem<CSSBackgroundLayer | CSSUnknownArrayItem>(props.index),
  )

  const alpha = parseAlphaFromCSSColor(props.value.color)
  return (
    <InspectorContextMenuWrapper
      id={`background-layer-subsection-context-menu-row-${props.index}`}
      items={[removeRow(onRemoveRowSubmit), ...props.unsetContextMenuItem]}
      data={null}
    >
      <UIGridRow
        tall
        alignItems='start'
        padded={true}
        variant='<-auto-><-auto->|90px|<----1fr---->|'
      >
        <CheckboxInput
          onChange={toggleCheckbox}
          checked={props.value.enabled}
          controlStatus={props.controlStatus}
          onMouseDown={stopPropagation}
        />
        <BackgroundSolidOrGradientThumbnailControl
          id={`background-layer-gradient-${props.index}`}
          key={`background-layer-gradient-${props.index}`}
          testId={`background-layer-gradient-${props.index}`}
          value={props.value}
          backgroundIndex={props.index}
          useSubmitValueFactory={props.useSubmitTransformedValuesFactory}
          onSubmitSolidStringValue={onStringSubmitValue}
          controlStatus={props.controlStatus}
          controlStyles={props.controlStyles}
          modalOffset={{ x: -45, y: 0 }}
          showString={false}
          popupOpen={props.popupOpen}
          setOpenPopup={props.setOpenPopup}
        />
        <StringBackgroundColorControl
          id={`background-layer-gradient-${props.index}`}
          key={`background-layer-gradient-${props.index}`}
          testId={`background-layer-gradient-${props.index}`}
          value={props.value}
          backgroundIndex={props.index}
          useSubmitValueFactory={props.useSubmitTransformedValuesFactory}
          onSubmitSolidStringValue={onStringSubmitValue}
          controlStatus={props.controlStatus}
          controlStyles={props.controlStyles}
          modalOffset={{ x: -45, y: 0 }}
          showString={true}
          popupOpen={props.popupOpen}
          setOpenPopup={props.setOpenPopup}
        />
        <SimplePercentInput
          id={`background-layer-alpha-${props.index}`}
          testId={`background-layer-alpha-${props.index}`}
          value={alpha}
          onSubmitValue={onAlphaSubmitValue}
          onTransientSubmitValue={onAlphaTransientSubmitValue}
          onForcedSubmitValue={onAlphaSubmitValue}
          controlStatus={props.controlStatus}
          scrubbable_innerlabel={<span style={{ fontSize: 12 }}>α</span>}
          minimum={0}
          maximum={1}
          stepSize={0.01}
          inputProps={{ onMouseDown: stopPropagation }}
          defaultUnitToHide={null}
          incrementControls={false}
        />
      </UIGridRow>
    </InspectorContextMenuWrapper>
  )
})
