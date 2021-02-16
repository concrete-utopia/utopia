import * as React from 'react'
import * as PP from '../../../core/shared/property-path'
import {
  CSSURLFunctionBackgroundLayer,
  isCSSImageURLBackgroundLayer,
  CSSBackgroundLayers,
} from '../common/css-utils'
import { getControlStyles } from '../common/control-status'
import { GridRow } from '../widgets/grid-row'
import { PropertyLabel } from '../widgets/property-label'
import { BackgroundLayerControlsProps } from '../sections/style-section/background-subsection/background-picker'
import { StringControl } from './string-control'

export function getIndexedUpdateCSSBackgroundLayerURLImageValue(index: number) {
  return function updateCSSBackgroundLayerURLImageValue(
    newValue: string,
    oldValue?: CSSBackgroundLayers,
  ): CSSBackgroundLayers {
    let newBackgroundLayers = oldValue != null ? [...oldValue] : []
    let workingBackgroundLayer = { ...newBackgroundLayers[index] }
    if (isCSSImageURLBackgroundLayer(workingBackgroundLayer)) {
      workingBackgroundLayer.url = newValue
      newBackgroundLayers[index] = workingBackgroundLayer
      return newBackgroundLayers
    } else {
      return newBackgroundLayers
    }
  }
}

interface URLBackgroundLayerMetadataControlsProps extends BackgroundLayerControlsProps {
  value: CSSURLFunctionBackgroundLayer
}

const backgroundImagePropertyPath = [PP.create(['style', 'backgroundImage'])]

export const URLBackgroundLayerMetadataControls: React.FunctionComponent<URLBackgroundLayerMetadataControlsProps> = (
  props,
) => {
  const [onSubmitValue] = props.useSubmitValueFactory(
    getIndexedUpdateCSSBackgroundLayerURLImageValue(props.index),
  )

  return (
    <GridRow padded type='<-auto-><----------1fr--------->'>
      <PropertyLabel target={backgroundImagePropertyPath}>URL</PropertyLabel>
      <StringControl
        id='metadata-editor-modal-background-url-value'
        key='metadata-editor-modal-background-url-value'
        testId='metadata-editor-modal-background-url-value'
        value={props.value.url}
        onSubmitValue={onSubmitValue}
        controlStatus={props.controlStatus}
        controlStyles={getControlStyles(props.controlStatus)}
      />
    </GridRow>
  )
}
