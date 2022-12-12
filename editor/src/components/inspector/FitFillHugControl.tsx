import React from 'react'
import { MetadataUtils } from '../../core/model/element-metadata-utils'
import { stripNulls } from '../../core/shared/array-utils'
import { isStoryboardChild } from '../../core/shared/element-path'
import { ElementInstanceMetadataMap } from '../../core/shared/element-template'
import { optionalMap } from '../../core/shared/optional-utils'
import { ElementPath } from '../../core/shared/project-file-types'
import { assertNever, NO_OP } from '../../core/shared/utils'
import { PopupList } from '../../uuiui'
import { getControlStyles, SelectOption } from '../../uuiui-deps'
import { useEditorState } from '../editor/store/store-hook'
import { metadataSelector, selectedViewsSelector } from './inpector-selectors'

const hugContentsApplicable = (
  metadata: ElementInstanceMetadataMap,
  elementPath: ElementPath,
): boolean => MetadataUtils.getChildrenPaths(metadata, elementPath).length > 0

const fillContainerApplicable = (elementPath: ElementPath): boolean =>
  isStoryboardChild(elementPath)

type FixedHugFill = 'fixed' | 'hug' | 'fill'

interface Option {
  value: 'fixed' | 'hug' | 'fill'
  label: string
}

function selectOption(value: FixedHugFill): SelectOption {
  switch (value) {
    case 'fill':
      return {
        value: 'fill',
        label: 'Fill container',
      }
    case 'fixed':
      return {
        value: 'fixed',
        label: 'Fixed width',
      }
    case 'hug':
      return {
        value: 'hug',
        label: 'Hug contents',
      }
    default:
      assertNever(value)
  }
}

const FillHugFixedControlOptions = ({
  hugAvailable,
  fillAvailable,
}: {
  hugAvailable: boolean
  fillAvailable: boolean
}): Array<SelectOption> =>
  stripNulls([
    selectOption('fixed'),
    hugAvailable ? selectOption('hug') : null,
    fillAvailable ? selectOption('fill') : null,
  ])

function detectFillHugFixedState(
  metadata: ElementInstanceMetadataMap,
  elementPath: ElementPath | null,
): FixedHugFill | null {
  const element = MetadataUtils.findElementByElementPath(metadata, elementPath)
  if (element == null) {
    return null
  }

  return null
}

interface FillHugFixedControlProps {}

export const FillHugFixedControl = React.memo<FillHugFixedControlProps>((props) => {
  // TODO: come up with better memo
  const options = useEditorState((store) => {
    const selectedView = selectedViewsSelector(store).at(0)
    if (selectedView == null) {
      return null
    }
    const metadata = metadataSelector(store)
    return FillHugFixedControlOptions({
      hugAvailable: hugContentsApplicable(metadata, selectedView),
      fillAvailable: fillContainerApplicable(selectedView),
    })
  }, 'FillHugFixedControl options')

  const currentValue = useEditorState(
    (store) =>
      detectFillHugFixedState(
        metadataSelector(store),
        selectedViewsSelector(store).at(0) ?? null,
      ) ?? undefined,
    'FillHugFixedControl currentValue',
  )

  const controlStylesRef = React.useRef(getControlStyles('simple'))

  if (options == null) {
    return null
  }

  return (
    <PopupList
      value={optionalMap(selectOption, currentValue) ?? undefined}
      options={options}
      onSubmitValue={NO_OP}
      controlStyles={controlStylesRef.current}
      containerMode='showBorderOnHover'
    />
  )
})
