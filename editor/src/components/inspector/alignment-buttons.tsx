/** @jsxRuntime classic */
/** @jsx jsx */
import { jsx } from '@emotion/react'
import React from 'react'
import { MetadataUtils } from '../../core/model/element-metadata-utils'
import { mapDropNulls } from '../../core/shared/array-utils'
import * as EP from '../../core/shared/element-path'
import { zeroRectangle, zeroRectIfNullOrInfinity } from '../../core/shared/math-utils'
import * as PP from '../../core/shared/property-path'
import { assertNever } from '../../core/shared/utils'
import { Button, Icn, Icons, SquareButton } from '../../uuiui'
import type { DropdownMenuItem } from '../../uuiui/radix-components'
import {
  DropdownMenu,
  regularDropdownMenuItem,
  separatorDropdownMenuItem,
} from '../../uuiui/radix-components'
import type { Alignment, Distribution, EditorAction } from '../editor/action-types'
import {
  alignSelectedViews,
  distributeSelectedViews,
  unsetProperty,
} from '../editor/actions/action-creators'
import { useDispatch } from '../editor/store/dispatch-context'
import { Substores, useEditorState } from '../editor/store/store-hook'
import { getControlStyles } from './common/control-styles'
import { OptionChainControl } from './controls/option-chain-control'
import { UIGridRow } from './widgets/ui-grid-row'

type ActiveAlignments = { [key in Alignment]: boolean }

export const AlignmentButtons = React.memo((props: { numberOfTargets: number }) => {
  const dispatch = useDispatch()

  const disableAlign = props.numberOfTargets === 0

  const selectedViews = useEditorState(
    Substores.metadata,
    (store) => store.editor.selectedViews,
    'AlignmentButtons selectedViews',
  )

  const jsxMetadata = useEditorState(
    Substores.metadata,
    (store) => store.editor.jsxMetadata,
    'AlignmentButtons jsxMetadata',
  )

  const activeAlignments: ActiveAlignments = React.useMemo(() => {
    function isActive(alignment: 'left' | 'hcenter' | 'right' | 'top' | 'vcenter' | 'bottom') {
      return (
        selectedViews.length > 0 &&
        selectedViews.every((view) => {
          const isFlexOrGridChild = MetadataUtils.isFlexOrGridChild(jsxMetadata, view)

          const meta = MetadataUtils.findElementByElementPath(jsxMetadata, view)
          if (meta == null) {
            return false
          }

          const { align, justify } = MetadataUtils.getRelativeAlignJustify(jsxMetadata, view)

          const parent = MetadataUtils.findElementByElementPath(jsxMetadata, EP.parentPath(view))
          const parentFrame =
            parent != null ? zeroRectIfNullOrInfinity(parent.globalFrame) : zeroRectangle

          switch (alignment) {
            case 'bottom':
              return isFlexOrGridChild
                ? meta.specialSizeMeasurements[align] === 'flex-end' ||
                    meta.specialSizeMeasurements[align] === 'end'
                : parent != null &&
                    meta.specialSizeMeasurements.offset.y ===
                      parentFrame.height - meta.specialSizeMeasurements.clientHeight
            case 'hcenter':
              return isFlexOrGridChild
                ? meta.specialSizeMeasurements[justify] === 'center'
                : parent != null &&
                    meta.specialSizeMeasurements.offset.x ===
                      Math.ceil(
                        parentFrame.width / 2 - meta.specialSizeMeasurements.clientWidth / 2,
                      )
            case 'left':
              return isFlexOrGridChild
                ? meta.specialSizeMeasurements[justify] === 'flex-start' ||
                    meta.specialSizeMeasurements[justify] === 'start'
                : meta.specialSizeMeasurements.offset.x === 0
            case 'right':
              return isFlexOrGridChild
                ? meta.specialSizeMeasurements[justify] === 'flex-end' ||
                    meta.specialSizeMeasurements[justify] === 'end'
                : parent != null &&
                    meta.specialSizeMeasurements.offset.x ===
                      parentFrame.width - meta.specialSizeMeasurements.clientWidth
            case 'top':
              return isFlexOrGridChild
                ? meta.specialSizeMeasurements[align] === 'flex-start' ||
                    meta.specialSizeMeasurements[align] === 'start'
                : meta.specialSizeMeasurements.offset.y === 0
            case 'vcenter':
              return isFlexOrGridChild
                ? meta.specialSizeMeasurements[align] === 'center'
                : parent != null &&
                    meta.specialSizeMeasurements.offset.y ===
                      Math.ceil(
                        parentFrame.height / 2 - meta.specialSizeMeasurements.clientHeight / 2,
                      )
            default:
              assertNever(alignment)
              return false
          }
        })
      )
    }
    return {
      left: isActive('left'),
      hcenter: isActive('hcenter'),
      right: isActive('right'),
      top: isActive('top'),
      vcenter: isActive('vcenter'),
      bottom: isActive('bottom'),
    }
  }, [jsxMetadata, selectedViews])

  const distributeSelected = React.useCallback(
    (distribution: Distribution) => {
      dispatch([distributeSelectedViews(distribution)], 'everyone')
    },
    [dispatch],
  )

  const disableDistribute = props.numberOfTargets < 3

  const distributeHorizontal = React.useCallback(
    () => distributeSelected('horizontal'),
    [distributeSelected],
  )

  const distributeVertical = React.useCallback(
    () => distributeSelected('vertical'),
    [distributeSelected],
  )

  const dropdownOpener = React.useCallback(() => {
    return (
      <Button highlight>
        <Icons.Threedots color='main' />
      </Button>
    )
  }, [])

  const activeAlignmentsList = React.useMemo(() => {
    return Object.entries(activeAlignments).reduce((acc, [key, set]) => {
      if (set) {
        return acc.concat(key as Alignment)
      }
      return acc
    }, [] as Alignment[])
  }, [activeAlignments])

  const hasActiveAlignments = React.useMemo(() => {
    return activeAlignmentsList.length > 0
  }, [activeAlignmentsList])

  const getUnsetAlignmentsActions = React.useCallback(() => {
    return mapDropNulls((path) => {
      let actions: { alignments: string[]; action: EditorAction }[] = []

      const isFlexOrGridChild = MetadataUtils.isFlexOrGridChild(jsxMetadata, path)
      const { align, justify } = MetadataUtils.getRelativeAlignJustify(jsxMetadata, path)

      if (activeAlignments.left || activeAlignments.hcenter || activeAlignments.right) {
        const alignments = activeAlignmentsList.filter(
          (alignment) => alignment === 'left' || alignment === 'hcenter' || alignment === 'right',
        )
        actions.push({
          action: unsetProperty(path, PP.create('style', isFlexOrGridChild ? justify : 'left')),
          alignments: alignments,
        })
      }

      if (activeAlignments.top || activeAlignments.vcenter || activeAlignments.bottom) {
        const alignments = activeAlignmentsList.filter(
          (alignment) => alignment === 'top' || alignment === 'vcenter' || alignment === 'bottom',
        )
        actions.push({
          action: unsetProperty(path, PP.create('style', isFlexOrGridChild ? align : 'top')),
          alignments: alignments,
        })
      }

      return actions
    }, selectedViews).flat()
  }, [jsxMetadata, activeAlignments, activeAlignmentsList, selectedViews])

  const unsetAllAlignments = React.useCallback(() => {
    dispatch(getUnsetAlignmentsActions().map((a) => a.action))
  }, [dispatch, getUnsetAlignmentsActions])

  const alignSelected = React.useCallback(
    (alignment: Alignment) => {
      if (activeAlignments[alignment]) {
        const unsetAlignment = getUnsetAlignmentsActions()
          .filter((a) => a.alignments.includes(alignment))
          .map((a) => a.action)
        dispatch(unsetAlignment)
      } else {
        dispatch([alignSelectedViews(alignment)], 'everyone')
      }
    },
    [dispatch, activeAlignments, getUnsetAlignmentsActions],
  )

  const dropdownItems = React.useMemo(() => {
    let items: DropdownMenuItem[] = []

    if (hasActiveAlignments) {
      items.push(
        regularDropdownMenuItem({
          id: 'unset',
          label: 'Unset',
          onSelect: unsetAllAlignments,
        }),
        separatorDropdownMenuItem('sep'),
      )
    }

    items.push(
      regularDropdownMenuItem({
        id: 'distribute-horizontal',
        label: 'Distribute Horizontal',
        onSelect: distributeHorizontal,
        disabled: disableDistribute,
        icon: (
          <Icn
            category='inspector'
            type={'distribute-horizontal'}
            width={16}
            height={16}
            color='white'
          />
        ),
      }),
      regularDropdownMenuItem({
        id: 'distribute-vertical',
        label: 'Distribute Vertical',
        onSelect: distributeVertical,
        disabled: disableDistribute,
        icon: (
          <Icn
            category='inspector'
            type={'distribute-vertical'}
            width={16}
            height={16}
            color='white'
          />
        ),
      }),
    )

    return items
  }, [
    disableDistribute,
    distributeHorizontal,
    distributeVertical,
    hasActiveAlignments,
    unsetAllAlignments,
  ])

  const chainValueJustify = React.useMemo(() => {
    return activeAlignments.left
      ? 'left'
      : activeAlignments.hcenter
      ? 'hcenter'
      : activeAlignments.right
      ? 'right'
      : null
  }, [activeAlignments])

  const chainValueAlign = React.useMemo(() => {
    return activeAlignments.top
      ? 'top'
      : activeAlignments.vcenter
      ? 'vcenter'
      : activeAlignments.bottom
      ? 'bottom'
      : null
  }, [activeAlignments])

  return (
    <UIGridRow padded={false} variant='<--1fr--><--1fr-->|22px|'>
      <OptionChainControl
        id='left'
        testId='left'
        key='left'
        onSubmitValue={alignSelected}
        value={chainValueJustify}
        controlStatus='controlled'
        controlStyles={getControlStyles('controlled')}
        options={[
          {
            value: 'left',
            icon: { category: 'inspector', type: 'justifySelf-start' },
            forceCallOnSubmitValue: true,
            disabled: disableAlign,
          },
          {
            value: 'hcenter',
            icon: { category: 'inspector', type: 'justifySelf-center' },
            forceCallOnSubmitValue: true,
            disabled: disableAlign,
          },
          {
            value: 'right',
            icon: { category: 'inspector', type: 'justifySelf-end' },
            forceCallOnSubmitValue: true,
            disabled: disableAlign,
          },
        ]}
      />
      <OptionChainControl
        id='left'
        testId='left'
        key='left'
        onSubmitValue={alignSelected}
        value={chainValueAlign}
        controlStatus='controlled'
        controlStyles={getControlStyles('controlled')}
        options={[
          {
            value: 'top',
            icon: { category: 'inspector', type: 'alignSelf-start' },
            forceCallOnSubmitValue: true,
            disabled: disableAlign,
          },
          {
            value: 'vcenter',
            icon: { category: 'inspector', type: 'alignSelf-center' },
            forceCallOnSubmitValue: true,
            disabled: disableAlign,
          },
          {
            value: 'bottom',
            icon: { category: 'inspector', type: 'alignSelf-end' },
            forceCallOnSubmitValue: true,
            disabled: disableAlign,
          },
        ]}
      />

      <DropdownMenu align='end' items={dropdownItems} opener={dropdownOpener} />
    </UIGridRow>
  )
})
AlignmentButtons.displayName = 'AlignmentButtons'

interface AlignDistributeButtonProps {
  onClick: () => void
  toolTip: string
  iconType: string
  disabled: boolean
  active: boolean
}

const AlignDistributeButton = React.memo<AlignDistributeButtonProps>(
  (props: AlignDistributeButtonProps) => {
    return (
      <SquareButton
        highlight
        disabled={props.disabled}
        onClick={props.onClick}
        spotlight={props.active}
      >
        <Icn
          tooltipText={props.toolTip}
          category='inspector'
          type={props.iconType}
          width={16}
          height={16}
        />
      </SquareButton>
    )
  },
)
AlignDistributeButton.displayName = 'AlignDistributeButton'
