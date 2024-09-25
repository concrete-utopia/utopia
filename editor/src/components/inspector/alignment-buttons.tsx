/** @jsxRuntime classic */
/** @jsx jsx */
import { jsx } from '@emotion/react'
import React from 'react'
import type { Alignment, Distribution, EditorAction } from '../editor/action-types'
import {
  alignSelectedViews,
  distributeSelectedViews,
  unsetProperty,
} from '../editor/actions/action-creators'

import { Button, FlexRow, Icn, Icons, SquareButton, colorTheme } from '../../uuiui'
import { useDispatch } from '../editor/store/dispatch-context'
import { UIGridRow } from './widgets/ui-grid-row'
import { Substores, useEditorState } from '../editor/store/store-hook'
import { MetadataUtils } from '../../core/model/element-metadata-utils'
import * as EP from '../../core/shared/element-path'
import { zeroRectangle, zeroRectIfNullOrInfinity } from '../../core/shared/math-utils'
import type { DropdownMenuItem } from '../../uuiui/radix-components'
import {
  DropdownMenu,
  regularDropdownMenuItem,
  separatorDropdownMenuItem,
} from '../../uuiui/radix-components'
import * as PP from '../../core/shared/property-path'
import { assertNever } from '../../core/shared/utils'

export const AlignmentButtons = React.memo((props: { numberOfTargets: number }) => {
  const dispatch = useDispatch()
  const alignSelected = React.useCallback(
    (alignment: Alignment) => {
      dispatch([alignSelectedViews(alignment)], 'everyone')
    },
    [dispatch],
  )

  const disableAlign = props.numberOfTargets === 0
  const multipleTargets = props.numberOfTargets > 1

  const alignLeft = React.useCallback(() => alignSelected('left'), [alignSelected])
  const alignHCenter = React.useCallback(() => alignSelected('hcenter'), [alignSelected])
  const alignRight = React.useCallback(() => alignSelected('right'), [alignSelected])
  const alignTop = React.useCallback(() => alignSelected('top'), [alignSelected])
  const alignVCenter = React.useCallback(() => alignSelected('vcenter'), [alignSelected])
  const alignBottom = React.useCallback(() => alignSelected('bottom'), [alignSelected])

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

  const activeToggles = React.useMemo(() => {
    function isActive(toggle: 'left' | 'hcenter' | 'right' | 'top' | 'vcenter' | 'bottom') {
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

          switch (toggle) {
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
              assertNever(toggle)
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

  const hasToggles = React.useMemo(() => {
    return (
      activeToggles.left ||
      activeToggles.hcenter ||
      activeToggles.right ||
      activeToggles.top ||
      activeToggles.vcenter ||
      activeToggles.bottom
    )
  }, [activeToggles])

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

  const unsetToggles = React.useCallback(() => {
    let actions: EditorAction[] = []
    for (const view of selectedViews) {
      const isFlexOrGridChild = MetadataUtils.isFlexOrGridChild(jsxMetadata, view)
      const { align, justify } = MetadataUtils.getRelativeAlignJustify(jsxMetadata, view)

      if (activeToggles.left || activeToggles.hcenter || activeToggles.right) {
        actions.push(unsetProperty(view, PP.create('style', isFlexOrGridChild ? justify : 'left')))
      }
      if (activeToggles.top || activeToggles.vcenter || activeToggles.bottom) {
        actions.push(unsetProperty(view, PP.create('style', isFlexOrGridChild ? align : 'top')))
      }
    }

    dispatch(actions)
  }, [selectedViews, dispatch, jsxMetadata, activeToggles])

  const dropdownItems = React.useMemo(() => {
    let items: DropdownMenuItem[] = []

    if (hasToggles) {
      items.push(
        regularDropdownMenuItem({
          id: 'unset',
          label: 'Unset',
          onSelect: unsetToggles,
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
  }, [disableDistribute, distributeHorizontal, distributeVertical, hasToggles, unsetToggles])

  return (
    <UIGridRow padded={false} variant='<--1fr--><--1fr-->|22px|'>
      <FlexRow
        style={{
          alignItems: 'center',
          justifyContent: 'space-between',
          flexShrink: 0,
          borderRadius: 4,
          padding: 1,
          flex: 1,
        }}
        css={{
          border: `1px solid ${colorTheme.border0.value}`,
          ':hover': {
            border: `1px solid ${colorTheme.border3.value}`,
          },
        }}
      >
        <AlignDistributeButton
          onMouseUp={alignLeft}
          toolTip={`Align to left of ${multipleTargets ? 'selection' : 'parent'}`}
          iconType='justifySelf-start'
          disabled={disableAlign}
          active={activeToggles.left}
        />
        <AlignDistributeButton
          onMouseUp={alignHCenter}
          toolTip={`Align to horizontal center of ${multipleTargets ? 'selection' : 'parent'}`}
          iconType='justifySelf-center'
          disabled={disableAlign}
          active={activeToggles.hcenter}
        />
        <AlignDistributeButton
          onMouseUp={alignRight}
          toolTip={`Align to right of ${multipleTargets ? 'selection' : 'parent'}`}
          iconType='justifySelf-end'
          disabled={disableAlign}
          active={activeToggles.right}
        />
      </FlexRow>
      <FlexRow
        style={{
          alignItems: 'center',
          justifyContent: 'space-between',
          flexShrink: 0,
          borderRadius: 4,
          padding: 1,
          flex: 1,
        }}
        css={{
          border: `1px solid ${colorTheme.border0.value}`,
          ':hover': {
            border: `1px solid ${colorTheme.border3.value}`,
          },
        }}
      >
        <AlignDistributeButton
          onMouseUp={alignTop}
          toolTip={`Align to top of ${multipleTargets ? 'selection' : 'parent'}`}
          iconType='alignSelf-start'
          disabled={disableAlign}
          active={activeToggles.top}
        />
        <AlignDistributeButton
          onMouseUp={alignVCenter}
          toolTip={`Align to vertical center of ${multipleTargets ? 'selection' : 'parent'}`}
          iconType='alignSelf-center'
          disabled={disableAlign}
          active={activeToggles.vcenter}
        />
        <AlignDistributeButton
          onMouseUp={alignBottom}
          toolTip={`Align to bottom of ${multipleTargets ? 'selection' : 'parent'}`}
          iconType='alignSelf-end'
          disabled={disableAlign}
          active={activeToggles.bottom}
        />
      </FlexRow>
      <DropdownMenu align='end' items={dropdownItems} opener={dropdownOpener} />
    </UIGridRow>
  )
})
AlignmentButtons.displayName = 'AlignmentButtons'

interface AlignDistributeButtonProps {
  onMouseUp: () => void
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
        onMouseUp={props.onMouseUp}
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
