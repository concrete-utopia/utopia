import createCachedSelector from 're-reselect'
import type { CSSProperties } from 'react'
import React from 'react'
import { safeIndex } from '../../core/shared/array-utils'
import { FlexRow, Icn, Tooltip } from '../../uuiui'
import { treatElementAsGroupLike } from '../canvas/canvas-strategies/strategies/group-helpers'
import { applyCommandsAction } from '../editor/actions/action-creators'
import { useDispatch } from '../editor/store/dispatch-context'
import { Substores, useEditorState, useRefEditorState } from '../editor/store/store-hook'
import type { MetadataSubstate } from '../editor/store/store-hook-substore-types'
import { metadataSelector, selectedViewsSelector } from './inpector-selectors'
import type { FixedHugFillMode } from './inspector-common'
import { setToFixedSizeCommands } from './inspector-common'
import { isFixedHugFillModeApplied } from './inspector-common'
import {
  getFixedFillHugOptionsForElement,
  resizeToFillCommands,
  resizeToFitCommands,
} from './inspector-common'
import * as EP from '../../core/shared/element-path'
import { assertNever } from '../../core/shared/utils'
import type { ElementPath } from '../../core/shared/project-file-types'
import type { ElementInstanceMetadataMap } from '../../core/shared/element-template'
import { MetadataUtils } from '../../core/model/element-metadata-utils'

export const ResizeToFitControlTestId = 'ResizeToFitControlTestId'
export const ResizeToFillControlTestId = 'ResizeToFillControlTestId'
export const ResizeToFixedControlTestId = 'ResizeToFixedControlTestId'

function checkGroupSuitability(
  metadata: ElementInstanceMetadataMap,
  target: ElementPath,
  mode: FixedHugFillMode,
): boolean {
  const parentPath = EP.parentPath(target)
  switch (mode) {
    case 'hug':
    case 'squeeze':
    case 'collapsed':
      // Do not let a group be the target of a resize to fit operation.
      return !treatElementAsGroupLike(metadata, target)
    case 'fill':
      // Neither a group or the child of a group should be eligible for a resize to fill operation.
      return !(
        treatElementAsGroupLike(metadata, target) || treatElementAsGroupLike(metadata, parentPath)
      )
    case 'fixed':
    case 'scaled':
    case 'computed':
    case 'detected':
    case 'hug-group':
      return true
    default:
      assertNever(mode)
  }
}

const isApplicableSelector = createCachedSelector(
  metadataSelector,
  (state) => state.editor.elementPathTree,
  selectedViewsSelector,
  (_: MetadataSubstate, mode: FixedHugFillMode) => mode,
  (metadata, pathTrees, selectedViews, mode) => {
    const firstSelectedView = safeIndex(selectedViews, 0)
    if (firstSelectedView == null || selectedViews.length < 1) {
      return false
    }

    const children = MetadataUtils.getChildrenOrdered(metadata, pathTrees, firstSelectedView)
    const targetsWithOnlyAbsoluteChildren =
      children.length > 0 &&
      children.every(MetadataUtils.isPositionAbsolute) &&
      !treatElementAsGroupLike(metadata, firstSelectedView)

    const isApplicable: boolean =
      targetsWithOnlyAbsoluteChildren ||
      (selectedViews.length > 0 &&
        checkGroupSuitability(metadata, firstSelectedView, mode) &&
        getFixedFillHugOptionsForElement(metadata, pathTrees, firstSelectedView).has(mode))

    const isAlreadyApplied = isFixedHugFillModeApplied(metadata, firstSelectedView, mode)
    return isApplicable && !isAlreadyApplied
  },
)((_, mode) => mode)

interface ResizeToFitControlProps {}

export const ResizeToFitControl = React.memo(() => {
  const dispatch = useDispatch()
  const selectedViewsRef = useRefEditorState((store) => store.editor.selectedViews)
  const elementPathTreeRef = useRefEditorState((store) => store.editor.elementPathTree)
  const allElementPropsRef = useRefEditorState((store) => store.editor.allElementProps)
  const metadataRef = useRefEditorState((store) => store.editor.jsxMetadata)

  const onlyOneElementSelected = useEditorState(
    Substores.selectedViews,
    (store) => store.editor.selectedViews.length === 1,
    'ResizeToFitControl onlyOneElementSelected',
  )

  const isHugApplicable = useEditorState(
    Substores.metadata,
    (store) => onlyOneElementSelected && isApplicableSelector(store, 'hug'),
    'ResizeToFitControl isHugApplicable',
  )

  const disabledStyles = (enabled: boolean): CSSProperties =>
    enabled
      ? { cursor: 'pointer' }
      : {
          cursor: 'pointer',
          opacity: 0.5,
          pointerEvents: 'none',
        }

  const onResizeToFit = React.useCallback(() => {
    if (isHugApplicable) {
      const commands = resizeToFitCommands(
        metadataRef.current,
        selectedViewsRef.current,
        elementPathTreeRef.current,
        allElementPropsRef.current,
      )
      if (commands.length > 0) {
        dispatch([applyCommandsAction(commands)])
      }
    }
  }, [
    isHugApplicable,
    metadataRef,
    selectedViewsRef,
    elementPathTreeRef,
    allElementPropsRef,
    dispatch,
  ])

  return (
    <Tooltip title={'Resize to Fit'}>
      <div
        data-testid={ResizeToFitControlTestId}
        onClick={onResizeToFit}
        style={{ cursor: 'pointer', ...disabledStyles(isHugApplicable) }}
      >
        <Icn type='fitToChildren' color='main' category='layout/commands' width={16} height={16} />
      </div>
    </Tooltip>
  )
})

export const ResizeToFitFillFixedControl = React.memo<ResizeToFitControlProps>(() => {
  const dispatch = useDispatch()
  const selectedViewsRef = useRefEditorState((store) => store.editor.selectedViews)
  const elementPathTreeRef = useRefEditorState((store) => store.editor.elementPathTree)
  const allElementPropsRef = useRefEditorState((store) => store.editor.allElementProps)
  const metadataRef = useRefEditorState((store) => store.editor.jsxMetadata)

  const onlyOneElementSelected = useEditorState(
    Substores.selectedViews,
    (store) => store.editor.selectedViews.length === 1,
    'ResizeToFitControl onlyOneElementSelected',
  )

  const isFillApplicable = useEditorState(
    Substores.metadata,
    (store) => onlyOneElementSelected && isApplicableSelector(store, 'fill'),
    'ResizeToFitControl isHugApplicable',
  )

  const onResizeToFill = React.useCallback(() => {
    if (isFillApplicable) {
      const commands = resizeToFillCommands(metadataRef.current, selectedViewsRef.current)
      if (commands.length > 0) {
        dispatch([applyCommandsAction(commands)])
      }
    }
  }, [dispatch, metadataRef, selectedViewsRef, isFillApplicable])

  const onSetToFixedSize = React.useCallback(() => {
    const commands = setToFixedSizeCommands(
      metadataRef.current,
      elementPathTreeRef.current,
      allElementPropsRef.current,
      selectedViewsRef.current,
    )
    if (commands.length > 0) {
      dispatch([applyCommandsAction(commands)])
    }
  }, [dispatch, metadataRef, elementPathTreeRef, allElementPropsRef, selectedViewsRef])

  const disabledStyles = (enabled: boolean): CSSProperties =>
    enabled
      ? { cursor: 'pointer' }
      : {
          cursor: 'pointer',
          opacity: 0.5,
          pointerEvents: 'none',
        }

  return (
    <FlexRow style={{ gap: 12 }}>
      <ResizeToFitControl />
      <Tooltip title={'Resize to Fill'}>
        <div
          data-testid={ResizeToFillControlTestId}
          onClick={onResizeToFill}
          style={{ cursor: 'pointer', ...disabledStyles(isFillApplicable) }}
        >
          <Icn type='growToParent' color='main' category='layout/commands' width={16} height={16} />
        </div>
      </Tooltip>
      <Tooltip title={'Fixed size'}>
        <div
          data-testid={ResizeToFixedControlTestId}
          onClick={onSetToFixedSize}
          style={{ cursor: 'pointer' }}
        >
          <Icn type='fixed' color='main' category='layout/commands' width={16} height={16} />
        </div>
      </Tooltip>
    </FlexRow>
  )
})
ResizeToFitFillFixedControl.displayName = 'ResizeToFitControl'
