import React from 'react'
import type { CSSProperties } from 'react'
import type { NavigatorEntry } from '../../../components/editor/store/editor-state'
import { isRegularNavigatorEntry } from '../../../components/editor/store/editor-state'
import { Substores, useEditorState } from '../../editor/store/store-hook'
import { MetadataUtils } from '../../../core/model/element-metadata-utils'
import { colorTheme } from '../../../uuiui'
import * as EP from '../../../core/shared/element-path'
import type { ElementPath } from '../../../core/shared/project-file-types'
import {
  findUtopiaCommentFlag,
  isUtopiaCommentFlagMapCount,
} from '../../../core/shared/comment-flags'
import { isRight } from '../../../core/shared/either'
import { isJSXMapExpression } from '../../../core/shared/element-template'
import { assertNever } from '../../../core/shared/utils'
import type { EditorDispatch } from '../../editor/action-types'
import { setMapCountOverride } from '../../editor/actions/action-creators'

export const MapCounterTestIdPrefix = 'map-counter-'

export function getMapCounterTestId(path: ElementPath): string {
  return `${MapCounterTestIdPrefix}${EP.toString(path)}`
}

type OverrideStatus = 'no-override' | 'overridden' | 'override-failed'
type SelectedStatus = true | false

interface MapCounterProps {
  navigatorEntry: NavigatorEntry
  dispatch: EditorDispatch
  selected: boolean
}

export const MapCounter = React.memo((props: MapCounterProps) => {
  const { navigatorEntry, dispatch } = props
  const { elementPath } = navigatorEntry

  const { nrChildren, countOverride } = useEditorState(
    Substores.metadata,
    (store) => {
      if (!isRegularNavigatorEntry(navigatorEntry)) {
        return {
          nrChildren: null,
          countOverride: null,
        }
      }
      const elementMetadata = MetadataUtils.findElementByElementPath(
        store.editor.jsxMetadata,
        elementPath,
      )

      const element =
        elementMetadata != null && isRight(elementMetadata.element)
          ? elementMetadata.element.value
          : null
      if (element == null || !isJSXMapExpression(element)) {
        return {
          nrChildren: null,
          countOverride: null,
        }
      }
      const commentFlag = findUtopiaCommentFlag(element.comments, 'map-count')
      const mapCountOverride = isUtopiaCommentFlagMapCount(commentFlag) ? commentFlag.value : null
      return {
        nrChildren: MetadataUtils.getChildrenOrdered(
          store.editor.jsxMetadata,
          store.editor.elementPathTree,
          elementPath,
        ).length,
        countOverride: mapCountOverride,
      }
    },
    'MapCounter counterValue',
  )

  const isOverridden = nrChildren != null && countOverride != null
  const shownCounterValue = countOverride ?? nrChildren
  const overrideFailed = isOverridden && countOverride > nrChildren
  const overrideStatus: OverrideStatus = (() => {
    if (isOverridden) {
      if (overrideFailed) {
        return 'override-failed'
      }
      return 'overridden'
    }
    return 'no-override'
  })()

  const onClick = React.useCallback(() => {
    if (nrChildren == null) {
      return
    }
    const nextValue = getNextOverrideValue(overrideStatus, countOverride, nrChildren)
    if (nextValue !== countOverride) {
      dispatch([setMapCountOverride(elementPath, nextValue)])
    }
  }, [elementPath, dispatch, overrideStatus, countOverride, nrChildren])

  if (nrChildren == null) {
    return null
  }

  const selectedStatus = props.selected

  return (
    <div
      data-testid={getMapCounterTestId(elementPath)}
      style={getMapCounterStyleProps(overrideStatus, selectedStatus)}
      onClick={onClick}
    >
      {shownCounterValue}
    </div>
  )
})

function getMapCounterStyleProps(
  overrideStatus: OverrideStatus,
  selectedStatus: SelectedStatus,
): CSSProperties {
  const stylePropsBase: CSSProperties = {
    display: 'flex',
    justifyContent: 'center',
    alignItems: 'center',
    height: 15,
    width: 'max-content',
    minWidth: 15,
    borderRadius: 15,
    padding: 4,
    fontWeight: 400,
    marginRight: 2,
  }

  switch (overrideStatus) {
    case 'no-override':
      return {
        ...stylePropsBase,
        backgroundColor: selectedStatus
          ? colorTheme.whiteOpacity30.value
          : colorTheme.fg0Opacity10.value,
        color: selectedStatus ? colorTheme.white.value : 'unset',
      }
    case 'overridden':
      return {
        ...stylePropsBase,
        backgroundColor: colorTheme.brandNeonPink60.value,
      }
    case 'override-failed':
      return {
        ...stylePropsBase,
        background: `linear-gradient(to left bottom, ${colorTheme.brandNeonPink60.value} 47%, ${colorTheme.brandNeonPink.value} 48%, ${colorTheme.brandNeonPink.value} 52%, ${colorTheme.brandNeonPink60.value} 53%)`,
        boxSizing: 'border-box',
        border: `1px solid ${colorTheme.brandNeonPink60.value}`,
      }
    default:
      assertNever(overrideStatus)
  }
}

function getNextOverrideValue(
  overrideStatus: OverrideStatus,
  countOverride: number | null,
  nrChildren: number,
): number | null {
  const maxOverride = Math.min(2, nrChildren)
  switch (overrideStatus) {
    case 'no-override':
      return maxOverride
    case 'override-failed':
      return null
    case 'overridden':
      if (countOverride === null || countOverride > 2) {
        return maxOverride
      }
      if (countOverride === 2 || countOverride === 1) {
        return Math.min(countOverride - 1, nrChildren)
      }
      return null
    default:
      assertNever(overrideStatus)
  }
}
