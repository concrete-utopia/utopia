import React from 'react'
import type { CSSProperties } from 'react'
import { Substores, useEditorState } from '../../editor/store/store-hook'
import { MetadataUtils } from '../../../core/model/element-metadata-utils'
import * as EP from '../../../core/shared/element-path'
import type { ElementPath } from '../../../core/shared/project-file-types'
import {
  findUtopiaCommentFlag,
  isUtopiaCommentFlagMapCount,
} from '../../../core/shared/comment-flags'
import { isRight } from '../../../core/shared/either'
import { isJSXMapExpression } from '../../../core/shared/element-template'
import { assertNever } from '../../../core/shared/utils'
import { setMapCountOverride } from '../../editor/actions/action-creators'
import { useDispatch } from '../../editor/store/dispatch-context'
import { colorTheme } from '../../../uuiui'

export const MapCounterTestIdPrefix = 'map-counter-'

export function getMapCounterTestId(
  path: ElementPath,
  source: 'navigator' | 'inspector' | 'data-picker',
): string {
  return `${MapCounterTestIdPrefix}${EP.toString(path)}-${source}`
}

type OverrideStatus = 'no-override' | 'overridden' | 'override-failed'
type SelectedStatus = true | false

interface MapCounterProps {
  elementPath: ElementPath
  selected: boolean
  source: 'navigator' | 'inspector' | 'data-picker'
}

export const MapCounter = React.memo((props: MapCounterProps) => {
  const dispatch = useDispatch()

  const counter = useEditorState(
    Substores.metadata,
    (store) => {
      const elementMetadata = MetadataUtils.findElementByElementPath(
        store.editor.jsxMetadata,
        props.elementPath,
      )

      const element =
        elementMetadata != null && isRight(elementMetadata.element)
          ? elementMetadata.element.value
          : null
      if (element == null || !isJSXMapExpression(element)) {
        return null
      }

      const commentFlag = findUtopiaCommentFlag(element.comments, 'map-count')
      const mapCountOverride = isUtopiaCommentFlagMapCount(commentFlag) ? commentFlag.value : null
      return {
        nrChildren: MetadataUtils.getChildrenOrdered(
          store.editor.jsxMetadata,
          store.editor.elementPathTree,
          props.elementPath,
        ).length,
        countOverride: mapCountOverride,
      }
    },
    'MapCounter counterValue',
  )

  const nrChildren = React.useMemo(() => {
    return counter?.nrChildren ?? null
  }, [counter])

  const countOverride = React.useMemo(() => {
    return counter?.countOverride ?? null
  }, [counter])

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
      dispatch([setMapCountOverride(props.elementPath, nextValue)])
    }
  }, [props.elementPath, dispatch, overrideStatus, countOverride, nrChildren])

  const selectedStatus = props.selected

  return (
    <MapCounterUi
      data-testid={getMapCounterTestId(props.elementPath, props.source)}
      counterValue={shownCounterValue}
      overrideStatus={overrideStatus}
      selectedStatus={selectedStatus}
      onClick={onClick}
    />
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
    borderRadius: 4,
    padding: 4,
    fontWeight: 400,
    marginRight: 2,
  }

  switch (overrideStatus) {
    case 'no-override':
      return {
        ...stylePropsBase,
        backgroundColor: selectedStatus ? colorTheme.whiteOpacity30.value : colorTheme.bg1.value,
        color: selectedStatus ? colorTheme.white.value : undefined,
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
interface MapCounterUIProps {
  'data-testid'?: string
  counterValue: number | null
  overrideStatus: OverrideStatus
  selectedStatus: SelectedStatus
  onClick?: React.MouseEventHandler<HTMLDivElement>
}
export const MapCounterUi = React.memo((props: MapCounterUIProps) => {
  const { counterValue, overrideStatus, selectedStatus, onClick } = props

  return (
    <div
      data-testid={props['data-testid']}
      style={getMapCounterStyleProps(overrideStatus, selectedStatus)}
      onClick={onClick}
    >
      {counterValue}
    </div>
  )
})

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
