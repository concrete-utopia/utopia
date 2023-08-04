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

export const MapCounterTestIdPrefix = 'map-counter-'

export function getMapCounterTestId(path: ElementPath): string {
  return `${MapCounterTestIdPrefix}${EP.toString(path)}`
}

type OverrideStatus = 'no-override' | 'overridden' | 'override-failed'

interface MapCounterProps {
  navigatorEntry: NavigatorEntry
}

export const MapCounter = React.memo((props: MapCounterProps) => {
  const { navigatorEntry } = props
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

  if (nrChildren == null) {
    return null
  }

  const isOverridden = countOverride != null
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

  return (
    <div
      data-testid={getMapCounterTestId(elementPath)}
      style={getMapCounterStyleProps(overrideStatus)}
    >
      {shownCounterValue}
    </div>
  )
})

function getMapCounterStyleProps(overrideStatus: OverrideStatus): CSSProperties {
  const stylePropsBase: CSSProperties = {
    display: 'flex',
    justifyContent: 'center',
    height: 22,
    minWidth: 22,
    width: 'max-content',
    padding: 5,
    alignItems: 'center',
    borderRadius: 11,
  }

  switch (overrideStatus) {
    case 'no-override':
      return {
        ...stylePropsBase,
        backgroundColor: colorTheme.dynamicBlue10.value,
      }
    case 'overridden':
      return {
        ...stylePropsBase,
        color: colorTheme.brandNeonPink.value,
        backgroundColor: colorTheme.pinkSubdued.value,
      }
    case 'override-failed':
      return {
        ...stylePropsBase,
        color: colorTheme.brandNeonPink.value,
        background: `linear-gradient(to left bottom, ${colorTheme.pinkSubdued.value} 48%, ${colorTheme.brandNeonPink.value} 48%, ${colorTheme.brandNeonPink.value} 52%, ${colorTheme.pinkSubdued.value} 52%)`,
        boxSizing: 'border-box',
        border: `1px solid ${colorTheme.brandNeonPink.value}`,
      }
    default:
      assertNever(overrideStatus)
  }
}
