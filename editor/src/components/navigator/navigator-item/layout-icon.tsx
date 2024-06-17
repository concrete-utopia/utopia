import React from 'react'
import type { ElementWarnings, NavigatorEntry } from '../../../components/editor/store/editor-state'
import {
  isInvalidOverrideNavigatorEntry,
  navigatorEntryToKey,
} from '../../../components/editor/store/editor-state'
import type { IcnProps } from '../../../uuiui'
import { Icn, Icons } from '../../../uuiui'
import { invalidGroupStateToString } from '../../canvas/canvas-strategies/strategies/group-helpers'
import { ChildWithPercentageSize } from '../../common/size-warnings'
import { useLayoutOrElementIcon } from '../layout-element-icons'
import { MetadataUtils } from '../../../core/model/element-metadata-utils'
import type { ElementPath } from '../../../core/shared/project-file-types'
import type { ElementInstanceMetadataMap } from '../../../core/shared/element-template'
import { isInfinityRectangle } from '../../../core/shared/math-utils'
import { isZeroSizedElement } from '../../canvas/controls/outline-utils'
import { optionalMap } from '../../../core/shared/optional-utils'
import createCachedSelector from 're-reselect'
import { metadataSelector } from '../../inspector/inpector-selectors'
import type { MetadataSubstate } from '../../editor/store/store-hook-substore-types'
import * as EP from '../../../core/shared/element-path'
import { Substores, useEditorState } from '../../editor/store/store-hook'
import type { Icon } from 'utopia-api'
import { when } from '../../../utils/react-conditionals'

interface LayoutIconProps {
  navigatorEntry: NavigatorEntry
  override?: Icon | null
  color: IcnProps['color']
  warningText?: string | null
  elementWarnings?: ElementWarnings | null
}

export function layoutIconTestIdForEntry(navigatorEntry: NavigatorEntry): string {
  return `layout-icn-${navigatorEntryToKey(navigatorEntry)}`
}

export function isZeroSizedDiv(elementPath: ElementPath, metadata: ElementInstanceMetadataMap) {
  const bounds = MetadataUtils.getFrameInCanvasCoords(elementPath, metadata)
  if (bounds == null || isInfinityRectangle(bounds)) {
    return false
  }

  const isElementDiv =
    optionalMap(
      (i) => MetadataUtils.isDiv(i),
      MetadataUtils.findElementByElementPath(metadata, elementPath),
    ) ?? false

  return isZeroSizedElement(bounds) && isElementDiv
}

const isZeroSizedDivSelector = createCachedSelector(
  metadataSelector,
  (_: MetadataSubstate, x: ElementPath) => x,
  (metadata, elementPath) => {
    return isZeroSizedDiv(elementPath, metadata)
  },
)((_, x) => EP.toString(x))

export const LayoutIcon: React.FunctionComponent<React.PropsWithChildren<LayoutIconProps>> =
  React.memo((props) => {
    const {
      elementWarnings,
      color: baseColor,
      warningText: propsWarningText,
      navigatorEntry,
    } = props
    const { iconProps, isPositionAbsolute } = useLayoutOrElementIcon(navigatorEntry)

    const addAbsoluteMarkerToIcon = isPositionAbsolute

    const isZeroSized = useEditorState(
      Substores.metadata,
      (store) => isZeroSizedDivSelector(store, props.navigatorEntry.elementPath),
      'LayoutIcon isZeroSized',
    )

    const warningText = React.useMemo(() => {
      if (elementWarnings == null) {
        return propsWarningText ?? null
      }
      if (elementWarnings.dynamicSceneChildWidthHeightPercentage) {
        return ChildWithPercentageSize
      } else if (elementWarnings.widthOrHeightZero) {
        return 'Missing width or height'
      } else if (elementWarnings.absoluteWithUnpositionedParent) {
        return 'Element is trying to be positioned absolutely with an unconfigured parent. Add absolute or relative position to the parent.'
      } else if (elementWarnings.invalidGroup != null) {
        return invalidGroupStateToString(elementWarnings.invalidGroup)
      } else if (elementWarnings.invalidGroupChild != null) {
        return invalidGroupStateToString(elementWarnings.invalidGroupChild)
      } else {
        return propsWarningText ?? null
      }
    }, [elementWarnings, propsWarningText])

    const isErroredGroup = React.useMemo(
      () => elementWarnings?.invalidGroup != null,
      [elementWarnings],
    )
    const isErroredGroupChild = React.useMemo(
      () => elementWarnings?.invalidGroupChild != null,
      [elementWarnings],
    )

    const iconTestId = React.useMemo(
      () => layoutIconTestIdForEntry(navigatorEntry),
      [navigatorEntry],
    )

    const { color, iconType, transform } = React.useMemo(() => {
      let colorToReturn = baseColor
      let iconTypeToReturn = iconProps.type
      let transformToReturn: string | undefined = undefined
      if (props.override != null) {
        iconTypeToReturn = props.override
        if (baseColor === 'white') {
          colorToReturn = baseColor
        } else if (
          props.override === 'row' ||
          props.override === 'column' ||
          props.override === 'layout' ||
          props.override === 'grid'
        ) {
          colorToReturn = 'primary'
        } else {
          colorToReturn = baseColor
        }
      } else if (isZeroSized) {
        iconTypeToReturn = 'zerosized-div'
        colorToReturn = baseColor
      } else if (isInvalidOverrideNavigatorEntry(navigatorEntry)) {
        iconTypeToReturn = 'warningtriangle'
        colorToReturn = baseColor
      } else if (warningText == null) {
        transformToReturn = addAbsoluteMarkerToIcon ? 'scale(.8)' : undefined
        if (baseColor === 'white') {
          colorToReturn = 'white'
        } else if (baseColor === 'component') {
          colorToReturn = 'component'
        } else if (
          iconProps.type === 'row' ||
          iconProps.type === 'column' ||
          iconProps.type === 'flex-column' ||
          iconProps.type === 'flex-row' ||
          iconProps.type === 'grid'
        ) {
          colorToReturn = 'primary'
        } else {
          colorToReturn = baseColor
        }
      } else if (isErroredGroup) {
        iconTypeToReturn = 'group-problematic'
      } else if (isErroredGroupChild) {
        iconTypeToReturn = iconProps.type
      } else {
        iconTypeToReturn = 'warningtriangle'
      }
      return { color: colorToReturn, iconType: iconTypeToReturn, transform: transformToReturn }
    }, [
      addAbsoluteMarkerToIcon,
      baseColor,
      iconProps.type,
      isErroredGroup,
      isErroredGroupChild,
      isZeroSized,
      navigatorEntry,
      props.override,
      warningText,
    ])

    const icon = React.useMemo(() => {
      return (
        <div
          style={{
            // with the current design, for absolute elements we apply a 4 corner overlay over the icon,
            // so we shrink it to make it visually fit inside the box
            transform: transform,
          }}
        >
          <Icn
            category='navigator-element'
            type={iconType}
            tooltipText={warningText ?? undefined}
            style={{ opacity: 'var(--iconOpacity)' }}
            testId={iconTestId}
            color={color}
            width={12}
            height={12}
          />
        </div>
      )
    }, [transform, iconType, warningText, iconTestId, color])

    const marker = React.useMemo(() => {
      if (warningText != null && isErroredGroupChild) {
        return (
          <Icons.ExclamationMark
            tooltipText={warningText}
            color={color}
            style={{
              transform: 'scale(1.25)',
            }}
          />
        )
      } else if (addAbsoluteMarkerToIcon) {
        return (
          <Icn
            category='navigator-element'
            type='absolute-corners'
            color={color}
            width={12}
            height={12}
            testId={`absolute-marker-for-${iconTestId}`}
            style={{ position: 'relative', left: 11, transform: 'scale(1.1)' }}
          />
        )
      } else {
        return null
      }
    }, [addAbsoluteMarkerToIcon, color, warningText, isErroredGroupChild, iconTestId])

    const listIndex = useEditorState(
      Substores.metadata,
      (store) => {
        const generatedIndex = EP.extractIndexFromIndexedUid(
          EP.toUid(props.navigatorEntry.elementPath),
        )
        if (generatedIndex == null) {
          return null
        }
        const parent = EP.parentPath(props.navigatorEntry.elementPath)
        return MetadataUtils.isJSXMapExpression(parent, store.editor.jsxMetadata)
          ? generatedIndex
          : null
      },
      'NavigatorRowLabel listIndex',
    )

    return (
      <div
        style={{
          width: 12,
          height: 12,
          display: 'flex',
          alignItems: 'center',
          justifyItems: 'center',
          position: 'relative',
        }}
      >
        {when(
          marker != null,
          <div
            style={{
              display: 'flex',
              alignItems: 'center',
              justifyContent: 'center',
              position: 'absolute',
              left: -9,
              height: 18,
              width: 8,
            }}
          >
            {marker}
          </div>,
        )}
        {when(
          listIndex != null,
          <div
            style={{
              position: 'absolute',
              top: 4,
              right: 13,
              fontSize: 8,
              fontWeight: 700,
            }}
          >
            {listIndex}
          </div>,
        )}
        {icon}
      </div>
    )
  })
