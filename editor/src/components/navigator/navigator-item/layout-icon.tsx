import React from 'react'
import type { ElementWarnings, NavigatorEntry } from '../../../components/editor/store/editor-state'
import {
  isErrorNavigatorEntry,
  navigatorEntryToKey,
} from '../../../components/editor/store/editor-state'
import type { IcnProps } from '../../../uuiui'
import { colorTheme } from '../../../uuiui'
import { Icn, Icons } from '../../../uuiui'
import { WarningIcon } from '../../../uuiui/warning-icon'
import { invalidGroupStateToString } from '../../canvas/canvas-strategies/strategies/group-helpers'
import { ChildWithPercentageSize } from '../../common/size-warnings'
import { useLayoutOrElementIcon } from '../layout-element-icons'

interface LayoutIconProps {
  navigatorEntry: NavigatorEntry
  color: IcnProps['color']
  warningText?: string | null
  elementWarnings?: ElementWarnings | null
}

export function layoutIconTestIdForEntry(navigatorEntry: NavigatorEntry): string {
  return `layout-icn-${navigatorEntryToKey(navigatorEntry)}`
}

export const LayoutIcon: React.FunctionComponent<React.PropsWithChildren<LayoutIconProps>> =
  React.memo((props) => {
    const { elementWarnings, color, warningText: propsWarningText, navigatorEntry } = props
    const { iconProps, isPositionAbsolute } = useLayoutOrElementIcon(navigatorEntry)

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

    const icon = React.useMemo(() => {
      const defaults = {
        ...iconProps,
        color: color,
        style: { opacity: 'var(--iconOpacity)' },
      }
      if (isErrorNavigatorEntry(navigatorEntry)) {
        return (
          <Icons.WarningTriangle
            color={'overridden'}
            tooltipText={navigatorEntry.message}
            testId={iconTestId}
          />
        )
      } else if (warningText == null) {
        return <Icn {...defaults} testId={iconTestId} />
      } else if (isErroredGroup) {
        return (
          <Icons.GroupProblematic testId={iconTestId} color={color} tooltipText={warningText} />
        )
      } else if (isErroredGroupChild) {
        return <Icn {...defaults} testId={iconTestId} tooltipText={warningText} />
      } else {
        return <WarningIcon tooltipText={warningText} testId={iconTestId} />
      }
    }, [
      isErroredGroup,
      isErroredGroupChild,
      warningText,
      iconProps,
      color,
      iconTestId,
      navigatorEntry,
    ])

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
      } else if (isPositionAbsolute) {
        return (
          <div
            style={{
              color: colorTheme.brandNeonPink.value,
              fontSize: 11,
              fontWeight: 600,
              paddingTop: 3,
            }}
          >
            *
          </div>
        )
      } else {
        return null
      }
    }, [isPositionAbsolute, color, warningText, isErroredGroupChild])

    return (
      <div
        style={{
          width: 18,
          height: 18,
          display: 'flex',
          alignItems: 'center',
          justifyItems: 'center',
          position: 'relative',
          transform: 'scale(.8)',
        }}
      >
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
        </div>
        {icon}
      </div>
    )
  })
