import { Placement } from 'tippy.js'
import React from 'react'
import { getPossiblyHashedURL } from '../utils/hashed-assets'
import { Tooltip } from './tooltip'
import { useEditorState } from '../components/editor/store/store-hook'
import { Theme } from '../components/editor/store/editor-state'

export type IcnColor =
  | 'main'
  | 'secondary'
  | 'subdued'
  | 'primary'
  | 'warning'
  | 'error'
  | 'component'
  | 'on-highlight-main'
  | 'on-highlight-secondary'

export type IcnResultingColor =
  | 'white'
  | 'gray'
  | 'darkgray'
  | 'lightgray'
  | 'black'
  | 'blue'
  | 'purple'
  | 'red'
  | 'orange'
  | 'colourful'

function useIconColor(intent: IcnColor): IcnResultingColor {
  const currentTheme: Theme = useEditorState((store) => store.userState.themeConfig, 'currentTheme')
  if (currentTheme === 'light') {
    switch (intent) {
      case 'main':
        return 'black'
      case 'secondary':
        return 'gray'
      case 'subdued':
        return 'lightgray'
      case 'primary':
        return 'blue'
      case 'warning':
        return 'orange'
      case 'error':
        return 'red'
      case 'component':
        return 'purple'
      case 'on-highlight-main':
        return 'white'
      case 'on-highlight-secondary':
        return 'lightgray'
      default:
        return 'white'
    }
  } else if (currentTheme === 'dark') {
    switch (intent) {
      case 'main':
        return 'white'
      case 'secondary':
        return 'lightgray'
      case 'subdued':
        return 'gray'
      case 'primary':
        return 'blue'
      case 'component':
        return 'purple'
      case 'error':
        return 'red'
      case 'warning':
        return 'orange'
      case 'on-highlight-main':
        return 'white'
      case 'on-highlight-secondary':
        return 'lightgray'
      default:
        return 'white'
    }
  }
  return 'black'
}

export interface IcnPropsBase {
  category?: string
  type: string
  width?: number
  height?: number
}

export interface IcnProps extends IcnPropsBase {
  color?: IcnColor
  style?: React.CSSProperties
  className?: string
  isDisabled?: boolean
  tooltipText?: string
  tooltipPlacement?: Placement
  onMouseDown?: (event: React.MouseEvent<HTMLImageElement>) => void
  onClick?: (event: React.MouseEvent<HTMLImageElement>) => void
  onDoubleClick?: (event: React.MouseEvent<HTMLImageElement>) => void
  onMouseUp?: (event: React.MouseEvent<HTMLImageElement>) => void
  onMouseOver?: (event: React.MouseEvent<HTMLImageElement>) => void
  onMouseLeave?: (event: React.MouseEvent<HTMLImageElement>) => void
}

const defaultIcnWidth = 16
const defaultIcnHeight = 16

/**
 *  This should only be used by the below component and where
 *  absolutely needed. Namely: logic that is better handled with
 *  CSS backgroundImage. Otherwise, use `<Icons.NiceIconName />`.
 */
export function UNSAFE_getIconURL(
  type: string,
  // TODO colortheme (icons)
  color: IcnResultingColor = 'black',
  category = 'semantic',
  width: number = 16,
  height: number = 16,
): string {
  const theme = 'light'
  return getPossiblyHashedURL(
    `/editor/icons/${theme}/${category}/${type}-${color}-${width}x${height}@2x.png`,
  )
}

export const Icn = React.memo(
  ({
    width = defaultIcnWidth,
    height = defaultIcnHeight,
    isDisabled = false,
    ...props
  }: IcnProps) => {
    const disabledStyle = isDisabled ? { opacity: 0.5 } : undefined

    const iconColor = useIconColor(props.color || 'main')

    const { onMouseDown: propsOnMouseDown, onClick } = props
    const onMouseDown = React.useCallback(
      (e: React.MouseEvent<HTMLImageElement>) => {
        if (propsOnMouseDown != null) {
          propsOnMouseDown(e)
        }
        if (onClick != null) {
          e.stopPropagation()
        }
      },
      [propsOnMouseDown, onClick],
    )

    const imageElement = (
      <img
        style={{
          userSelect: 'none',
          display: 'block',
          ...props.style,
          ...disabledStyle,
        }}
        className={props.className}
        width={width}
        height={height}
        src={UNSAFE_getIconURL(props.type, iconColor, props.category, width, height)}
        draggable={false}
        onClick={isDisabled ? undefined : props.onClick}
        onDoubleClick={isDisabled ? undefined : props.onDoubleClick}
        onMouseDown={onMouseDown}
        onMouseUp={isDisabled ? undefined : props.onMouseUp}
        onMouseOver={props.onMouseOver}
        onMouseLeave={props.onMouseLeave}
      />
    )
    if (props.tooltipText == null) {
      return imageElement
    } else {
      return (
        <Tooltip title={props.tooltipText} placement={props.tooltipPlacement}>
          {imageElement}
        </Tooltip>
      )
    }
  },
)
Icn.displayName = 'Icon'

export const IcnSpacer = React.memo(
  ({ width = 16, height = 16 }: { width?: number; height?: number }) => {
    return <div style={{ width, height }} />
  },
)
