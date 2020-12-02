import { Placement } from 'tippy.js'
import * as React from 'react'
import { betterReactMemo } from '../utils/react-performance'
import { getPossiblyHashedURL } from '../utils/hashed-assets'
import { Tooltip } from './tooltip'

export type IcnColor =
  | 'white'
  | 'gray'
  | 'darkgray'
  | 'lightgray'
  | 'black'
  | 'blue'
  | 'purple'
  | 'red'
  | 'orange'

export interface IcnProps {
  category?: string
  type: string
  color?: IcnColor
  width?: number
  height?: number
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
  color: IcnProps['color'] = 'darkgray',
  category = 'semantic',
  width: number = 16,
  height: number = 16,
): string {
  const theme = 'light'
  return getPossiblyHashedURL(
    `/editor/icons/${theme}/${category}/${type}-${color}-${width}x${height}@2x.png`,
  )
}

export const Icn = betterReactMemo(
  'Icn',
  ({
    width = defaultIcnWidth,
    height = defaultIcnHeight,
    isDisabled = false,
    ...props
  }: IcnProps) => {
    const disabledStyle = isDisabled ? { opacity: 0.5 } : undefined

    const { onMouseDown: propsOnMouseDown, onClick } = props
    const onMouseDown = React.useCallback(
      (e: React.MouseEvent<HTMLImageElement>) => {
        if (propsOnMouseDown) {
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
        src={UNSAFE_getIconURL(props.type, props.color, props.category, width, height)}
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

export const IcnSpacer = betterReactMemo(
  'Icn Spacer',
  ({ width = 16, height = 16 }: { width?: number; height?: number }) => {
    return <div style={{ width, height }} />
  },
)
