import * as React from 'react'
import { FlexRow } from '../../uuiui'

interface MiniMenuProps {
  className?: string
  style?: React.CSSProperties
}

interface MiniMenuGroupProps {
  className?: string
  style?: React.CSSProperties
}

interface MiniMenuItemProps {
  className?: string
  animationClassName?: string
  style?: React.CSSProperties
  disabled?: boolean
  onMouseDown?: (event: React.MouseEvent<HTMLDivElement>) => void
  onClick?: (event: React.MouseEvent<HTMLDivElement>) => void
  onDoubleClick?: (event: React.MouseEvent<HTMLDivElement>) => void
  onMouseUp?: (event: React.MouseEvent<HTMLDivElement>) => void
}

interface MiniMenuButtonProps {
  className?: string
  style?: React.CSSProperties
  onMouseDown?: (event: React.MouseEvent<HTMLDivElement>) => void
  onClick?: (event: React.MouseEvent<HTMLDivElement>) => void
  onMouseUp?: (event: React.MouseEvent<HTMLDivElement>) => void
  disabled?: boolean
  selectable?: boolean
  selected?: boolean
  primary?: boolean
  square?: boolean
  height?: number
}

export const MiniMenu: React.StatelessComponent<MiniMenuProps> = (props) => {
  const stopProp = React.useCallback((event: React.MouseEvent) => {
    event.stopPropagation()
  }, [])
  return (
    <FlexRow
      style={{ overflow: 'hidden' }}
      className={'h29 ph4 f10 ' + props.className}
      onMouseDown={stopProp}
    >
      {props.children}
    </FlexRow>
  )
}
MiniMenu.displayName = 'MiniMenu'

export const MiniMenuGroup: React.StatelessComponent<MiniMenuGroupProps> = (props) => {
  return <FlexRow className='mr20'>{props.children}</FlexRow>
}

export const MiniMenuItem: React.StatelessComponent<MiniMenuItemProps> = (props) => {
  const isDisabled = props.disabled == null ? false : props.disabled
  const disabledStyle = isDisabled ? { opacity: 0.5 } : {}

  const propsClassName = props.className == null ? '' : props.className
  const animationClassName = props.animationClassName == null ? '' : props.animationClassName
  const computedClassName = `${propsClassName} ${isDisabled ? '' : animationClassName}`

  return (
    <FlexRow
      className={computedClassName}
      style={{ ...props.style, ...disabledStyle }}
      onClick={isDisabled ? undefined : props.onClick}
      onDoubleClick={isDisabled ? undefined : props.onDoubleClick}
      onMouseDown={isDisabled ? undefined : props.onMouseDown}
      onMouseUp={isDisabled ? undefined : props.onMouseUp}
    >
      {props.children}
    </FlexRow>
  )
}

export const MiniMenuButton: React.StatelessComponent<MiniMenuButtonProps> = (props) => {
  const isDisabled = props.disabled == null ? false : props.disabled

  const primaryStyle = {
    borderRadius: '1',
    padding: '2px',
    fontWeight: 500,
  }
  const disabledStyle = isDisabled ? { opacity: 0.5 } : {}

  return (
    <div
      className={`${isDisabled ? '' : 'darken'} f10 dib`}
      style={{ ...primaryStyle, ...disabledStyle }}
      onClick={isDisabled ? undefined : props.onClick}
      onMouseDown={isDisabled ? undefined : props.onMouseDown}
      onMouseUp={isDisabled ? undefined : props.onMouseUp}
    >
      {props.children}
    </div>
  )
}
