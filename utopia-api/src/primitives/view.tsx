import { Interpolation, Theme } from '@emotion/react'
import * as React from 'react'
import {
  UtopiaComponentProps,
  calculateChildStylesToPrepend,
  calculateOwnStyleProp,
  addEventHandlersToDivProps,
} from './common'

function filterFrameFromStyle(style: React.CSSProperties | undefined): React.CSSProperties {
  if (style == null) {
    return {}
  } else {
    const { top, left, width, height, ...styleWithoutFrame } = style
    return styleWithoutFrame
  }
}

export interface ViewProps
  extends React.DetailedHTMLProps<React.HTMLAttributes<HTMLDivElement>, HTMLDivElement>,
    UtopiaComponentProps {
  css?: Interpolation<Theme>
}

export const View: React.FunctionComponent<ViewProps> = (props: ViewProps) => {
  let { layout: passedLayout, 'data-uid': dataUid, 'data-label': dataLabel, ...divProps } = props

  const childStyles = calculateChildStylesToPrepend(props, props.children)
  const ownStyle = calculateOwnStyleProp(props, props.children)

  const propsWithEventHandlers = addEventHandlersToDivProps(divProps)

  const isGroup: boolean = passedLayout == null ? false : passedLayout.layoutSystem === 'group'
  let styleFromProps = isGroup ? filterFrameFromStyle(divProps.style) : { ...divProps.style }

  return (
    <div
      {...propsWithEventHandlers}
      data-uid={dataUid}
      data-label={dataLabel}
      style={{
        ...ownStyle,
        ...styleFromProps,
      }}
    >
      {React.Children.map(props.children, (child, index) => {
        if (React.isValidElement(child)) {
          const removeLayoutPropFromReactBuiltins =
            typeof child.type === 'string' ? { layout: undefined } : {}

          const layoutEnhancedStyleProp = {
            ...childStyles[index],
            ...child.props.style,
          }
          return React.cloneElement(child as any, {
            ...removeLayoutPropFromReactBuiltins,
            style: layoutEnhancedStyleProp,
          })
        } else {
          return child
        }
      })}
    </div>
  )
}
View.displayName = 'View'
