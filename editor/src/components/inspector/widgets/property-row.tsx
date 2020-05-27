import * as React from 'react'

export const PropertyRowHeightDefault = 33
export const PropertyRowHeightWithLabel = 45

export const PropertyRow: React.FunctionComponent<{
  className?: string
  style?: React.CSSProperties
}> = (props) => {
  const { style } = props
  const styleProps: React.CSSProperties = React.useMemo(() => {
    return {
      minHeight: PropertyRowHeightDefault,
      padding: '0 12px',
      position: 'relative',
      boxSizing: 'border-box',
      display: 'grid',
      gridTemplateColumns: 'repeat(6, 1fr)',
      gridColumnGap: 12,
      gridRowGap: 8,
      ...style,
    }
  }, [style])
  return (
    <div className={props.className} style={styleProps}>
      {props.children}
    </div>
  )
}
PropertyRow.displayName = 'PropertyRow'
