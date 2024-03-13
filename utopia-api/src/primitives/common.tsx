import type React from 'react'

export interface UtopiaComponentProps {
  'data-uid'?: string
  'data-label'?: string
  style?: React.CSSProperties
}

export function addEventHandlersToDivProps(
  props: React.DetailedHTMLProps<React.HTMLAttributes<HTMLDivElement>, HTMLDivElement>,
): React.DetailedHTMLProps<React.HTMLAttributes<HTMLDivElement>, HTMLDivElement> {
  // Duplicate some handlers to cover ourselves.
  let updatedProps = {
    ...props,
  }
  const touchStart = props.onTouchStart
  if (touchStart != null) {
    updatedProps = {
      ...props,
      onMouseDown: () => touchStart({} as any),
    }
  }
  const touchEnd = props.onTouchEnd
  if (touchEnd != null) {
    updatedProps = {
      ...props,
      onMouseUp: () => touchEnd({} as any),
    }
  }

  return updatedProps
}
