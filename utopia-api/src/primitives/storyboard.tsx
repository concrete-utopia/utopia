import React from 'react'

export const Storyboard = React.memo((props: React.PropsWithChildren<any>) => {
  return <React.Fragment key='monkey-oh-monkey-please-leave-me-be'>{props.children}</React.Fragment>
})
Storyboard.displayName = 'Storyboard'
