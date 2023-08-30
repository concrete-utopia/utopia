import React from 'react'
import { UtopiaRemixRootComponent } from '../remix/utopia-remix-root-component'
import { UtopiaStyles, useColorTheme } from '../../../uuiui'

export const REMIX_SCENE_TESTID = 'remix-scene'

export interface RemixSceneProps {
  style?: React.CSSProperties
}

export const RemixSceneComponent = React.memo((props: React.PropsWithChildren<RemixSceneProps>) => {
  const colorTheme = useColorTheme()
  const canvasIsLive = false

  const { style, ...remainingProps } = props

  const sceneStyle: React.CSSProperties = {
    position: 'relative',
    backgroundColor: colorTheme.emphasizedBackground.value,
    boxShadow: canvasIsLive
      ? UtopiaStyles.scene.live.boxShadow
      : UtopiaStyles.scene.editing.boxShadow,
    ...UtopiaStyles.backgrounds.checkerboardBackground,
    ...style,
  }

  return (
    <div data-testid={REMIX_SCENE_TESTID} {...remainingProps} style={sceneStyle}>
      <UtopiaRemixRootComponent />
    </div>
  )
})
