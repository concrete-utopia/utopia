import React from 'react'
import { UtopiaRemixRootComponent } from '../remix/utopia-remix-root-component'
import { UtopiaStyles, useColorTheme } from '../../../uuiui'
import { UTOPIA_PATH_KEY } from '../../../core/model/utopia-constants'
import * as EP from '../../../core/shared/element-path'

export const REMIX_SCENE_TESTID = 'remix-scene'

export interface RemixSceneProps {
  style?: React.CSSProperties
  [UTOPIA_PATH_KEY]?: string
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

  const pathString = props[UTOPIA_PATH_KEY]
  if (pathString == null) {
    throw new Error('Cannot render `UtopiaRemixRootComponent` without a path prop')
  }

  return (
    <div data-testid={REMIX_SCENE_TESTID} {...remainingProps} style={sceneStyle}>
      <UtopiaRemixRootComponent data-path={EP.fromString(pathString)} />
    </div>
  )
})
