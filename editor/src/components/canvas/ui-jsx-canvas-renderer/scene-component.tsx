import * as React from 'react'
import { useContextSelector } from 'use-context-selector'
import { Scene, SceneProps } from 'utopia-api'
import { colorTheme, UtopiaStyles } from '../../../uuiui'
import { betterReactMemo } from '../../../uuiui-deps'
import { RerenderUtopiaContext } from './ui-jsx-canvas-contexts'

export const SceneComponent = betterReactMemo(
  'Scene',
  (props: React.PropsWithChildren<SceneProps>) => {
    const canvasIsLive = useContextSelector(RerenderUtopiaContext, (c) => c.canvasIsLive)

    const { style, ...remainingProps } = props

    const sceneStyle: React.CSSProperties = {
      position: 'relative',
      backgroundColor: colorTheme.emphasizedBackground.value,
      boxShadow: canvasIsLive
        ? UtopiaStyles.scene.live.boxShadow
        : UtopiaStyles.scene.editing.boxShadow,
      ...style,
    }

    return (
      <Scene {...remainingProps} style={sceneStyle}>
        {props.children}
      </Scene>
    )
  },
)
