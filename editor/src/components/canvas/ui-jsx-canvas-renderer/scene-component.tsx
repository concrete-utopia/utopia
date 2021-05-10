import * as React from 'react'
import * as fastDeepEquals from 'fast-deep-equal'
import { useContextSelector } from '../../../utils/react-performance'
import { Scene, SceneProps } from 'utopia-api'
import { colorTheme, UtopiaStyles } from '../../../uuiui'
import { betterReactMemo } from '../../../uuiui-deps'
import { RerenderUtopiaContext, RerenderUtopiaContextProps } from './ui-jsx-canvas-contexts'

const selectCanvasIsLive = (c: RerenderUtopiaContextProps) => c.canvasIsLive

export const SceneComponent = betterReactMemo(
  'Scene',
  (props: React.PropsWithChildren<SceneProps>) => {
    const canvasIsLive = useContextSelector(RerenderUtopiaContext, selectCanvasIsLive)

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
  (
    prevProps: React.PropsWithChildren<SceneProps>,
    nextProps: React.PropsWithChildren<SceneProps>,
  ) => {
    // Compare child types (to see if a child was actually changed), and compare style
    const prevChildren = React.Children.toArray(prevProps.children)
    const nextChildren = React.Children.toArray(nextProps.children)

    const childrenMatch =
      prevChildren.length === nextChildren.length &&
      prevChildren.every((child, index) => childUnchanged(child, nextChildren[index]))

    return (
      childrenMatch &&
      fastDeepEquals(prevProps.style, nextProps.style) &&
      fastDeepEquals(prevProps['data-label'], nextProps['data-label'])
    )
  },
)

type ReactChild = Exclude<React.ReactNode, boolean | null | undefined>

function childUnchanged(prevChild: ReactChild, nextChild: ReactChild): boolean {
  if (typeof prevChild === 'string' || typeof prevChild === 'number') {
    return nextChild === prevChild
  } else if (React.isValidElement(prevChild)) {
    return React.isValidElement(nextChild) && prevChild.type === nextChild.type
  } else {
    // FIXME Fragments are all that is left
    return false
  }
}
