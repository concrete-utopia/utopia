import * as React from 'react'
import * as fastDeepEquals from 'fast-deep-equal'
import { useContextSelector } from 'use-context-selector'
import { Scene, SceneProps } from 'utopia-api'
import { colorTheme, UtopiaStyles } from '../../../uuiui'
import { betterReactMemo } from '../../../uuiui-deps'
import { RerenderUtopiaContext } from './ui-jsx-canvas-contexts'
import { DomWalkerInvalidateScenesContext, UiJsxCanvasContext } from '../ui-jsx-canvas'
import { UTOPIA_SCENE_ID_KEY } from '../../../core/model/utopia-constants'
import { fastForEach } from '../../../core/shared/utils'

function useClearMetadataForScene(props: any) {
  const metadataContext = React.useContext(UiJsxCanvasContext)
  const scenePath = props['data-utopia-scene-id']
  if (scenePath != null) {
    fastForEach(Object.keys(metadataContext.current.spyValues.metadata), (path) => {
      if (path.startsWith(scenePath)) {
        delete metadataContext.current.spyValues.metadata[path]
      }
    })
  }
}

type ExtendedSceneProps = SceneProps & { [UTOPIA_SCENE_ID_KEY]: string }

export const SceneComponent = betterReactMemo(
  'Scene',
  (props: React.PropsWithChildren<ExtendedSceneProps>) => {
    const canvasIsLive = useContextSelector(RerenderUtopiaContext, (c) => c.canvasIsLive)
    const updateInvalidatedScenes = React.useContext(DomWalkerInvalidateScenesContext)

    const { style, ...remainingProps } = props

    const sceneStyle: React.CSSProperties = {
      position: 'relative',
      backgroundColor: colorTheme.emphasizedBackground.value,
      boxShadow: canvasIsLive
        ? UtopiaStyles.scene.live.boxShadow
        : UtopiaStyles.scene.editing.boxShadow,
      ...style,
    }

    useClearMetadataForScene(remainingProps)

    // TODO right now we don't actually invalidate the path, just let the dom-walker know it should walk again
    updateInvalidatedScenes((current) => current)

    return (
      <Scene {...remainingProps} style={sceneStyle}>
        {props.children}
      </Scene>
    )
  },
  (
    prevProps: React.PropsWithChildren<ExtendedSceneProps>,
    nextProps: React.PropsWithChildren<ExtendedSceneProps>,
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
    return (
      React.isValidElement(nextChild) &&
      prevChild.type === nextChild.type &&
      fastDeepEquals(prevChild.props, nextChild.props)
    )
  } else {
    // FIXME Fragments are all that is left
    return false
  }
}
