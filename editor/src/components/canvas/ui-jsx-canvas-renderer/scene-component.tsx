import React from 'react'
import fastDeepEquals from 'fast-deep-equal'
import { Scene, SceneProps } from 'utopia-api'
import { useColorTheme, UtopiaStyles } from '../../../uuiui'
import { DomWalkerInvalidatePathsCtxAtom } from '../ui-jsx-canvas'
import { UTOPIA_SCENE_ID_KEY } from '../../../core/model/utopia-constants'
import { AlwaysTrue, usePubSubAtomReadOnly } from '../../../core/shared/atom-with-pub-sub'

type ExtendedSceneProps = SceneProps & {
  [UTOPIA_SCENE_ID_KEY]: string
  border: boolean
  background: string
}

const backgroundOptions: { [key: string]: any } = {
  White: { backgroundColor: '#ffffff' },
  Checkerboard: UtopiaStyles.backgrounds.checkerboardBackground,
  'Light Grey': { backgroundColor: '#f4f4f4' },
  'Mid Grey': { backgroundColor: '#888888' },
  'Dark Grey': { backgroundColor: '#333' },
  Black: { backgroundColor: '#000' },
}

export const SceneComponent = React.memo(
  (props: React.PropsWithChildren<ExtendedSceneProps>) => {
    const colorTheme = useColorTheme()
    const canvasIsLive = false
    const updateInvalidatedPaths = usePubSubAtomReadOnly(
      DomWalkerInvalidatePathsCtxAtom,
      AlwaysTrue,
    )

    const { style, border, background: backgroundProp, ...remainingProps } = props

    const { top, height, left, width } = style!
    const headerAreaHeight = 21

    const sceneTop: number = (top as number) - headerAreaHeight
    const sceneHeight: number = (height as number) + headerAreaHeight

    const sceneStyle: React.CSSProperties = {
      position: 'absolute',
      top: sceneTop,
      height: sceneHeight,
      left: left,
      width: width,
    }

    const background = backgroundOptions[backgroundProp] ?? {
      backgroundColor: colorTheme.emphasizedBackground.value,
    }

    const innerDivStyle: React.CSSProperties = {
      position: 'absolute',
      boxShadow: canvasIsLive
        ? UtopiaStyles.scene.live.boxShadow
        : UtopiaStyles.scene.editing.boxShadow,
      ...background,
      ...style,
      top: headerAreaHeight,
      left: 0,
      border: border ? `1px solid ${colorTheme.fg9.value}` : undefined,
    }

    // TODO right now we don't actually change the invalidated paths, just let the dom-walker know it should walk again
    updateInvalidatedPaths((current) => current)

    return (
      <Scene {...remainingProps} style={sceneStyle}>
        <div style={innerDivStyle}>{props.children}</div>
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
