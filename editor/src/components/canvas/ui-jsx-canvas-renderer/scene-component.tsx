import React from 'react'
import fastDeepEquals from 'fast-deep-equal'
import { SceneProps } from 'utopia-api'
import { useColorTheme, UtopiaStyles } from '../../../uuiui'
import { DomWalkerInvalidatePathsCtxAtom } from '../ui-jsx-canvas'
import { UTOPIA_SCENE_ID_KEY } from '../../../core/model/utopia-constants'
import { AlwaysTrue, usePubSubAtomReadOnly } from '../../../core/shared/atom-with-pub-sub'

type ExtendedSceneProps = SceneProps & { [UTOPIA_SCENE_ID_KEY]: string }

export const SceneComponent = React.memo(
  (props: React.PropsWithChildren<ExtendedSceneProps>) => {
    const colorTheme = useColorTheme()
    const canvasIsLive = false
    const updateInvalidatedPaths = usePubSubAtomReadOnly(
      DomWalkerInvalidatePathsCtxAtom,
      AlwaysTrue,
    )

    const { style, ['data-uid']: dataUid, ...remainingProps } = props
    // We're removing the data-uid prop here as the monkey patch will deal with it

    const sceneStyle: React.CSSProperties = {
      overflow: 'hidden',
      position: 'relative',
      backgroundColor: colorTheme.emphasizedBackground.value,
      boxShadow: canvasIsLive
        ? UtopiaStyles.scene.live.boxShadow
        : UtopiaStyles.scene.editing.boxShadow,
      ...UtopiaStyles.backgrounds.checkerboardBackground,
      ...style,
    }

    // TODO right now we don't actually change the invalidated paths, just let the dom-walker know it should walk again
    updateInvalidatedPaths((current) => current)

    return (
      <div {...remainingProps} style={sceneStyle}>
        {props.children}
      </div>
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
