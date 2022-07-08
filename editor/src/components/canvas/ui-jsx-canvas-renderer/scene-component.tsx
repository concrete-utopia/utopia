import React from 'react'
import fastDeepEquals from 'fast-deep-equal'
import { useContextSelector } from 'use-context-selector'
import { Scene, SceneProps } from 'utopia-api'
import { useColorTheme, UtopiaStyles } from '../../../uuiui'
import { RerenderUtopiaCtxAtom } from './ui-jsx-canvas-contexts'
import { DomWalkerInvalidatePathsCtxAtom, UiJsxCanvasCtxAtom } from '../ui-jsx-canvas'
import { UTOPIA_PATH_KEY, UTOPIA_SCENE_ID_KEY } from '../../../core/model/utopia-constants'
import { AlwaysTrue, usePubSubAtomReadOnly } from '../../../core/shared/atom-with-pub-sub'
import { unless, when } from '../../../utils/react-conditionals'
import { checkerboardBackground } from '../../inspector/common/inspector-utils'
import { useEditorState } from '../../editor/store/store-hook'
import { fromString, pathsEqual } from '../../../core/shared/element-path'

type ExtendedSceneProps = SceneProps & { [UTOPIA_SCENE_ID_KEY]: string }

export const SceneComponent = React.memo(
  (props: React.PropsWithChildren<ExtendedSceneProps>) => {
    const colorTheme = useColorTheme()
    const canvasIsLive = usePubSubAtomReadOnly(RerenderUtopiaCtxAtom, AlwaysTrue).canvasIsLive
    const updateInvalidatedPaths = usePubSubAtomReadOnly(
      DomWalkerInvalidatePathsCtxAtom,
      AlwaysTrue,
    )

    const scenePathString = (props as any)[UTOPIA_PATH_KEY]
    const scenePath = scenePathString == null ? null : fromString(scenePathString)
    const isSelected = useEditorState((store) => {
      if (scenePath == null) {
        return false
      } else {
        const selectedViews = store.editor.selectedViews
        return selectedViews.some((p) => pathsEqual(p, scenePath))
      }
    }, 'SceneComponent isSelected')
    const isHighlighted = useEditorState((store) => {
      if (scenePath == null) {
        return false
      } else {
        const highlightedViews = store.editor.highlightedViews
        return highlightedViews.some((p) => pathsEqual(p, scenePath))
      }
    }, 'SceneComponent isSelected')
    const renderHeader = (isSelected || isHighlighted) && !canvasIsLive

    const highlightColor = colorTheme.secondaryBackground.value
    // const highlightColor = colorTheme.neutralBackground.value
    // const highlightColor = colorTheme.canvasSelectionPrimaryOutline.value

    const outlineColor = isSelected ? colorTheme.secondaryBackground.value : highlightColor
    const boxShadow = ` 0px 0px 0px 1.5px ${outlineColor}` //, ${UtopiaStyles.scene.editing.boxShadow}`

    const { style, ...remainingProps } = props

    const sceneStyle: React.CSSProperties = {
      position: 'relative',
      backgroundColor: colorTheme.emphasizedBackground.value,
      boxShadow: canvasIsLive
        ? UtopiaStyles.scene.live.boxShadow
        : UtopiaStyles.scene.editing.boxShadow,
      ...checkerboardBackground,
      ...style,
    }

    // TODO right now we don't actually change the invalidated paths, just let the dom-walker know it should walk again
    updateInvalidatedPaths((current) => current)

    return (
      <React.Fragment>
        {renderHeader ? (
          <div
            style={{
              position: 'absolute',
              width: style!.width as number,
              left: style!.left as number,
              top: (style!.top as number) - 28,
              bottom: -(style!.top as number) + 10,
              boxShadow: boxShadow,
              borderRadius: '3px',
              backgroundColor: isSelected ? colorTheme.secondaryBackground.value : highlightColor,
            }}
          />
        ) : null}
        <Scene {...remainingProps} style={sceneStyle}>
          {props.children}
        </Scene>
      </React.Fragment>
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
