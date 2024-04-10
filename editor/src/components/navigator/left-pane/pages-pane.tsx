import { useAtom } from 'jotai'
import React from 'react'
import { matchRoutes } from 'react-router'
import { uniqBy } from '../../../core/shared/array-utils'
import * as EP from '../../../core/shared/element-path'
import { FlexColumn, FlexRow, UtopiaTheme, colorTheme } from '../../../uuiui'
import { RemixIndexPathLabel } from '../../canvas/remix/remix-utils'
import {
  ActiveRemixSceneAtom,
  RemixNavigationAtom,
} from '../../canvas/remix/utopia-remix-root-component'
import { Substores, useEditorState } from '../../editor/store/store-hook'

type RouteMatch = {
  path: string
  resolvedPath: string
}

export const PagesPane = React.memo((props) => {
  const remixRoutes: Array<RouteMatch> = useEditorState(
    Substores.derived,
    (store) => {
      const result = uniqBy(
        registeredExampleRoutes.flatMap((exampleRoute) => {
          const matchResult = matchRoutes(store.derived.remixData?.routes ?? [], exampleRoute) ?? []

          return matchResult?.map(
            (match): RouteMatch => ({
              resolvedPath: match.pathname,
              path: match.route.path ?? '/',
            }),
          )
        }),
        (l, r) => l?.resolvedPath === r?.resolvedPath,
      )

      return result
    },
    'PagesPane remixRoutes',
  )

  const [navigationControls] = useAtom(RemixNavigationAtom)
  const [activeRemixScene] = useAtom(ActiveRemixSceneAtom)

  const pathname = navigationControls[EP.toString(activeRemixScene)]?.location?.pathname ?? ''

  const matchResult = matchRoutes(remixRoutes, pathname)

  return (
    <FlexColumn style={{ height: '100%', overflowY: 'scroll' }}>
      {remixRoutes.map((route: RouteMatch, index) => {
        const { path, resolvedPath } = route
        const pathMatchesActivePath = matchResult?.[0].route.path === path
        const pathToDisplay = path ?? RemixIndexPathLabel

        return (
          <PageRouteEntry
            key={path}
            routePath={pathToDisplay}
            resolvedPath={resolvedPath}
            active={pathMatchesActivePath}
          />
        )
      })}
    </FlexColumn>
  )
})

interface PageRouteEntryProps {
  routePath: string
  resolvedPath: string
  active: boolean
}
const PageRouteEntry = React.memo<PageRouteEntryProps>((props) => {
  const [navigationControls] = useAtom(RemixNavigationAtom)
  const [activeRemixScene] = useAtom(ActiveRemixSceneAtom)

  const onClick = React.useCallback(() => {
    void navigationControls[EP.toString(activeRemixScene)]?.navigate(props.resolvedPath)
  }, [navigationControls, activeRemixScene, props.resolvedPath])

  return (
    <FlexRow
      style={{
        flexShrink: 0,
        color: colorTheme.neutralForeground.value,
        backgroundColor: props.active ? colorTheme.subtleBackground.value : 'transparent',
        marginLeft: 8,
        marginRight: 8,
        paddingLeft: 3,
        paddingTop: 3,
        paddingBottom: 3,
        height: UtopiaTheme.layout.rowHeight.smaller,
        alignItems: 'center',
        justifyContent: 'space-between',
        borderRadius: 2,
        position: 'relative',
      }}
      onClick={onClick}
    >
      {/* TODO if we want renaming, cannibalize it from FileBrowserItem */}
      <span
        style={{
          flex: 1,
          marginLeft: 6,
          display: 'inline-block',
          whiteSpace: 'nowrap',
          overflow: 'hidden',
          textOverflow: 'ellipsis',
        }}
      >
        {props.routePath === '' ? RemixIndexPathLabel : props.routePath}
      </span>
    </FlexRow>
  )
})

const registeredExampleRoutes = [
  '/',
  '/collections/unisex',
  '/products/beanie',
  '/blogs/news',
  '/blogs/news/making-liquid',
]
