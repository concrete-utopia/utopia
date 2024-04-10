/** @jsxRuntime classic */
/** @jsx jsx */
import { jsx } from '@emotion/react'

import { useAtom } from 'jotai'
import React from 'react'
import { matchRoutes } from 'react-router'
import { uniqBy } from '../../../core/shared/array-utils'
import * as EP from '../../../core/shared/element-path'
import { NO_OP } from '../../../core/shared/utils'
import { FlexColumn, FlexRow, Icn, UtopiaTheme, colorTheme } from '../../../uuiui'
import { RemixIndexPathLabel } from '../../canvas/remix/remix-utils'
import {
  ActiveRemixSceneAtom,
  RemixNavigationAtom,
} from '../../canvas/remix/utopia-remix-root-component'
import { Substores, useEditorState } from '../../editor/store/store-hook'
import { ExpandableIndicator } from '../navigator-item/expandable-indicator'

type RouteMatch = {
  path: string
  resolvedPath: string
  matchesRealRoute: boolean
}

export const PagesPane = React.memo((props) => {
  const remixRoutes: Array<RouteMatch> = useEditorState(
    Substores.derived,
    (store) => {
      const result = uniqBy(
        registeredExampleRoutes.flatMap((exampleRoute): Array<RouteMatch> => {
          const matchResult = matchRoutes(store.derived.remixData?.routes ?? [], exampleRoute) ?? []

          if (
            matchResult[matchResult.length - 1]?.pathname !==
            matchResult[matchResult.length - 1]?.pathnameBase
          ) {
            return [
              {
                path: exampleRoute,
                resolvedPath: exampleRoute,
                matchesRealRoute: false,
              },
            ]
          }

          return matchResult?.map(
            (match): RouteMatch => ({
              resolvedPath: match.pathname,
              path: '/' + (match.route.path ?? ''),
              matchesRealRoute: true,
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
            resolvedPath={pathMatchesActivePath ? matchResult?.[0].pathname : resolvedPath}
            active={pathMatchesActivePath}
            matchesRealRoute={route.matchesRealRoute}
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
  matchesRealRoute: boolean
}
const PageRouteEntry = React.memo<PageRouteEntryProps>((props) => {
  const [navigationControls] = useAtom(RemixNavigationAtom)
  const [activeRemixScene] = useAtom(ActiveRemixSceneAtom)

  const onClick = React.useCallback(() => {
    void navigationControls[EP.toString(activeRemixScene)]?.navigate(props.resolvedPath)
  }, [navigationControls, activeRemixScene, props.resolvedPath])

  const resolvedRouteSegments = props.resolvedPath.split('/')
  const templateRouteSegments = props.routePath.split('/')

  const lastResolvedSegment = resolvedRouteSegments[resolvedRouteSegments.length - 1]
  const lastTemplateSegment = templateRouteSegments[templateRouteSegments.length - 1]

  const indentation = resolvedRouteSegments.length - 2

  const isDynamicPathSegment = lastTemplateSegment.startsWith(':')

  return (
    <FlexRow
      style={{
        flexShrink: 0,
        color: props.matchesRealRoute
          ? colorTheme.neutralForeground.value
          : colorTheme.subduedForeground.value,
        backgroundColor: props.active ? colorTheme.subtleBackground.value : 'transparent',
        marginLeft: 8,
        marginRight: 8,
        paddingLeft: 3 + indentation * 15,
        paddingTop: 3,
        paddingBottom: 3,
        height: UtopiaTheme.layout.rowHeight.smaller,
        alignItems: 'center',
        borderRadius: 2,
        position: 'relative',
      }}
      onClick={props.matchesRealRoute ? onClick : NO_OP}
    >
      <ExpandableIndicator
        key='expandable-indicator'
        visible={true}
        collapsed={false}
        selected={false}
        style={{ transform: 'scale(0.6)', opacity: 'var(--paneHoverOpacity)' }}
        iconColor={'main'}
      />
      <Icn
        style={{
          marginRight: 0,
          transform: 'scale(.85)',
        }}
        category='filetype'
        color={'main'}
        type={props.matchesRealRoute ? 'other' : 'folder-open'}
        width={18}
        height={18}
      />
      {/* TODO if we want renaming, cannibalize it from FileBrowserItem */}
      <span
        style={{
          // color: isDynamicPathSegment
          //   ? colorTheme.primary.value
          //   : colorTheme.neutralForeground.value,
          marginLeft: 6,
          display: 'inline-block',
          whiteSpace: 'nowrap',
          overflow: 'hidden',
          textOverflow: 'ellipsis',
          flexGrow: 1,
        }}
      >
        {lastResolvedSegment === '' ? RemixIndexPathLabel : '/' + lastResolvedSegment}
      </span>
      <span
        style={{
          flexShrink: 0,
          display: !isDynamicPathSegment ? 'none' : props.active ? 'inline-block' : undefined, // I'm sorry
          color: colorTheme.subduedForeground.value,
          marginLeft: 6,
          whiteSpace: 'nowrap',
          overflow: 'hidden',
          textOverflow: 'ellipsis',
          paddingRight: 8,
          paddingLeft: 2,
        }}
        css={{
          display: 'none',
          '*:hover > &': {
            display: 'inline-block',
          },
        }}
      >
        {lastTemplateSegment}
      </span>
    </FlexRow>
  )
})

const registeredExampleRoutes = fillInGapsInRoute([
  '/',
  '/collections/unisex',
  '/products/beanie',
  '/blogs/news',
  '/blogs/news/making-liquid',
])

function fillInGapsInRoute(routes: Array<string>): Array<string> {
  // if we find a route /collections, and a route /collections/hats/beanies, we should create an entry for /collections/hats, so there are no gaps in the tree structure
  const result: Array<string> = []
  for (const route of routes) {
    const parts = route.split('/')
    for (let i = 1; i < parts.length - 1; i++) {
      const parentRoute = parts.slice(0, i + 1).join('/')
      if (!result.includes(parentRoute)) {
        result.push(parentRoute)
      }
    }
    result.push(route)
  }

  return result
}
