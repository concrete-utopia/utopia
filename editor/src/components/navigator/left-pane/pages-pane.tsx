/** @jsxRuntime classic */
/** @jsx jsx */
/** @jsxFrag React.Fragment */
import { jsx } from '@emotion/react'

import { useAtom } from 'jotai'
import React from 'react'
import { matchRoutes } from 'react-router'
import { safeIndex, uniqBy } from '../../../core/shared/array-utils'
import * as EP from '../../../core/shared/element-path'
import { NO_OP, PortalTargetID } from '../../../core/shared/utils'
import {
  FlexColumn,
  FlexRow,
  FunctionIcons,
  Icn,
  InspectorSectionHeader,
  SquareButton,
  Subdued,
  Tooltip,
  UtopiaTheme,
  colorTheme,
} from '../../../uuiui'
import type { PageTemplate } from '../../canvas/remix/remix-utils'
import { RemixIndexPathLabel } from '../../canvas/remix/remix-utils'
import {
  ActiveRemixSceneAtom,
  RemixNavigationAtom,
} from '../../canvas/remix/utopia-remix-root-component'
import { Substores, useEditorState } from '../../editor/store/store-hook'
import { ExpandableIndicator } from '../navigator-item/expandable-indicator'
import {
  getFeaturedRoutesFromPackageJSON,
  getPageTemplatesFromPackageJSON,
} from '../../../printer-parsers/html/external-resources-parser'
import { defaultEither } from '../../../core/shared/either'
import { when } from '../../../utils/react-conditionals'
import { MomentumContextMenu } from '../../context-menu-wrapper'
import { useDispatch } from '../../editor/store/dispatch-context'
import {
  addNewFeaturedRoute,
  addNewPage,
  removeFeaturedRoute,
  showContextMenu,
} from '../../editor/actions/action-creators'
import type { ElementContextMenuInstance } from '../../element-context-menu'
import ReactDOM from 'react-dom'
import { createNewPageName } from '../../editor/store/editor-state'

type RouteMatch = {
  path: string
  resolvedPath: string
  matchesRealRoute: boolean
}

export const PagesPane = React.memo((props) => {
  const featuredRoutes = useEditorState(
    Substores.projectContents,
    (store) => {
      return defaultEither([], getFeaturedRoutesFromPackageJSON(store.editor.projectContents))
    },
    'PagesPane featuredRoutes',
  )
  const registeredExampleRoutes = fillInGapsInRoute(featuredRoutes)

  const remixRoutes: Array<RouteMatch> = useEditorState(
    Substores.derived,
    (store) => {
      const result = uniqBy(
        registeredExampleRoutes.flatMap((exampleRoute): Array<RouteMatch> => {
          const matchResult = matchRoutes(store.derived.remixData?.routes ?? [], exampleRoute) ?? []

          const lastMatchResult = safeIndex(matchResult, matchResult.length - 1)

          if (lastMatchResult?.pathname !== lastMatchResult?.pathnameBase) {
            return [
              {
                path: exampleRoute,
                resolvedPath: exampleRoute,
                matchesRealRoute: false,
              },
            ]
          }

          return matchResult.map(
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

  const pageTemplates = useEditorState(
    Substores.projectContents,
    (store) => {
      return defaultEither([], getPageTemplatesFromPackageJSON(store.editor.projectContents))
    },
    'PagesPane pageTemplates',
  )

  const canAddPage = React.useMemo(() => {
    return pageTemplates.length > 0 // TODO more constraints
  }, [pageTemplates])

  const dispatch = useDispatch()

  const onClickAddPage = React.useCallback(
    (e: React.MouseEvent) => {
      dispatch([showContextMenu('context-menu-add-page', e.nativeEvent)])
    },
    [dispatch],
  )

  const [navigateTo, setNavigateTo] = React.useState<string | null>(null)

  const onAfterPageAdd = React.useCallback((pageName: string) => {
    setNavigateTo(`/${pageName}`)
  }, [])

  useNavigateToRouteWhenAvailable(remixRoutes, navigateTo, () => {
    setNavigateTo(null)
  })

  const activeRoute = matchResult?.[0].pathname ?? ''
  const activeRouteDoesntMatchAnyFavorites = !featuredRoutes.includes(activeRoute)

  if (remixRoutes.length === 0) {
    return (
      <FlexColumn
        style={{
          height: '100%',
          overflowY: 'scroll',
          whiteSpace: 'normal',
          padding: 16,
          paddingTop: 64,
        }}
      >
        <Subdued>
          No featured routes were found in the project. Please add a 'featuredRoutes' field to the
          'utopia' field in the package.json file.
        </Subdued>
      </FlexColumn>
    )
  }
  return (
    <FlexColumn style={{ height: '100%', overflowY: 'scroll' }}>
      <InspectorSectionHeader>
        <FlexRow style={{ flexGrow: 1 }}>Favorites</FlexRow>
      </InspectorSectionHeader>
      <FlexColumn style={{ paddingBottom: 24 }}>
        {featuredRoutes.map((favorite: string) => {
          const pathMatchesActivePath = matchResult?.[0].pathname === favorite

          return (
            <FavoriteEntry
              key={favorite}
              favorite={favorite}
              active={pathMatchesActivePath}
              addedToFavorites={true}
            />
          )
        })}
        {activeRouteDoesntMatchAnyFavorites ? (
          <FavoriteEntry
            key={activeRoute}
            favorite={activeRoute}
            active={true}
            addedToFavorites={false}
          />
        ) : (
          <div style={{ height: UtopiaTheme.layout.rowHeight.smaller }} />
        )}
      </FlexColumn>
      <InspectorSectionHeader>
        <FlexRow style={{ flexGrow: 1 }}>Routes</FlexRow>
        {when(
          canAddPage,
          <React.Fragment>
            <SquareButton onClick={onClickAddPage}>
              <FunctionIcons.Add />
            </SquareButton>
            <AddPageContextMenu
              contextMenuInstance={'context-menu-add-page'}
              pageTemplates={pageTemplates}
              onAfterPageAdd={onAfterPageAdd}
            />
          </React.Fragment>,
        )}
      </InspectorSectionHeader>

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
            navigateTo={navigateTo}
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
  navigateTo: string | null
}
const PageRouteEntry = React.memo<PageRouteEntryProps>((props) => {
  const ref = React.useRef<HTMLDivElement | null>(null)
  useScrollToNavigateToRoute(ref, props.resolvedPath, props.navigateTo)

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
      ref={ref}
      style={{
        flexShrink: 0,
        color: props.active
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
        iconColor={'secondary'}
      />
      <Icn
        style={{
          marginRight: 0,
        }}
        category='filetype'
        color={props.active ? 'main' : 'secondary'}
        type={props.matchesRealRoute ? 'other' : 'folder-open'}
        width={12}
        height={12}
      />
      {/* TODO if we want renaming, cannibalize it from FileBrowserItem */}
      <span
        style={{
          marginLeft: 6,
          display: 'inline-block',
          whiteSpace: 'nowrap',
          overflow: 'hidden',
          textOverflow: 'ellipsis',
          flexGrow: 1,
        }}
      >
        {lastTemplateSegment === '' ? RemixIndexPathLabel : '/' + lastTemplateSegment}
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
        {lastResolvedSegment}
      </span>
    </FlexRow>
  )
})

interface FavoriteEntryProps {
  favorite: string
  active: boolean
  addedToFavorites: boolean
}

const FavoriteEntry = React.memo(({ favorite, active, addedToFavorites }: FavoriteEntryProps) => {
  const [navigationControls] = useAtom(RemixNavigationAtom)
  const [activeRemixScene] = useAtom(ActiveRemixSceneAtom)

  const onClick = React.useCallback(() => {
    void navigationControls[EP.toString(activeRemixScene)]?.navigate(favorite)
  }, [navigationControls, activeRemixScene, favorite])

  return (
    <FlexRow
      style={{
        flexShrink: 0,
        color: active ? colorTheme.neutralForeground.value : colorTheme.subduedForeground.value,
        backgroundColor: active ? colorTheme.subtleBackground.value : 'transparent',
        border: addedToFavorites
          ? '1px solid transparent'
          : '1px dashed ' + colorTheme.border3.value,
        boxSizing: 'border-box',
        marginLeft: 8,
        marginRight: 8,
        paddingLeft: 19, // to visually align the icons with the route entries underneath the favorites section
        paddingTop: 3,
        paddingBottom: 3,
        height: UtopiaTheme.layout.rowHeight.smaller,
        alignItems: 'center',
        borderRadius: 2,
        position: 'relative',
      }}
      onClick={onClick}
    >
      <Icn
        style={{
          marginRight: 0,
        }}
        category='filetype'
        color={active ? 'main' : 'secondary'}
        type={'other'}
        width={12}
        height={12}
      />
      {/* TODO if we want renaming, cannibalize it from FileBrowserItem */}
      <span
        style={{
          marginLeft: 6,
          display: 'inline-block',
          whiteSpace: 'nowrap',
          overflow: 'hidden',
          textOverflow: 'ellipsis',
          flexGrow: 1,
        }}
      >
        {favorite}
      </span>
      <StarUnstarIcon url={favorite} selected={active} addedToFavorites={addedToFavorites} />
    </FlexRow>
  )
})

interface StarUnstarIconProps {
  url: string
  addedToFavorites: boolean
  selected: boolean
}

const StarUnstarIcon = React.memo(({ url, addedToFavorites, selected }: StarUnstarIconProps) => {
  const dispatch = useDispatch()

  const onClickAddOrRemoveFavorite = React.useCallback(
    (e: React.MouseEvent) => {
      if (!addedToFavorites) {
        dispatch([addNewFeaturedRoute(url)])
      } else {
        dispatch([removeFeaturedRoute(url)])
      }
      e.stopPropagation()
    },
    [dispatch, url, addedToFavorites],
  )

  const [mouseOver, setMouseOver] = React.useState(false)
  const onMouseOver = React.useCallback(() => {
    setMouseOver(true)
  }, [])
  const onMouseLeave = React.useCallback(() => {
    setMouseOver(false)
  }, [])

  const type: 'star' | 'starfilled' = (() => {
    if (addedToFavorites) {
      return mouseOver ? 'star' : 'starfilled'
    } else {
      return mouseOver ? 'starfilled' : 'star'
    }
  })()

  return (
    <Tooltip title={addedToFavorites ? 'Remove from Favorites' : 'Add to Favorites'}>
      <Icn
        onMouseOver={onMouseOver}
        onMouseLeave={onMouseLeave}
        onClick={onClickAddOrRemoveFavorite}
        category='navigator-element'
        type={type}
        color={'main'}
        width={12}
        height={12}
        style={{
          flexShrink: 0,
          opacity: selected ? 1 : undefined,
          color: colorTheme.subduedForeground.value,
          marginLeft: 6,
          marginRight: 6,
          whiteSpace: 'nowrap',
          overflow: 'hidden',
          textOverflow: 'ellipsis',
          cursor: 'pointer',
        }}
        css={{
          opacity: 0,
          '*:hover > &': {
            opacity: 1,
          },
        }}
      />
    </Tooltip>
  )
})

function fillInGapsInRoute(routes: Array<string>): Array<string> {
  // if we find a route /collections, and a route /collections/hats/beanies, we should create an entry for /collections/hats, so there are no gaps in the tree structure
  let result: Array<string> = []
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

export const AddPageContextMenu = React.memo(
  ({
    contextMenuInstance,
    pageTemplates,
    onAfterPageAdd,
  }: {
    contextMenuInstance: ElementContextMenuInstance
    pageTemplates: PageTemplate[]
    onAfterPageAdd: (pageName: string) => void
  }) => {
    const dispatch = useDispatch()

    const addPageAction = React.useCallback(
      (template: PageTemplate) => () => {
        const newPageName = createNewPageName()
        dispatch([addNewPage('/app/routes', template, newPageName)])
        onAfterPageAdd(newPageName)
      },
      [dispatch, onAfterPageAdd],
    )

    const portalTarget = document.getElementById(PortalTargetID)
    if (portalTarget == null) {
      return null
    }

    return ReactDOM.createPortal(
      <MomentumContextMenu
        id={contextMenuInstance}
        key='add-page-context-menu'
        items={pageTemplates.map((t) => ({
          name: t.label,
          enabled: true,
          action: addPageAction(t),
        }))}
        dispatch={dispatch}
        getData={NO_OP}
      />,
      portalTarget,
    )
  },
)

function useScrollToNavigateToRoute(
  ref: React.MutableRefObject<HTMLDivElement | null>,
  path: string,
  navigateTo: string | null,
) {
  React.useEffect(() => {
    if (navigateTo === path) {
      ref.current?.scrollIntoView()
    }
  }, [navigateTo, ref, path])
}

function useNavigateToRouteWhenAvailable(
  remixRoutes: RouteMatch[],
  navigateTo: string | null,
  onNavigate: () => void,
) {
  const [navigationControls] = useAtom(RemixNavigationAtom)
  const [activeRemixScene] = useAtom(ActiveRemixSceneAtom)

  React.useEffect(() => {
    if (navigateTo == null) {
      return
    }
    if (remixRoutes.some((r) => r.resolvedPath === navigateTo)) {
      void navigationControls[EP.toString(activeRemixScene)]?.navigate(navigateTo)
      onNavigate()
    }
  }, [navigateTo, remixRoutes, navigationControls, activeRemixScene, onNavigate])
}
