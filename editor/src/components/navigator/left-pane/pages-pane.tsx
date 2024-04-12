/** @jsxRuntime classic */
/** @jsx jsx */
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
  addNewPage,
  setShouldNavigateToRemixRoute,
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

  useNavigateToRouteWhenAvailable(remixRoutes)

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
        <FlexRow style={{ flexGrow: 1 }}>Pages</FlexRow>
        {when(
          canAddPage,
          <React.Fragment>
            <SquareButton onClick={onClickAddPage}>
              <FunctionIcons.Add />
            </SquareButton>
            <AddPageContextMenu
              contextMenuInstance={'context-menu-add-page'}
              pageTemplates={pageTemplates}
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
  const ref = React.useRef<HTMLDivElement | null>(null)
  useScrollToNavigateToRoute(ref, props.resolvedPath)

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
        color: colorTheme.neutralForeground.value,
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
        }}
        category='filetype'
        color={'main'}
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
  }: {
    contextMenuInstance: ElementContextMenuInstance
    pageTemplates: PageTemplate[]
  }) => {
    const dispatch = useDispatch()

    const addPageAction = React.useCallback(
      (template: PageTemplate) => () => {
        const newPageName = createNewPageName()
        dispatch([addNewPage('/app/routes', template, newPageName)])
      },
      [dispatch],
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
) {
  const shouldNavigateToRemixRoute = useEditorState(
    Substores.restOfEditor,
    (store) => store.editor.shouldNavigateToRemixRoute,
    'useScrollToNavigateToRoute shouldNavigateToRemixRoute',
  )

  React.useEffect(() => {
    if (shouldNavigateToRemixRoute === path) {
      ref.current?.scrollIntoView()
    }
  }, [shouldNavigateToRemixRoute, ref, path])
}

function useNavigateToRouteWhenAvailable(remixRoutes: RouteMatch[]) {
  const dispatch = useDispatch()

  const [navigationControls] = useAtom(RemixNavigationAtom)
  const [activeRemixScene] = useAtom(ActiveRemixSceneAtom)

  const shouldNavigateToRemixRoute = useEditorState(
    Substores.restOfEditor,
    (store) => store.editor.shouldNavigateToRemixRoute,
    'useNavigateToRouteWhenAvailable shouldNavigateToRemixRoute',
  )

  React.useEffect(() => {
    if (shouldNavigateToRemixRoute == null) {
      return
    }
    if (remixRoutes.some((r) => r.resolvedPath === shouldNavigateToRemixRoute)) {
      void navigationControls[EP.toString(activeRemixScene)]?.navigate(shouldNavigateToRemixRoute)
      dispatch([setShouldNavigateToRemixRoute(null)])
    }
  }, [shouldNavigateToRemixRoute, remixRoutes, navigationControls, activeRemixScene, dispatch])
}
