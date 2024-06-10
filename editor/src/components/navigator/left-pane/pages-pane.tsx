/** @jsxRuntime classic */
/** @jsx jsx */
/** @jsxFrag React.Fragment */
import { jsx } from '@emotion/react'

import { useAtom } from 'jotai'
import React from 'react'
import { matchPath, matchRoutes } from 'react-router'
import { mapFirstApplicable, safeIndex, uniqBy } from '../../../core/shared/array-utils'
import * as EP from '../../../core/shared/element-path'
import {
  NO_OP,
  PortalTargetID,
  arrayEqualsByReference,
  assertNever,
} from '../../../core/shared/utils'
import {
  FlexColumn,
  FlexRow,
  FunctionIcons,
  Icn,
  InspectorSectionHeader,
  SquareButton,
  StringInput,
  Subdued,
  Tooltip,
  UtopiaTheme,
  colorTheme,
} from '../../../uuiui'
import type { PageTemplate } from '../../canvas/remix/remix-utils'
import { RemixIndexPathLabel, getRemixUrlFromLocation } from '../../canvas/remix/remix-utils'
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
import { unless, when } from '../../../utils/react-conditionals'
import { ContextMenu } from '../../context-menu-wrapper'
import { useDispatch } from '../../editor/store/dispatch-context'
import {
  addNewPage,
  showContextMenu,
  showToast,
  updateRemixRoute,
  addNewFeaturedRoute,
  removeFeaturedRoute,
  scrollToPosition,
} from '../../editor/actions/action-creators'
import type { ElementContextMenuInstance } from '../../element-context-menu'
import ReactDOM from 'react-dom'
import { createNewPageName } from '../../editor/store/editor-state'
import urljoin from 'url-join'
import { notice } from '../../common/notice'
import type { EditorDispatch } from '../../editor/action-types'
import { maybeToArray } from '../../../core/shared/optional-utils'
import { StarUnstarIcon } from '../../canvas/star-unstar-icon'
import { canvasRectangle } from '../../../core/shared/math-utils'

type RouteMatch = {
  path: string
  resolvedPath: string | null
  matchesRealRoute: boolean
}

type RouteMatches = {
  [path: string]: RouteMatch
}

type NavigateTo = {
  resolvedPath: string
  routePath: string
  mode: NavigateMode
}

type NavigateMode =
  | 'only-active-scene' // apply navigation only on the active scene
  | 'all-scenes' // apply navigation to all scenes

export const PagesPane = React.memo((props) => {
  const [navigationControls] = useAtom(RemixNavigationAtom)
  const [activeRemixScene] = useAtom(ActiveRemixSceneAtom)

  const activeLocation = navigationControls[EP.toString(activeRemixScene)]?.location
  const pathname = activeLocation?.pathname ?? ''
  const activeRoute = getRemixUrlFromLocation(activeLocation) ?? ''

  const featuredRoutes = useEditorState(
    Substores.projectContents,
    (store) => {
      return defaultEither([], getFeaturedRoutesFromPackageJSON(store.editor.projectContents))
    },
    'PagesPane featuredRoutes',
  )

  const remixRoutesObject: RouteMatches = useEditorState(
    Substores.derived,
    (store) => {
      function processRoutes(routes: Array<any>, prefix: string): RouteMatches {
        let result: RouteMatches = {}
        for (const route of routes) {
          const delimiter = prefix == '' ? '/' : ''
          const path = urljoin(prefix, delimiter, route.path ?? '')
          const firstMatchingFavorite = mapFirstApplicable(
            [...featuredRoutes, activeRoute],
            (favorite) => matchPath(path, favorite)?.pathname ?? null,
          )

          if (path !== '') {
            result[path] = {
              path: path,
              resolvedPath: firstMatchingFavorite,
              matchesRealRoute: true,
            }
          }
          if (route.children != null) {
            result = { ...result, ...processRoutes(route.children, path) }
          }
        }
        return result
      }
      const routes = processRoutes(store.derived.remixData?.routes ?? [], '')
      return routes
    },
    'PagesPane remixRoutes',
  )

  // all remix routes in the project, ordered alphabetically
  const remixRoutes = fillInGapsInRoutes(remixRoutesObject)

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

  const [navigateTo, setNavigateTo] = React.useState<NavigateTo | null>(null)

  const onAfterPageAdd = React.useCallback((pageName: string) => {
    const resolvedPath = urljoin('/', pageName)
    setNavigateTo({
      resolvedPath: resolvedPath,
      routePath: resolvedPath, // it's a straight path anyways
      mode: 'only-active-scene',
    })
  }, [])

  const onAfterRouteRenamed = React.useCallback((routePath: string, resolvedPath: string) => {
    setNavigateTo({
      routePath: urljoin('/', routePath),
      resolvedPath: urljoin('/', resolvedPath),
      mode: 'all-scenes',
    })
  }, [])

  useNavigateToRouteWhenAvailable(remixRoutes, navigateTo, () => {
    setNavigateTo(null)
  })

  const activeRouteDoesntMatchAnyFavorites = !featuredRoutes.includes(activeRoute!)
  const activeRouteTemplatePath = matchRoutes(remixRoutes, pathname)?.[0].route.path

  return (
    <FlexColumn style={{ height: '100%', overflowY: 'scroll' }}>
      <InspectorSectionHeader>
        <FlexRow style={{ flexGrow: 1 }}>Favorites</FlexRow>
      </InspectorSectionHeader>
      <FlexColumn style={{ paddingBottom: 24 }}>
        {featuredRoutes.map((favorite: string) => {
          const pathMatchesActivePath = activeRoute === favorite

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
        const pathMatchesActivePath = path === activeRouteTemplatePath
        const pathToDisplay = path ?? RemixIndexPathLabel

        return (
          <PageRouteEntry
            key={path}
            routePath={pathToDisplay}
            resolvedPath={resolvedPath}
            active={pathMatchesActivePath}
            matchesRealRoute={route.matchesRealRoute}
            navigateTo={navigateTo}
            onAfterRouteRenamed={onAfterRouteRenamed}
          />
        )
      })}
    </FlexColumn>
  )
})

interface PageRouteEntryProps {
  routePath: string
  resolvedPath: string | null
  active: boolean
  matchesRealRoute: boolean
  navigateTo: NavigateTo | null
  onAfterRouteRenamed: (routePath: string, resolvedPath: string) => void
}
const PageRouteEntry = React.memo<PageRouteEntryProps>((props) => {
  const ref = React.useRef<HTMLDivElement | null>(null)

  const [navigationControls] = useAtom(RemixNavigationAtom)
  const [activeRemixScene] = useAtom(ActiveRemixSceneAtom)

  const pathCanBeClickedToNavigate =
    props.matchesRealRoute && !(props.resolvedPath == null && props.routePath.includes(':'))

  const onClick = React.useCallback(() => {
    if (!pathCanBeClickedToNavigate) {
      return
    }
    void navigationControls[EP.toString(activeRemixScene)]?.navigate(
      props.resolvedPath ?? props.routePath,
    )
  }, [
    navigationControls,
    activeRemixScene,
    pathCanBeClickedToNavigate,
    props.resolvedPath,
    props.routePath,
  ])

  const resolvedPathWithoutQueryOrHash = props.resolvedPath?.split('?')[0].split('#')[0]

  const resolvedRouteSegments = resolvedPathWithoutQueryOrHash?.split('/')
  const templateRouteSegments = props.routePath.split('/')

  const lastResolvedSegment = resolvedRouteSegments?.[resolvedRouteSegments.length - 1]
  const lastTemplateSegment = templateRouteSegments[templateRouteSegments.length - 1]

  const indentation = templateRouteSegments.length - 2

  const isDynamicPathSegment = lastTemplateSegment.startsWith(':')

  const renaming = useRenaming(props)

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
        cursor: pathCanBeClickedToNavigate ? 'pointer' : 'auto',
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
      {when(renaming.isRenaming, renaming.InputField)}
      {unless(
        renaming.isRenaming,
        <React.Fragment>
          <span
            onDoubleClick={renaming.startRenaming}
            style={{
              marginLeft: 6,
              display: 'inline-block',
              whiteSpace: 'nowrap',
              overflow: 'hidden',
              textOverflow: 'ellipsis',
              flexGrow: 1,
            }}
          >
            {props.routePath}
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
        </React.Fragment>,
      )}
    </FlexRow>
  )
})

interface FavoriteEntryProps {
  favorite: string
  active: boolean
  addedToFavorites: boolean
}

const FavoriteEntry = React.memo(({ favorite, active, addedToFavorites }: FavoriteEntryProps) => {
  const url = favorite
  const routeWithoutQueryOrHash = url.split('?')[0].split('#')[0]
  // We insert a line break before every param
  const queryAndHashWithLineBreaks = url.slice(routeWithoutQueryOrHash.length).replace(/&/g, '\n&')

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
        fontStyle: !addedToFavorites ? 'italic' : 'normal',
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
        {routeWithoutQueryOrHash}
        {when(
          queryAndHashWithLineBreaks.length > 0,
          <Tooltip
            title={
              <span
                style={{
                  whiteSpace: 'pre-line',
                }}
              >
                {queryAndHashWithLineBreaks}
              </span>
            }
          >
            <span
              style={{
                position: 'relative',
                bottom: 3,
                height: 10,
                display: 'inline-flex',
                flexDirection: 'row',
                alignItems: 'center',
                borderRadius: 2,
                marginLeft: 6,
                paddingLeft: 2,
                paddingRight: 2,
                paddingBottom: 6,
                color: colorTheme.neutralInvertedForeground.value,
                backgroundColor: active
                  ? colorTheme.subduedForeground.value
                  : colorTheme.verySubduedForeground.value,
              }}
            >
              …
            </span>
          </Tooltip>,
        )}
      </span>
      <StarUnstarIcon
        url={favorite}
        selected={active}
        addedToFavorites={addedToFavorites}
        testId='favorite-entry-star'
      />
    </FlexRow>
  )
})

function fillInGapsInRoutes(routes: RouteMatches): Array<RouteMatch> {
  // if we find a route /collections, and a route /collections/hats/beanies, we should create an entry for /collections/hats, so there are no gaps in the tree structure
  let result: RouteMatches = {}
  for (const route of Object.values(routes)) {
    const parts = route.path.split('/')
    for (let i = 1; i < parts.length - 1; i++) {
      const parentRoute = parts.slice(0, i + 1).join('/')
      if (!(parentRoute in result)) {
        result[parentRoute] = {
          path: parentRoute,
          resolvedPath: null,
          matchesRealRoute: false,
        }
      }
    }
    result[route.path] = route
  }

  return Object.values(result).sort((a, b) => a.path.localeCompare(b.path))
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
        dispatch([
          addNewPage('/app/routes', template, newPageName),
          scrollToPosition(
            canvasRectangle({
              // hardcoded values for now, so the content sits at a usable spot
              x: 380,
              y: 120,
              width: 0,
              height: 0,
            }),
            'to-center',
          ),
        ])
        onAfterPageAdd(newPageName)
      },
      [dispatch, onAfterPageAdd],
    )

    const portalTarget = document.getElementById(PortalTargetID)
    if (portalTarget == null) {
      return null
    }

    return ReactDOM.createPortal(
      <ContextMenu
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

function useNavigateToRouteWhenAvailable(
  remixRoutes: RouteMatch[],
  navigateTo: NavigateTo | null,
  onNavigate: () => void,
) {
  const [navigationControls] = useAtom(RemixNavigationAtom)
  const [activeRemixScene] = useAtom(ActiveRemixSceneAtom)

  const scenesToNavigate: string[] = React.useMemo(() => {
    if (navigateTo == null) {
      return []
    }
    switch (navigateTo.mode) {
      case 'all-scenes':
        return Object.keys(navigationControls)
      case 'only-active-scene':
        return [EP.toString(activeRemixScene)]
      default:
        assertNever(navigateTo.mode)
    }
  }, [navigationControls, activeRemixScene, navigateTo])

  React.useEffect(() => {
    if (navigateTo == null) {
      return
    }

    // if the target is a specific route, go to it
    const navigateToMatchesRealRoute =
      matchRoutes(remixRoutes, navigateTo.resolvedPath)?.[0].route.matchesRealRoute ?? false

    // otherwise if the target is not a specific route, go to the first valid match for its prefix
    const navigationTarget = navigateToMatchesRealRoute
      ? navigateTo.resolvedPath
      : remixRoutes.find((r) => r.matchesRealRoute && r.path.startsWith(navigateTo.routePath))
          ?.resolvedPath ?? null

    if (navigationTarget != null) {
      scenesToNavigate.forEach((path) => {
        void navigationControls[path]?.navigate(navigationTarget)
      })
      onNavigate()
    }
  }, [navigateTo, remixRoutes, navigationControls, activeRemixScene, onNavigate, scenesToNavigate])
}

function isReplacementToken(token: string): boolean {
  return token.startsWith(':')
}

function slashPathToRemixPath(path: string): string {
  return path
    .replace(/\//g, '.') // slashes to dots
    .replace(/:/g, '$') // colons to dollars
    .replace(/^\./, '') // chomp first dot
    .trim()
}

// keeping this as a hook so we can reuse it if, for example, we want to add renaming abilities to the favorites section
function useRenaming(props: PageRouteEntryProps) {
  const dispatch = useDispatch()

  const [isRenaming, setIsRenaming] = React.useState(false)

  const startRenaming = React.useCallback(() => {
    const canRename =
      props.routePath !== '/' && (props.resolvedPath != null || !props.routePath.includes(':'))
    if (canRename) {
      setIsRenaming(true)
    }
  }, [props.routePath, props.resolvedPath])

  const onDoneRenaming = React.useCallback(
    (newPath: string | null) => {
      setIsRenaming(false)
      const pathIsDynamicUnresolved = props.resolvedPath == null && props.routePath.includes(':')
      if (pathIsDynamicUnresolved) {
        return
      }
      const newResolvedPath = runRenameRemixRoute(
        dispatch,
        newPath,
        props.routePath,
        props.resolvedPath ?? props.routePath,
      )
      if (newPath != null && newResolvedPath != null) {
        props.onAfterRouteRenamed(newPath, newResolvedPath)
      }
    },
    [props, dispatch],
  )

  return {
    isRenaming: isRenaming,
    InputField: <RenameInputField doneRenaming={onDoneRenaming} routePath={props.routePath} />,
    startRenaming: startRenaming,
  }
}

/**
 * Rename a Remix route, update the related featured entry in the package.json and return the new resolved path.
 */
function runRenameRemixRoute(
  dispatch: EditorDispatch,
  newPath: string | null,
  routePath: string,
  resolvedPath: string,
): string | null {
  if (newPath == null) {
    return null
  }

  // split route and resolved path by `/` so we get every path token
  const routeTokens = routePath.split('/')
  const resolvedTokens = resolvedPath.split('/')

  // keep track of the replacement tokens (:token) and their resolved values
  let replacements: { [key: string]: string } = {}
  for (let i = 0; i < routeTokens.length; i++) {
    const token = routeTokens[i]
    if (isReplacementToken(token)) {
      replacements[token] = resolvedTokens[i]
    }
  }

  // if the renamed value does not match the original replacement tokens, stop here
  const newResolvedPathReplacementTokens = newPath.split('/').filter(isReplacementToken)
  const replacementTokens = routeTokens.filter(isReplacementToken)
  if (!arrayEqualsByReference(replacementTokens, newResolvedPathReplacementTokens)) {
    dispatch([
      showToast(
        notice(
          `Invalid tokens ${Array.from(newResolvedPathReplacementTokens).join(', ')}`,
          'ERROR',
          false,
        ),
      ),
    ])
    return null
  }

  // build the new route path by applying the relevant replacements
  let newResolvedPath = newPath
  for (const [key, value] of Object.entries(replacements)) {
    newResolvedPath = newResolvedPath.replace(key, value)
  }

  // update the remix route
  dispatch(
    [
      updateRemixRoute(
        urljoin('/app/routes', slashPathToRemixPath(routePath)),
        urljoin('/app/routes', slashPathToRemixPath(newPath)),
        resolvedPath,
        urljoin('/', newResolvedPath),
      ),
    ],
    'everyone',
  )

  return newResolvedPath
}

type RenameInputFieldProps = {
  doneRenaming: (newPath: string | null) => void
  routePath: string
}

const RenameInputField = React.memo((props: RenameInputFieldProps) => {
  const [value, setValue] = React.useState(props.routePath)

  const onBlur = React.useCallback(() => {
    props.doneRenaming(null)
  }, [props])

  const onFocus = React.useCallback((e: React.FocusEvent<HTMLInputElement>) => {
    e.target.select()
  }, [])

  const onKeyDown = React.useCallback(
    (e: React.KeyboardEvent) => {
      switch (e.key) {
        case 'Escape':
          props.doneRenaming(null)
          break
        case 'Enter':
          props.doneRenaming(value)
          break
      }
    },
    [props, value],
  )

  const onChange = React.useCallback((e: React.ChangeEvent<HTMLInputElement>) => {
    setValue(e.target.value)
  }, [])

  return (
    <StringInput
      testId='pages-pane-rename-route-input'
      type='text'
      autoFocus={true}
      onFocus={onFocus}
      onBlur={onBlur}
      onKeyDown={onKeyDown}
      value={value}
      onChange={onChange}
    />
  )
})
RenameInputField.displayName = 'RenameInputField'
