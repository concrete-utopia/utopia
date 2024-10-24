/** @jsxRuntime classic */
/** @jsx jsx */
import { jsx } from '@emotion/react'
import { useAtom } from 'jotai'
import React, { useState } from 'react'
import {
  ActiveRemixSceneAtom,
  RemixNavigationAtom,
} from '../canvas/remix/utopia-remix-root-component'
import { Substores, useEditorState } from './store/store-hook'
import { FlexRow, Icn, StringInput, Tooltip, colorTheme } from '../../uuiui'
import { stopPropagation } from '../inspector/common/inspector-utils'
import * as EP from '../../core/shared/element-path'
import { getRemixLocationLabel, getRemixUrlFromLocation } from '../canvas/remix/remix-utils'
import { StarUnstarIcon } from '../canvas/star-unstar-icon'
import { matchPath, matchRoutes } from 'react-router'
import { useDispatch } from './store/dispatch-context'
import { showToast } from './actions/action-creators'
import { notice } from '../common/notice'
import { defaultEither, foldEither } from '../../core/shared/either'
import { getFeaturedRoutesFromPackageJSON } from '../../printer-parsers/html/external-resources-parser'

export const RemixNavigationBarPathTestId = 'remix-navigation-bar-path'

export type RemixSceneLabelButtonType = 'back' | 'forward' | 'home'

export const RemixNavigationBarButtonTestId = (button: RemixSceneLabelButtonType): string =>
  `remix-navigation-bar-button-${button}`

export const RemixNavigationBar = React.memo(() => {
  const [isHovered, setIsHovered] = useState(false)
  const setIsHoveredTrue = React.useCallback(() => {
    setIsHovered(true)
  }, [])

  const setIsHoveredFalse = React.useCallback(() => {
    setIsHovered(false)
  }, [])

  const [navigationControls] = useAtom(RemixNavigationAtom)
  const [activeRemixScene] = useAtom(ActiveRemixSceneAtom)
  const routes = useEditorState(
    Substores.derived,
    (store) =>
      foldEither(
        () => [],
        (remixData) => remixData?.routes ?? [],
        store.derived.remixData,
      ),
    'RemixNavigationBar routes',
  )
  const featuredRoutes = useEditorState(
    Substores.projectContents,
    (store) => {
      return defaultEither([], getFeaturedRoutesFromPackageJSON(store.editor.projectContents))
    },
    'RemixNavigationBar featuredRoutes',
  )

  const dispatch = useDispatch()

  const isSelectOrLiveMode = useEditorState(
    Substores.restOfEditor,
    (store) => store.editor.mode.type === 'select' || store.editor.mode.type === 'live',
    'RemixNavigationBar isLiveMode',
  )

  const forward = React.useCallback(
    () => void navigationControls[EP.toString(activeRemixScene)]?.forward(),
    [activeRemixScene, navigationControls],
  )
  const back = React.useCallback(
    () => void navigationControls[EP.toString(activeRemixScene)]?.back(),
    [activeRemixScene, navigationControls],
  )
  const home = React.useCallback(
    () => void navigationControls[EP.toString(activeRemixScene)]?.home(),
    [activeRemixScene, navigationControls],
  )

  const activeRemixURL = getRemixUrlFromLocation(
    navigationControls[EP.toString(activeRemixScene)]?.location,
  )

  const [currentURL, setCurrentURL] = React.useState<string | null>(activeRemixURL)

  const addedToFavorites = currentURL != null && featuredRoutes.includes(currentURL)

  React.useEffect(() => {
    setCurrentURL(activeRemixURL)
  }, [activeRemixURL])

  const onInputChange = React.useCallback((e: React.ChangeEvent<HTMLInputElement>) => {
    setCurrentURL(e.target.value)
  }, [])

  const changeRemixURLInEditor = React.useCallback(
    (url: string) => {
      try {
        const baseDomain = `domainthatisveryunlikelytoberandomlyused.com`
        const parsedUrl = new URL(url, `http://${baseDomain}`)
        // If the host is the same as `baseDomain`, then that likely means the URL has no domain.
        if (parsedUrl.host == baseDomain) {
          const routeExists = matchRoutes(routes, url) != null
          if (routeExists) {
            // Route exists, so it's fine to navigate to it.
            void navigationControls[EP.toString(activeRemixScene)]?.navigate(url)
          } else {
            // Route does not exist, so show an error.
            dispatch([showToast(notice('Route does not exist.', 'ERROR'))])
          }
        } else {
          // Route has a domain name, indicate that is not currently supported.
          dispatch([
            showToast(
              notice(`You can't edit this yet. Please use a path without a domain name.`, 'ERROR'),
            ),
          ])
        }
      } catch (e) {
        // Path cannot be parsed, so show an error.
        dispatch([showToast(notice('Route does not exist', 'ERROR'))])
      }
    },
    [activeRemixScene, dispatch, navigationControls, routes],
  )

  const resetURL = React.useCallback(() => {
    setCurrentURL(activeRemixURL)
  }, [activeRemixURL])

  if (!isSelectOrLiveMode || navigationControls == null || activeRemixURL == null) {
    return null
  }

  return (
    <FlexRow
      style={{
        gap: 10,
        pointerEvents: 'initial',
        userSelect: 'none',
        padding: '0 8px',
        height: 32,
        width: '100%',
      }}
      onMouseDown={stopPropagation}
      onClick={stopPropagation}
    >
      <Tooltip title={'Back'} placement='bottom'>
        <span
          data-testid={RemixNavigationBarButtonTestId('back')}
          style={{ cursor: 'pointer', fontSize: 10 }}
          css={{
            '&:hover': {
              color: colorTheme.dynamicBlue.value,
            },
          }}
          onClick={back}
        >
          〱
        </span>
      </Tooltip>
      <Tooltip title={'Forward'} placement='bottom'>
        <span
          data-testid={RemixNavigationBarButtonTestId('forward')}
          style={{ cursor: 'pointer', fontSize: 10, transform: 'scale(-1, 1)' }}
          css={{
            '&:hover': {
              color: colorTheme.dynamicBlue.value,
            },
          }}
          onClick={forward}
        >
          〱
        </span>
      </Tooltip>
      <Tooltip title={'Home'} placement='bottom'>
        <div
          onMouseEnter={setIsHoveredTrue}
          onMouseLeave={setIsHoveredFalse}
          onClick={home}
          data-testid={RemixNavigationBarButtonTestId('home')}
          style={{ cursor: 'pointer' }}
        >
          <Icn
            category='semantic'
            type='home'
            width={18}
            height={18}
            color={isHovered ? 'dynamic' : 'main'}
          />
        </div>
      </Tooltip>
      <FlexRow
        data-testid={RemixNavigationBarPathTestId}
        style={{
          backgroundColor: colorTheme.seperator.value,
          borderRadius: 20,
          padding: '2px 2px 2px 6px',
          fontSize: 11,
          minWidth: 150,
          flexGrow: 1,
          height: 20,
          alignItems: 'center',
          justifyContent: 'space-between',
        }}
      >
        <StringInput
          testId={`${RemixNavigationBarPathTestId}-input`}
          value={currentURL ?? ''}
          onChange={onInputChange}
          growInputAutomatically={true}
          includeBoxShadow={false}
          onSubmitValue={changeRemixURLInEditor}
          onEscape={resetURL}
          pasteHandler={true}
        />
        <StarUnstarIcon
          url={currentURL ?? ''}
          selected={false}
          addedToFavorites={addedToFavorites}
          testId={`${RemixNavigationBarPathTestId}-star`}
        />
      </FlexRow>
    </FlexRow>
  )
})
