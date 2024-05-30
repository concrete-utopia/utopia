/** @jsxRuntime classic */
/** @jsx jsx */
import { jsx, keyframes } from '@emotion/react'
import React from 'react'
import { NpmDependencyVersionAndStatusIndicator } from './dependecy-version-status-indicator'
import type { ContextMenuItem } from '../context-menu-items'
import { NO_OP } from '../../core/shared/utils'
import {
  useColorTheme,
  FlexRow,
  UtopiaTheme,
  Tooltip,
  Icons,
  AlternateColorThemeComponent,
} from '../../uuiui'
import { MenuProvider, ContextMenu } from '../../uuiui-deps'
import type { DependencyPackageDetails } from '../editor/store/editor-state'
import { unless } from '../../utils/react-conditionals'

interface DependencyListItemProps {
  packageDetails: DependencyPackageDetails
  editingLocked: boolean
  isNewlyLoaded: boolean
  openDependencyEditField: (dependencyName: string, openVersionInput?: boolean) => void
  updateDependencyToLatestVersion: (dependencyName: string) => void
  removeDependency: (key: string) => void
}

export const DependencyListItem: React.FunctionComponent<
  React.PropsWithChildren<DependencyListItemProps>
> = ({
  packageDetails,
  editingLocked,
  isNewlyLoaded,
  openDependencyEditField,
  updateDependencyToLatestVersion,
  removeDependency,
}) => {
  const colorTheme = useColorTheme()
  const ref = React.useRef<HTMLDivElement>(null)

  const LoadingTextColor = colorTheme.subduedForeground.value
  const DefaultTextColor = colorTheme.neutralForeground.value

  const FlashAnimation = React.useMemo(
    () =>
      keyframes({
        from: {
          backgroundColor: colorTheme.listNewItemFlashBackground.value,
          color: colorTheme.subduedForeground.value,
        },
        to: {
          backgroundColor: colorTheme.listNewItemFlashBackground0.value,
          color: colorTheme.neutralForeground.value,
        },
      }),
    [colorTheme],
  )

  const isLoading =
    packageDetails.status === 'loading' || packageDetails.status === 'version-lookup'
  const isDefault = packageDetails.status === 'default-package'
  const hasError = packageDetails.status === 'error'
  const name = packageDetails.name

  const versionFieldNode: React.ReactNode = (
    <NpmDependencyVersionAndStatusIndicator
      status={packageDetails.status}
      version={packageDetails.version}
    />
  )
  let textStyle: Pick<React.CSSProperties, 'color' | 'fontStyle'>

  if (isLoading) {
    textStyle = { color: LoadingTextColor, fontStyle: 'italic' }
  } else if (hasError) {
    textStyle = { color: colorTheme.errorForeground.value, fontStyle: 'italic' }
  } else {
    textStyle = {
      color: DefaultTextColor,
    }
  }

  const onVersionDoubleClick = React.useCallback(
    (e: React.MouseEvent) => {
      if (!isDefault) {
        e.stopPropagation()
        openDependencyEditField(name, true)
      }
    },
    [openDependencyEditField, name, isDefault],
  )

  // list items listen to keyboard events as they have tabIndex={0}
  const onKeyUp = React.useCallback(
    (e: React.KeyboardEvent) => {
      if (e.key == 'Delete') {
        removeDependency(name)
        e.stopPropagation()
      } else if (e.key == 'Enter') {
        openDependencyEditField(name)
        e.stopPropagation()
      }
    },
    [name, openDependencyEditField, removeDependency],
  )

  const menuId = `dependency-list-item-contextmenu-${packageDetails.name}`
  const menuItems: Array<ContextMenuItem<any>> = isDefault
    ? []
    : [
        {
          name: 'Edit',
          enabled: true,
          action: () => {
            openDependencyEditField(name)
          },
        },
        {
          name: 'Update to latest version',
          enabled: true,
          action: () => {
            updateDependencyToLatestVersion(name)
          },
        },
        {
          name: 'Remove',
          enabled: true,
          action: () => {
            removeDependency(name)
          },
        },
      ]

  return (
    <MenuProvider id={menuId} itemsLength={menuItems.length}>
      <FlexRow
        onKeyUp={onKeyUp}
        role='listItem'
        ref={ref}
        key={name}
        tabIndex={0}
        className='dependency-item'
        css={{
          ...textStyle,
          alignItems: 'center',
          height: UtopiaTheme.layout.rowHeight.smaller,
          paddingLeft: 8,
          paddingRight: 8,
          borderRadius: 2,
          '&:focus': {
            background: colorTheme.subtleBackground.value,
            color: colorTheme.emphasizedForeground.value,
          },
          '&:hover': {
            background: colorTheme.subtleBackground.value,
            color: colorTheme.emphasizedForeground.value,
          },
          ...(isNewlyLoaded
            ? {
                animationName: `${FlashAnimation}`,
                animationDuration: '10s',
                animationIterationCount: 1,
              }
            : null),
        }}
        onDoubleClick={onVersionDoubleClick}
      >
        <FlexRow
          css={{
            flexGrow: 1,
            flexShrink: 0,
          }}
          className='packageName-container'
        >
          <span
            style={{
              textDecoration: 'none',
            }}
          >
            {name}
          </span>
          {isDefault ? (
            <Tooltip title='Dependency is required for Utopian projects'>
              <FlexRow
                css={{
                  backgroundColor: colorTheme.fg1.value,
                  boxShadow: `0 0 0 1px ${colorTheme.canvasControlsSizeBoxShadowColor50.value}`,
                  color: colorTheme.bg0.value,
                  borderRadius: 2,
                  height: 14,
                  padding: '0 3px',
                  margin: 0,
                  marginLeft: 6,
                  display: 'none',
                  '.dependency-item:hover &': {
                    display: 'flex',
                  },
                  flexGrow: 0,
                  flexShrink: 0,
                }}
              >
                default
              </FlexRow>
            </Tooltip>
          ) : null}
          <a
            target='_blank'
            rel='noopener noreferrer'
            href={`https://www.npmjs.com/package/${name}`}
            css={{
              display: 'none',
              '.dependency-item:hover &': {
                display: 'block',
              },
            }}
          >
            <Icons.ExternalLinkSmaller />
          </a>
        </FlexRow>
        <FlexRow
          style={{
            flexGrow: 0,
            flexShrink: 0,
          }}
        >
          {unless(
            isDefault,
            <FlexRow
              css={{
                display: 'none',
                '.dependency-item:hover &': {
                  display: 'block',
                },
                cursor: 'pointer',
              }}
              // eslint-disable-next-line react/jsx-no-bind
              onClick={() => removeDependency(name)}
            >
              <Icons.Cross tooltipText={`Remove ${name}`} />
            </FlexRow>,
          )}
          {versionFieldNode}
        </FlexRow>
      </FlexRow>
      <ContextMenu id={menuId} items={menuItems} getData={NO_OP} />
    </MenuProvider>
  )
}
