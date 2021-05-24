/** @jsx jsx */
import { jsx, keyframes } from '@emotion/react'
import * as React from 'react'
import { NpmDependencyVersionAndStatusIndicator } from './dependecy-version-status-indicator'
import { ContextMenuItem } from '../context-menu-items'
import { NO_OP } from '../../core/shared/utils'
import { colorTheme, FlexRow, UtopiaTheme, Tooltip, Icons } from '../../uuiui'
import { MenuProvider, MomentumContextMenu } from '../../uuiui-deps'
import { handleKeyDown } from '../editor/global-shortcuts'
import type { DependencyPackageDetails } from '../editor/store/editor-state'

interface DependencyListItemProps {
  packageDetails: DependencyPackageDetails
  editingLocked: boolean
  isNewlyLoaded: boolean
  openDependencyEditField: (dependencyName: string, openVersionInput?: boolean) => void
  updateDependencyToLatestVersion: (dependencyName: string) => void
  removeDependency: (key: string) => void
}

const LoadingTextColor = colorTheme.subduedForeground.value
const DefaultTextColor = colorTheme.neutralForeground.value

const FlashAnimation = keyframes({
  from: {
    backgroundColor: colorTheme.listNewItemFlashBackground.o(100).value,
    color: colorTheme.subduedForeground.value,
  },
  to: {
    backgroundColor: colorTheme.listNewItemFlashBackground.o(0).value,
    color: colorTheme.neutralForeground.value,
  },
})

export const DependencyListItem: React.FunctionComponent<DependencyListItemProps> = ({
  packageDetails,
  editingLocked,
  isNewlyLoaded,
  openDependencyEditField,
  updateDependencyToLatestVersion,
  removeDependency,
}) => {
  const ref = React.useRef<HTMLDivElement>(null)

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
    <MenuProvider id={menuId}>
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
          color: colorTheme.secondaryForeground.value,
          '&:focus': {
            background: UtopiaTheme.color.subtleBackground.value,
            color: colorTheme.emphasizedForeground.value,
          },
          '&:hover': {
            background: UtopiaTheme.color.subtleBackground.value,
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
                  backgroundColor: 'rgba(0, 0, 0, 0.05)',
                  boxShadow: '0 0 0 1px rgba(0, 0, 0, 0.05)',
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
              marginLeft: 4,
              marginTop: 4,
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
          {versionFieldNode}
        </FlexRow>
      </FlexRow>
      <MomentumContextMenu id={menuId} items={menuItems} getData={NO_OP} />
    </MenuProvider>
  )
}
