/** @jsx jsx */
import { jsx, keyframes } from '@emotion/react'
import * as React from 'react'
import type { PackageDetails } from './dependency-list'
import { NpmDependencyVersionAndStatusIndicator } from './dependecy-version-status-indicator'
import { ContextMenuItem } from '../context-menu-items'
import { MenuProvider as ReactContexifyMenuProvider } from 'react-contexify'
import { NO_OP } from '../../core/shared/utils'
import { colorTheme, FlexRow, UtopiaTheme, Tooltip, Icons } from '../../uuiui'
import { MomentumContextMenu } from '../../uuiui-deps'

// FIXME: For some reason we've been able to use this incorrectly
// according to the types, but following the types causes the code to fail.
const MenuProvider = ReactContexifyMenuProvider as any

interface DependencyListItemProps {
  packageDetails: PackageDetails
  editingLocked: boolean
  isNewlyLoaded: boolean
  openDependencyEditField: (dependencyName: string, openVersionInput?: boolean) => void
  updateDependencyToLatestVersion: (dependencyName: string) => void
  removeDependency: (key: string) => void
}

const LoadingTextColor = colorTheme.tertiaryForeground.value
const DefaultTextColor = colorTheme.neutralForeground.value

const FlashAnimation = keyframes({
  from: {
    backgroundColor: colorTheme.listNewItemFlashBackground.o(100).value,
    color: colorTheme.tertiaryForeground.value,
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

  const onNameDoubleClick = React.useCallback(
    (e: React.MouseEvent) => {
      if (!isDefault) {
        e.stopPropagation()
        openDependencyEditField(name)
      }
    },
    [openDependencyEditField, name, isDefault],
  )

  const onVersionDoubleClick = React.useCallback(
    (e: React.MouseEvent) => {
      if (!isDefault) {
        e.stopPropagation()
        openDependencyEditField(name, true)
      }
    },
    [openDependencyEditField, name, isDefault],
  )

  const menuId = `dependency-list-item-contextmenu-${packageDetails.name}`
  const menuItems: Array<ContextMenuItem<any>> = isDefault
    ? []
    : [
        {
          name: 'Update to latest version',
          enabled: true,
          action: () => {
            updateDependencyToLatestVersion(name)
          },
        },
        {
          name: 'Rename',
          enabled: true,
          action: () => {
            openDependencyEditField(name)
          },
        },
        {
          name: 'Delete',
          enabled: true,
          action: () => {
            removeDependency(name)
          },
        },
      ]

  return (
    <MenuProvider id={menuId} storeRef={false}>
      <FlexRow
        ref={ref}
        key={name}
        className='dependency-item'
        css={{
          ...textStyle,
          minHeight: UtopiaTheme.layout.rowHeight.medium,
          display: 'flex',
          padding: '0 16px',
          ...(isNewlyLoaded
            ? {
                animationName: `${FlashAnimation}`,
                animationDuration: '10s',
                animationIterationCount: 1,
              }
            : null),
        }}
        onDoubleClick={onNameDoubleClick}
      >
        <FlexRow
          style={{
            flexGrow: 1,
            flexShrink: 0,
          }}
          className='packageName-container'
        >
          <div
            style={{
              textDecoration: 'none',
            }}
          >
            {name}
          </div>
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
        <div
          style={{
            flexGrow: 0,
            flexShrink: 0,
          }}
          onDoubleClick={onVersionDoubleClick}
        >
          {versionFieldNode}
        </div>
      </FlexRow>
      <MomentumContextMenu id={menuId} items={menuItems} getData={NO_OP} />
    </MenuProvider>
  )
}
