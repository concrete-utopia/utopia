/** @jsx jsx */
import { jsx, keyframes } from '@emotion/core'
import * as React from 'react'
import { Button, colorTheme, FlexRow, FunctionIcons, Icons, UtopiaTheme } from 'uuiui'
import { PackageDetails } from './dependency-list'

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
  const version = packageDetails.version
  const name = packageDetails.name

  let versionFieldNode: React.ReactNode
  let textStyle: Pick<React.CSSProperties, 'color' | 'fontStyle'>

  if (isLoading) {
    versionFieldNode = (
      <React.Fragment>
        <img
          src='/editor/animated-icons/animation-loading-14x12@2x.gif'
          width={14}
          height={12}
          style={{ marginRight: 8, position: 'relative', top: 2 }}
        />
        loading…
      </React.Fragment>
    )
    textStyle = { color: LoadingTextColor, fontStyle: 'italic' }
  } else if (hasError) {
    versionFieldNode = 'failed to load'
    textStyle = { color: colorTheme.errorForeground.value, fontStyle: 'italic' }
  } else {
    versionFieldNode = version
    textStyle = {
      color: DefaultTextColor,
    }
  }

  const onNameDoubleClick = React.useCallback(
    (e: React.MouseEvent) => {
      if (isDefault) {
        e.stopPropagation()
        openDependencyEditField(name, true)
      }
    },
    [openDependencyEditField, name, isDefault],
  )

  const onVersionDoubleClick = React.useCallback(
    (e: React.MouseEvent) => {
      if (isDefault) {
        e.stopPropagation()
        openDependencyEditField(name, true)
      }
    },
    [openDependencyEditField, name, isDefault],
  )

  const onUpdateDependencyClick = React.useCallback(() => updateDependencyToLatestVersion(name), [
    updateDependencyToLatestVersion,
    name,
  ])
  const onOpenDependencyClick = React.useCallback(() => openDependencyEditField(name), [
    openDependencyEditField,
    name,
  ])
  const onRemoveDependencyClick = React.useCallback(() => removeDependency(name), [
    removeDependency,
    name,
  ])

  return (
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
            marginTop: 1,
          }}
        >
          <Icons.ExternalLinkSmaller />
        </a>
      </FlexRow>
      {isDefault ? (
        <div
          css={{
            backgroundColor: 'rgba(0, 0, 0, 0.05)',
            boxShadow: '0 0 0 1px rgba(0, 0, 0, 0.05)',
            borderRadius: 2,
            height: 14,
            padding: '0 3px',
            margin: '0 6px',
            display: 'none',
            '.dependency-item:hover &': {
              display: 'flex',
            },
            flexDirection: 'row',
            flexGrow: 0,
            flexShrink: 0,
          }}
        >
          default
        </div>
      ) : (
        <div
          css={{
            display: 'none',
            '.dependency-item:hover &': {
              display: 'flex',
            },
            flexDirection: 'row',
            flexGrow: 0,
            flexShrink: 0,
            textAlign: 'center',
            marginTop: 1,
            '& > * ': {
              marginLeft: 4,
              marginRight: 4,
            },
          }}
        >
          <Button onClick={onUpdateDependencyClick} disabled={editingLocked || isLoading}>
            <FunctionIcons.Refresh />
          </Button>
          <Button onClick={onOpenDependencyClick} disabled={editingLocked || isLoading}>
            <FunctionIcons.Edit />
          </Button>
          <Button onClick={onRemoveDependencyClick} disabled={editingLocked || isLoading}>
            <FunctionIcons.Delete />
          </Button>
        </div>
      )}

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
  )
}
