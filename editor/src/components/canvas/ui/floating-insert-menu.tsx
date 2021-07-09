import * as React from 'react'
import { betterReactMemo } from '../../../uuiui-deps'
import { useEditorState } from '../../editor/store/store-hook'

import styled from '@emotion/styled'
import { FlexColumn, FlexRow, HeadlessStringInput } from '../../../uuiui'
import { usePossiblyResolvedPackageDependencies } from '../../editor/npm-dependency/npm-dependency'
import {
  getComponentGroups,
  getInsertableGroupLabel,
  InsertableComponentGroup,
} from '../../shared/project-components'

function useGetInsertableComponents(): Array<InsertableComponentGroup> {
  const dependencies = usePossiblyResolvedPackageDependencies()

  const { packageStatus, propertyControlsInfo, projectContents, fullPath } = useEditorState(
    (store) => {
      return {
        packageStatus: store.editor.nodeModules.packageStatus,
        propertyControlsInfo: store.editor.propertyControlsInfo,
        projectContents: store.editor.projectContents,
        fullPath: store.editor.canvas.openFile?.filename ?? null,
      }
    },
    'RenderAsRow',
  )

  const insertableComponents = React.useMemo(() => {
    if (fullPath == null) {
      return []
    } else {
      return getComponentGroups(
        packageStatus,
        propertyControlsInfo,
        projectContents,
        dependencies,
        fullPath,
      )
    }
  }, [packageStatus, propertyControlsInfo, projectContents, dependencies, fullPath])

  return insertableComponents
}

export const ListItem = styled.div((props: { selected?: boolean }) => ({
  flex: '0 0 25px',
  display: 'flex',
  alignItems: 'center',
  borderRadius: 2,
  paddingLeft: 4,
  paddingRight: 4,
  background: 'transparent',
  color: 'hsl(0,0%,10%)', // theme
  '&:hover': {
    background: '#007aff',
    color: 'white',
  },
}))

export const Subdued = styled.div({
  color: 'hsl(0,0%,70%)',
})

const showInsertionOptions = false

export var FloatingMenu = () => {
  const insertableComponents = useGetInsertableComponents()
  const [filterString, setFilterString] = React.useState('')
  const onFilterInput = React.useCallback((event: React.FormEvent<HTMLInputElement>) => {
    setFilterString(event.currentTarget.value)
  }, [])

  return (
    <div
      style={{
        width: '100%',
        height: '100%',
        backgroundColor: '#fefefe',
        position: 'relative',
        margin: 20,
        fontSize: 11,
      }}
    >
      <FlexColumn
        style={{
          border: '1px solid hsl(0,0%,93%)',
          borderRadius: 3,
          background: 'hsl(0,0%,98%)',
          boxShadow: '1px 1px 3px #00000022',
          width: 220,
          overflow: 'hidden',
          height: 300,
        }}
      >
        <FlexRow
          style={{
            paddingLeft: 8,
            minHeight: 34,
            color: '#007aff',
            fontWeight: 600,
          }}
        >
          Insert
        </FlexRow>
        <FlexRow style={{ minHeight: 34 }}>
          <HeadlessStringInput
            style={{
              border: 'none',
              height: 22,
              paddingLeft: 4,
              background: 'hsl(0,0%,96%)',
              flexGrow: 1,
            }}
            onInput={onFilterInput}
            placeholder='Type to filter'
          />
        </FlexRow>
        <FlexColumn
          style={{
            minHeight: 80,
            overflowY: 'scroll',
            paddingLeft: 8,
            paddingRight: 8,
            flexGrow: 1,
          }}
        >
          {insertableComponents.map((componentGroup) => {
            const groupLabel = getInsertableGroupLabel(componentGroup.source)
            const filterStringLowercase = filterString.toLowerCase()
            const filteredComponents = componentGroup.insertableComponents.filter(
              (insertableComponent) =>
                insertableComponent.name.toLowerCase().startsWith(filterStringLowercase),
            )
            if (filteredComponents.length == 0) {
              return null
            } else {
              return (
                <React.Fragment key={groupLabel}>
                  <Subdued>{groupLabel}</Subdued>
                  {filteredComponents.map((insertableComponent) => {
                    return (
                      <ListItem key={insertableComponent.name}>{insertableComponent.name}</ListItem>
                    )
                  })}
                </React.Fragment>
              )
            }
          })}
        </FlexColumn>
        {showInsertionOptions ? (
          <FlexColumn
            style={{
              borderTop: '1px solid hsl(0,0%,93%)',
              minHeight: 48,
              paddingTop: 4,
              paddingLeft: 8,
              paddingRight: 8,
            }}
          >
            <Subdued
              style={{
                lineHeight: 1.3,
                fontFamily: 'Inter',
                fontSize: 10,
              }}
            >
              display: 'flex', flexDirection: 'column', alignItems: 'center', justifyContent:
              'center'
            </Subdued>
            <FlexRow style={{ height: 34, gap: 8, padding: 0 }}>
              <input type='checkbox' />
              <label htmlFor='withContent'>Add content</label>
            </FlexRow>
            <FlexRow style={{ height: 34, gap: 8, padding: 0 }}>
              <input type='checkbox' />
              <label htmlFor='withContent'>Fixed dimensions</label>
            </FlexRow>
          </FlexColumn>
        ) : null}
      </FlexColumn>
    </div>
  )
}

interface FloatingInsertMenuProps {}

export const FloatingInsertMenu = betterReactMemo(
  'FloatingInsertMenu',
  (props: FloatingInsertMenuProps) => {
    const isVisible = useEditorState(
      (store) => store.editor.floatingInsertMenu.insertMenuOpen,
      'FloatingInsertMenu insertMenuOpen',
    )
    return isVisible ? (
      <div
        style={{
          pointerEvents: 'initial',
          position: 'absolute',
          left: '50%',
          top: '50%',
          transform: 'translateX(-50%) translateY(-50%)',
        }}
      >
        <FloatingMenu />
      </div>
    ) : null
  },
)
