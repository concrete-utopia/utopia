import * as React from 'react'
import { betterReactMemo } from '../../../uuiui-deps'
import { useEditorState } from '../../editor/store/store-hook'

import styled from '@emotion/styled'
import { FlexColumn, FlexRow } from '../../../uuiui'
import { usePossiblyResolvedPackageDependencies } from '../../editor/npm-dependency/npm-dependency'
import { getComponentGroups, InsertableComponentGroup } from '../../shared/project-components'

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

  background: props.selected ? '#007aff' : 'transparent',
  color: props.selected ? 'white' : 'hsl(0,0%,10%)',
}))

export const Subdued = styled.div({
  color: 'hsl(0,0%,70%)',
})

export var FloatingMenu = () => {
  const insertableComponents = useGetInsertableComponents()

  return (
    <div
      style={{
        width: '100%',
        height: '100%',
        backgroundColor: '#fefefe',
        position: 'relative',
        padding: 20,
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
            height: 34,
            color: '#007aff',
            fontWeight: 600,
          }}
        >
          Insert
        </FlexRow>
        <FlexRow style={{ height: 34 }}>
          <input
            style={{
              border: 'none',
              height: 22,
              paddingLeft: 4,
              background: 'hsl(0,0%,96%)',
              flexGrow: 1,
            }}
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
            return componentGroup.insertableComponents.map((insertableComponent) => {
              return <ListItem key={insertableComponent.name}>{insertableComponent.name}</ListItem>
            })
          })}
        </FlexColumn>
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
            display: 'flex', flexDirection: 'column', alignItems: 'center', justifyContent: 'center'
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
