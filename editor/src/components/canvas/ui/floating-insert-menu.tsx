/** @jsx jsx */
import * as React from 'react'
import { jsx } from '@emotion/react'

import { betterReactMemo } from '../../../uuiui-deps'
import { useEditorState, useRefEditorState } from '../../editor/store/store-hook'

import styled from '@emotion/styled'
import { FlexColumn, FlexRow, HeadlessStringInput, OnClickOutsideHOC } from '../../../uuiui'
import { usePossiblyResolvedPackageDependencies } from '../../editor/npm-dependency/npm-dependency'
import {
  getComponentGroups,
  getInsertableGroupLabel,
  InsertableComponent,
  InsertableComponentGroup,
  InsertableComponentGroupType,
} from '../../shared/project-components'
import { closeFloatingInsertMenu, wrapInView } from '../../editor/actions/action-creators'
import { generateUidWithExistingComponents } from '../../../core/model/element-template-utils'
import {
  jsxAttributeValue,
  jsxElement,
  setJSXAttributesAttribute,
} from '../../../core/shared/element-template'
import { emptyComments } from '../../../core/workers/parser-printer/parser-printer-comments'
import { useHandleCloseOnESCOrEnter } from '../../inspector/common/inspector-utils'

function useFocusOnMount<T extends HTMLElement>(): React.RefObject<T> {
  const ref = React.useRef<T>(null)
  React.useEffect(() => {
    // eslint-disable-next-line no-unused-expressions
    ref.current?.focus()
  }, [ref])
  return ref
}

type InsertMenuItem = InsertableComponent & {
  source: InsertableComponentGroupType | null
  key: string
}

type InsertableComponentFlatList = Array<InsertMenuItem>

function convertInsertableComponentsToFlatList(
  insertableComponents: InsertableComponentGroup[],
): InsertableComponentFlatList {
  return insertableComponents.flatMap((componentGroup) => {
    return componentGroup.insertableComponents.map(
      (insertableComponent, index): InsertMenuItem => {
        const source = index === 0 ? componentGroup.source : null
        return {
          ...insertableComponent,
          key: `${getInsertableGroupLabel(componentGroup.source)}-${insertableComponent.name}`,
          source: source,
        }
      },
    )
  })
}

function useGetInsertableComponents(): InsertableComponentFlatList {
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
      return convertInsertableComponentsToFlatList(
        getComponentGroups(
          packageStatus,
          propertyControlsInfo,
          projectContents,
          dependencies,
          fullPath,
        ),
      )
    }
  }, [packageStatus, propertyControlsInfo, projectContents, dependencies, fullPath])

  return insertableComponents
}

export const ListItem: React.FunctionComponent<{
  highlighted: boolean
  insertableComponent: InsertMenuItem
  onClick: (insertableComponent: InsertMenuItem) => void
  onMouseOver: (insertableComponent: InsertMenuItem) => void
}> = betterReactMemo(
  'ListItem',
  ({ onClick, onMouseOver, insertableComponent, highlighted, ...props }) => {
    const handleClick = React.useCallback(() => {
      onClick(insertableComponent)
    }, [insertableComponent, onClick])

    const handleMouseOver = React.useCallback(() => {
      onMouseOver(insertableComponent)
    }, [insertableComponent, onMouseOver])

    return (
      <div
        onClick={handleClick}
        onMouseOver={handleMouseOver}
        css={{
          flex: '0 0 25px',
          display: 'flex',
          alignItems: 'center',
          borderRadius: 2,
          paddingLeft: 4,
          paddingRight: 4,
          background: highlighted ? '#007aff' : 'transparent', // TODO BEFORE MERGE Theme!
          color: highlighted ? 'white' : 'hsl(0,0%,10%)', // TODO BEFORE MERGE Theme!
        }}
        {...props}
      />
    )
  },
)

export const Subdued = styled.div({
  color: 'hsl(0,0%,70%)',
})

const showInsertionOptions = false

export var FloatingMenu = () => {
  const [highlightedKey, setHighlightedKey] = React.useState('')
  const dispatch = useEditorState((store) => store.dispatch, 'FloatingMenu dispatch')
  // TODO move onClickOutside to here as well?
  useHandleCloseOnESCOrEnter(
    React.useCallback(
      (key: 'Escape' | 'Enter') => {
        dispatch([closeFloatingInsertMenu()])
      },
      [dispatch],
    ),
  )
  const inputRef = useFocusOnMount<HTMLInputElement>()
  const projectContentsRef = useRefEditorState((store) => store.editor.projectContents)
  const selectedViewsref = useRefEditorState((store) => store.editor.selectedViews)
  const insertableComponents = useGetInsertableComponents()
  const [filterString, setFilterString] = React.useState('')
  const onFilterInput = React.useCallback((event: React.FormEvent<HTMLInputElement>) => {
    setFilterString(event.currentTarget.value)
  }, [])

  const onClickElement = React.useCallback(
    (insertableComponent: InsertableComponent) => {
      const newUID = generateUidWithExistingComponents(projectContentsRef.current)
      const newElement = jsxElement(
        insertableComponent.element.name,
        newUID,
        setJSXAttributesAttribute(
          insertableComponent.element.props,
          'data-uid',
          jsxAttributeValue(newUID, emptyComments),
        ),
        insertableComponent.element.children,
      )
      dispatch([
        wrapInView(selectedViewsref.current, {
          element: newElement,
          importsToAdd: insertableComponent.importsToAdd,
        }),
        closeFloatingInsertMenu(),
      ])
    },
    [dispatch, projectContentsRef, selectedViewsref],
  )

  const onMouseOverElement = React.useCallback((insertableComponent: InsertMenuItem) => {
    setHighlightedKey(insertableComponent.key)
  }, [])

  return (
    <div
      style={{
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
            ref={inputRef}
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
          {insertableComponents.map((insertableComponent, index) => {
            return (
              <React.Fragment key={insertableComponent.key}>
                {insertableComponent.source != null ? (
                  <Subdued>{getInsertableGroupLabel(insertableComponent.source)}</Subdued>
                ) : null}
                <ListItem
                  key={insertableComponent.key}
                  onClick={onClickElement}
                  onMouseOver={onMouseOverElement}
                  insertableComponent={insertableComponent}
                  highlighted={highlightedKey === insertableComponent.key}
                >
                  {insertableComponent.name}
                </ListItem>
              </React.Fragment>
            )
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
    const dispatch = useEditorState((store) => store.dispatch, 'FloatingInsertMenu dispatch')
    const isVisible = useEditorState(
      (store) => store.editor.floatingInsertMenu.insertMenuOpen,
      'FloatingInsertMenu insertMenuOpen',
    )
    const onClickOutside = React.useCallback(() => {
      dispatch([closeFloatingInsertMenu()])
    }, [dispatch])

    return isVisible ? (
      <OnClickOutsideHOC onClickOutside={onClickOutside}>
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
      </OnClickOutsideHOC>
    ) : null
  },
)
