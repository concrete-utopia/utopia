/** @jsx jsx */
import * as React from 'react'
import { jsx } from '@emotion/react'
import Select, { StylesConfig } from 'react-select'

import { betterReactMemo } from '../../../uuiui-deps'
import { useEditorState, useRefEditorState } from '../../editor/store/store-hook'

import { FlexColumn, OnClickOutsideHOC, useColorTheme } from '../../../uuiui'
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

type InsertMenuItemValue = InsertableComponent & {
  source: InsertableComponentGroupType | null
  key: string
}

type InsertMenuItem = {
  label: string
  value: InsertMenuItemValue
}

type InsertMenuItemGroup = {
  label: string
  options: Array<InsertMenuItem>
}

type InsertableComponentFlatList = Array<InsertMenuItemGroup>

function convertInsertableComponentsToFlatList(
  insertableComponents: InsertableComponentGroup[],
): InsertableComponentFlatList {
  return insertableComponents.flatMap((componentGroup) => {
    return {
      label: getInsertableGroupLabel(componentGroup.source),
      options: componentGroup.insertableComponents.map(
        (insertableComponent, index): InsertMenuItem => {
          const source = index === 0 ? componentGroup.source : null
          return {
            label: insertableComponent.name,
            value: {
              ...insertableComponent,
              key: `${getInsertableGroupLabel(componentGroup.source)}-${insertableComponent.name}`,
              source: source,
            },
          }
        },
      ),
    }
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

function useComponentSelectorStyles(): StylesConfig {
  const colorTheme = useColorTheme()
  // componentSelectorStyles will only be recreated if the theme changes, otherwise we re-use the same object
  return React.useMemo(
    () => ({
      container: (styles) => ({
        // the outermost element. It contains the popup menu,  so don't set a height on it!
        // shouldn't contain any sizing
        // ...styles,
        flexGrow: 1,
        display: 'flex',
        flexDirection: 'column',
      }),
      control: (styles) => ({
        // need to remove styles here, since that implicitly sets a height of 38
        display: 'flex',
        background: 'transparent',
        outline: 'none',
        ':focus-within': {
          outline: 'none',
          border: 'none',
        },
      }),
      valueContainer: (styles) => ({
        // the container for added options (tags) and input
        // sibling to indicatorsContainer
        // default styles mess with layout, so ignore them
        // ...styles,
        display: 'flex',
        position: 'relative',
        flexGrow: 1,
        flexShrink: 0,
        overflowX: 'scroll',
        alignItems: 'center',
        gap: 4,
        // height: 22,
        paddingLeft: 4,
        paddingRight: 4,
        paddingTop: 0,
        paddingBottom: 0,
      }),
      indicatorsContainer: (styles) => ({
        display: 'none',
      }),

      multiValue: (styles, { data }) => {
        return {
          cursor: 'pointer',
          display: 'flex',
          alignItems: 'center',
        }
      },
      multiValueLabel: (styles, { data }) => ({
        // ...styles,
        fontSize: 10,
        padding: '2px 4px',
      }),
      multiValueRemove: (styles, { data }) => ({
        // ...styles,
        width: 11,
        display: 'flex',
        paddingTop: 2,
        opacity: 0.4,
        color: data.color,
        ':hover': {
          opacity: 1,
          backgroundColor: data.color,
        },
      }),
      menu: (styles) => {
        // the outer shell
        return {
          // ...styles,
          boxShadow: 'none',
          borderRadius: 0,
          background: 'transparent',
          overflowY: 'scroll',
          flex: 1,
        }
      },
      menuList: (styles) => {
        // the list wrapper
        return {
          position: 'relative',
          maxHeight: 300,
          padding: 4,
          overflowY: 'scroll',
        }
      },
      input: (styles) => {
        return {
          ...styles,
          color: 'black',
          fontSize: 11,
          flexGrow: 1,
          letterSpacing: 0.3,
          background: 'transparent',
          display: 'flex',
          alignItems: 'center',
        }
      },
      option: (styles, { data, isDisabled, isFocused, isSelected }) => {
        // a single entry in the options list

        return {
          ...styles,
          height: 25,
          display: 'flex',
          alignItems: 'center',
          paddingLeft: 8,
          paddingRight: 8,
          cursor: isDisabled ? 'not-allowed' : 'default',
        }
      },
      group: () => {
        return { paddingTop: 6 }
      },
      groupHeading: (styles) => {
        return {
          color: colorTheme.fg7.value,
          height: 25,
          right: 8,
          position: 'absolute',
          display: 'flex',
          alignItems: 'center',
        }
      },
    }),
    [colorTheme],
  )
}
export var FloatingMenu = betterReactMemo('FloatingMenu', () => {
  const colorTheme = useColorTheme()
  const componentSelectorStyles = useComponentSelectorStyles()
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
  const projectContentsRef = useRefEditorState((store) => store.editor.projectContents)
  const selectedViewsref = useRefEditorState((store) => store.editor.selectedViews)
  const insertableComponents = useGetInsertableComponents()

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

  return (
    <div
      style={{
        backgroundColor: '#fefefe', // TODO Theme
        position: 'relative',
        margin: 20,
        fontSize: 11,
      }}
    >
      <FlexColumn
        style={{
          width: 280,
          background: 'hsl(0,0%,96%)', // TODO Theme
          border: '1px solid hsl(0,0%,93%)', // TODO Theme
          borderRadius: 2,
          minHeight: 300,
          overflow: 'hidden',
          boxShadow: '0px 0px 4px 1px hsla(0,0%,30%,10%)', // TODO Theme
        }}
      >
        <div
          style={{
            display: 'flex',
            paddingLeft: 8,
            paddingRight: 8,
            height: 34,
            alignItems: 'center',
          }}
        >
          <b>Wrap In...</b>
        </div>

        <Select
          autoFocus
          isMulti={false}
          controlShouldRenderValue={false}
          hideSelectedOptions={false}
          menuIsOpen
          // eslint-disable-next-line react/jsx-no-bind
          onChange={(value, action) => {
            if (value != null && !Array.isArray(value)) {
              onClickElement(((value as any) as InsertMenuItem).value)
            }
          }}
          options={insertableComponents}
          placeholder='Search...'
          styles={componentSelectorStyles}
          tabSelectsValue={false}
        />
      </FlexColumn>
    </div>
  )
})

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
