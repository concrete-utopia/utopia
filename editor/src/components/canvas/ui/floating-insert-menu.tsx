/** @jsxRuntime classic */
/** @jsx jsx */
import React from 'react'
import { jsx } from '@emotion/react'
import type { CSSObject } from '@emotion/serialize'
import type { OptionProps, StylesConfig } from 'react-windowed-select'

import { getControlStyles } from '../../../uuiui-deps'
import { Substores, useEditorState } from '../../editor/store/store-hook'

import { Icn, useColorTheme, UtopiaTheme } from '../../../uuiui'
import { usePossiblyResolvedPackageDependencies } from '../../editor/npm-dependency/npm-dependency'
import type {
  InsertableComponent,
  InsertableComponentGroup,
  InsertableComponentGroupType,
  InsertableVariable,
} from '../../shared/project-components'
import {
  getInsertableGroupLabel,
  getNonEmptyComponentGroups,
  isInsertableVariable,
} from '../../shared/project-components'
import { InspectorInputEmotionStyle } from '../../../uuiui/inputs/base-input'
import { optionalMap } from '../../../core/shared/optional-utils'
import type { InsertMenuMode } from './floating-insert-menu-helpers'
import {
  convertVariablesToElements,
  getVariablesInScope,
} from '../../../components/shared/scoped-variables'

export const FloatingMenuTestId = 'floating-menu-test-id'

export type InsertMenuItemValue = InsertableComponent & {
  source: InsertableComponentGroupType | null
  key: string
}

export type InsertMenuItem = {
  label: string
  source: string | null
  value: InsertMenuItemValue
}

export type InsertMenuItemGroup = {
  label: string
  options: Array<InsertMenuItem>
}

export type InsertableComponentFlatList = Array<InsertMenuItemGroup>

function convertInsertableComponentsToFlatList(
  insertableComponents: InsertableComponentGroup[],
): InsertableComponentFlatList {
  return insertableComponents.flatMap((componentGroup) => {
    return {
      label: getInsertableGroupLabel(componentGroup.source),
      options: componentGroup.insertableComponents.map(
        (componentToBeInserted, index): InsertMenuItem => {
          const source = index === 0 ? componentGroup.source : null
          let label = componentToBeInserted.name
          // there is no indentation, so for inner props we want to show here the full path (i.e myObj.myProp)
          if (
            isInsertableVariable(componentToBeInserted) &&
            componentToBeInserted.originalName != null
          ) {
            label = componentToBeInserted.originalName
          }
          return {
            label: label,
            source: optionalMap(getInsertableGroupLabel, source),
            value: {
              ...componentToBeInserted,
              key: `${getInsertableGroupLabel(componentGroup.source)}-${label}`,
              source: source,
            },
          }
        },
      ),
    }
  })
}

export function useGetInsertableComponents(
  insertMenuMode: InsertMenuMode,
): InsertableComponentFlatList {
  const dependencies = usePossiblyResolvedPackageDependencies()

  const { packageStatus, propertyControlsInfo } = useEditorState(
    Substores.restOfEditor,
    (store) => {
      return {
        packageStatus: store.editor.nodeModules.packageStatus,
        propertyControlsInfo: store.editor.propertyControlsInfo,
      }
    },
    'useGetInsertableComponents',
  )

  const projectContents = useEditorState(
    Substores.projectContents,
    (store) => store.editor.projectContents,
    'useGetInsertableComponents projectContents',
  )

  const fullPath = useEditorState(
    Substores.canvas,
    (store) => store.editor.canvas.openFile?.filename ?? null,
    'useGetInsertableComponents fullPath',
  )

  const insertableComponents = React.useMemo(() => {
    if (fullPath == null) {
      return []
    } else {
      return convertInsertableComponentsToFlatList(
        getNonEmptyComponentGroups(
          insertMenuMode,
          packageStatus,
          propertyControlsInfo,
          projectContents,
          dependencies,
          fullPath,
        ),
      )
    }
  }, [packageStatus, propertyControlsInfo, projectContents, dependencies, fullPath, insertMenuMode])

  const scopedVariables = useEditorState(
    Substores.variablesInScope,
    (store) =>
      getVariablesInScope(
        store.editor.selectedViews[0],
        projectContents,
        store.editor.variablesInScope,
        store.editor.jsxMetadata,
      ),
    'useGetInsertableComponents scopedVariables',
  )

  const insertableVariables = React.useMemo(() => {
    if (fullPath == null) {
      return []
    } else {
      return convertInsertableComponentsToFlatList(convertVariablesToElements(scopedVariables))
    }
  }, [fullPath, scopedVariables])

  if (insertMenuMode === 'insert') {
    return insertableComponents.concat(insertableVariables)
  } else {
    return insertableComponents
  }
}

export function useComponentSelectorStyles(): StylesConfig<InsertMenuItem, false> {
  const colorTheme = useColorTheme()
  // componentSelectorStyles will only be recreated if the theme changes, otherwise we re-use the same object
  return React.useMemo(
    () => ({
      container: (styles): CSSObject => ({
        // the outermost element. It contains the popup menu,  so don't set a height on it!
        // shouldn't contain any sizing
        // ...styles,
        flexGrow: 1,
        display: 'flex',
        flexDirection: 'column',
      }),
      control: (styles): CSSObject => ({
        // need to remove styles here, since that implicitly sets a height of 38
        // ...styles,
        display: 'flex',
        background: 'transparent',
        height: UtopiaTheme.layout.rowHeight.normal,
        outline: 'none',
        paddingLeft: 8,
        paddingRight: 8,
        ':focus-within': {
          outline: 'none',
          border: 'none',
        },
      }),
      valueContainer: (styles): CSSObject => ({
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
        paddingLeft: 0,
        paddingRight: 0,
        paddingTop: 0,
        paddingBottom: 0,
      }),
      indicatorsContainer: (styles): CSSObject => ({
        // ...styles,
        display: 'none',
      }),

      multiValue: (styles, { data }): CSSObject => {
        return {
          // ...styles,
          cursor: 'pointer',
          display: 'flex',
          alignItems: 'center',
        }
      },
      multiValueLabel: (styles, { data }): CSSObject => ({
        // ...styles,
        fontSize: 10,
        padding: '2px 4px',
      }),
      multiValueRemove: (styles, { data }): CSSObject => ({
        // ...styles,
        width: 11,
        display: 'flex',
        paddingTop: 2,
        opacity: 0.4,
        color: styles.color,
        ':hover': {
          opacity: 1,
          backgroundColor: styles.color,
        },
      }),
      menu: (styles): CSSObject => {
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
      menuList: (styles): CSSObject => {
        // the list wrapper
        return {
          // ...styles,
          position: 'relative',
          maxHeight: 210,
          paddingLeft: 8,
          paddingRight: 8,
          overflowY: 'auto',
          display: 'flex',
          flexDirection: 'column',
          gap: 6,
          paddingBottom: UtopiaTheme.layout.rowHeight.large,
        }
      },
      input: (styles): CSSObject => {
        return {
          // ...styles,
          ...(InspectorInputEmotionStyle({
            hasLabel: false,
            controlStyles: getControlStyles('simple'),
          }) as CSSObject),
          paddingLeft: 4,
          backgroundColor: colorTheme.seperator.value,
          flexGrow: 1,
          display: 'flex',
          alignItems: 'center',
        }
      },
      placeholder: (styles): CSSObject => {
        return { ...styles, marginLeft: 4 }
      },
      option: (styles, { data, isDisabled, isFocused, isSelected }): CSSObject => {
        // a single entry in the options list

        return {
          // ...styles,
          height: UtopiaTheme.layout.rowHeight.smaller,
          display: 'flex',
          alignItems: 'center',
          paddingLeft: 4,
          paddingRight: 4,
          cursor: isDisabled ? 'not-allowed' : 'default',
          color: isFocused ? colorTheme.bg0.value : colorTheme.fg0.value,
          backgroundColor: 'transparent',
          borderRadius: UtopiaTheme.inputBorderRadius,
        }
      },
      group: (): CSSObject => {
        return {
          // ...styles,
        }
      },
      groupHeading: (styles): CSSObject => {
        return {
          // ...styles,
          display: 'none',
        }
      },
    }),
    [colorTheme],
  )
}

export const CustomComponentOption = (props: OptionProps<InsertMenuItem, false>) => {
  const { innerRef, innerProps, isDisabled, isFocused, label, data } = props
  const colorTheme = useColorTheme()
  return (
    <div
      ref={innerRef}
      {...innerProps}
      data-testid={`floating-menu-item-${label}`}
      style={{
        boxSizing: 'border-box',
        height: UtopiaTheme.layout.rowHeight.smaller,
        display: 'flex',
        alignItems: 'center',
        justifyContent: 'space-between',
        cursor: isDisabled ? 'not-allowed' : 'default',
        color: isFocused ? colorTheme.dynamicBlue.value : colorTheme.fg0.value,
        backgroundColor: 'transparent',
        borderRadius: UtopiaTheme.inputBorderRadius,
      }}
    >
      <div style={{ display: 'flex', gap: 8, flexDirection: 'row', paddingRight: 2 }}>
        <Icn
          category='semantic'
          type='classicarrow-right'
          width={16}
          height={16}
          style={{ opacity: isFocused ? 1 : 0 }}
          color='dynamic'
        />
        {label}
      </div>
      <div
        style={{
          color: isFocused ? colorTheme.dynamicBlue30.value : colorTheme.fg7.value,
          height: UtopiaTheme.layout.rowHeight.smaller,
          right: 12,
          display: 'flex',
          alignItems: 'center',
          pointerEvents: 'none',
          overflow: 'hidden',
        }}
      >
        <div style={{ overflow: 'hidden', textOverflow: 'ellipsis', whiteSpace: 'nowrap' }}>
          {data.source ?? ''}
        </div>
        <span style={{ minWidth: 0 }}></span>
      </div>
    </div>
  )
}
