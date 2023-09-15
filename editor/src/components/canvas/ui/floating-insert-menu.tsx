/** @jsxRuntime classic */
/** @jsx jsx */
import React from 'react'
import { jsx } from '@emotion/react'
import type { CSSObject } from '@emotion/serialize'
import type { InputActionMeta, OptionProps, StylesConfig } from 'react-windowed-select'
import WindowedSelect from 'react-windowed-select'

import { getControlStyles } from '../../../uuiui-deps'
import { Substores, useEditorState, useRefEditorState } from '../../editor/store/store-hook'

import {
  FlexColumn,
  FlexRow,
  OnClickOutsideHOC,
  useColorTheme,
  UtopiaStyles,
  UtopiaTheme,
} from '../../../uuiui'
import { usePossiblyResolvedPackageDependencies } from '../../editor/npm-dependency/npm-dependency'
import type {
  InsertableComponent,
  InsertableComponentGroup,
  InsertableComponentGroupType,
} from '../../shared/project-components'
import {
  getInsertableGroupLabel,
  getNonEmptyComponentGroups,
  insertableComponent,
} from '../../shared/project-components'
import {
  closeFloatingInsertMenu,
  insertInsertable,
  updateJSXElementName,
  wrapInElement,
} from '../../editor/actions/action-creators'
import { generateUidWithExistingComponents } from '../../../core/model/element-template-utils'
import type {
  JSXConditionalExpressionWithoutUID,
  JSXFragmentWithoutUID,
} from '../../../core/shared/element-template'
import {
  emptyComments,
  jsExpressionValue,
  jsxElement,
  jsxTextBlock,
  setJSXAttributesAttribute,
} from '../../../core/shared/element-template'
import {
  getElementsToTarget,
  useHandleCloseOnESCOrEnter,
} from '../../inspector/common/inspector-utils'
import type { EditorAction } from '../../editor/action-types'
import { InspectorInputEmotionStyle } from '../../../uuiui/inputs/base-input'
import type { ElementPath, Imports } from '../../../core/shared/project-file-types'
import { safeIndex } from '../../../core/shared/array-utils'
import { optionalMap } from '../../../core/shared/optional-utils'
import { useDispatch } from '../../editor/store/dispatch-context'
import { assertNever } from '../../../core/shared/utils'
import { emptyImports } from '../../../core/workers/common/project-file-utils'
import { emptyElementPath } from '../../../core/shared/element-path'
import { getInsertionPath } from '../../editor/store/insertion-path'
import type { InsertMenuMode } from './floating-insert-menu-helpers'
import { getActionsToApplyChange } from '../../../components/editor/convert-callbacks'
import { isFeatureEnabled } from '../../../utils/feature-switches'

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

type InsertMenuItemGroup = {
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
          return {
            label: componentToBeInserted.name,
            source: optionalMap(getInsertableGroupLabel, source),
            value: {
              ...componentToBeInserted,
              key: `${getInsertableGroupLabel(componentGroup.source)}-${
                componentToBeInserted.name
              }`,
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

  return insertableComponents
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
          // color: 'black',
          // fontSize: 11,
          flexGrow: 1,
          // letterSpacing: 0.3,
          // background: 'transparent',
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
          backgroundColor: isFocused ? colorTheme.dynamicBlue.value : 'transparent',
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

interface CheckboxRowProps {
  id: string
  checked: boolean
  onChange: (value: boolean) => void
}

export const CustomComponentOption = (props: OptionProps<InsertMenuItem, false>) => {
  const { innerRef, innerProps, isDisabled, isFocused, label, data } = props
  const colorTheme = useColorTheme()
  return (
    <div
      ref={innerRef}
      {...innerProps}
      style={{
        boxSizing: 'border-box',
        height: UtopiaTheme.layout.rowHeight.smaller,
        display: 'flex',
        alignItems: 'center',
        justifyContent: 'space-between',
        paddingLeft: 4,
        paddingRight: 4,
        cursor: isDisabled ? 'not-allowed' : 'default',
        color: isFocused ? colorTheme.bg0.value : colorTheme.fg0.value,
        backgroundColor: isFocused ? colorTheme.dynamicBlue.value : 'transparent',
        borderRadius: UtopiaTheme.inputBorderRadius,
      }}
    >
      <div style={{ paddingRight: 2 }}>{label}</div>
      <div
        style={{
          color: colorTheme.fg7.value,
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

const CheckboxRow = React.memo<React.PropsWithChildren<CheckboxRowProps>>(
  ({ id, checked, onChange, children }) => {
    const colorTheme = useColorTheme()

    const handleChange = React.useCallback(
      (event: React.ChangeEvent<HTMLInputElement>) => {
        onChange(event.target.checked)
      },
      [onChange],
    )

    return (
      <FlexRow css={{ height: 25, gap: 8, flex: 1 }}>
        <input
          type='checkbox'
          checked={checked}
          onChange={handleChange}
          css={{
            '&:focus': {
              outline: 'auto',
              outlineColor: colorTheme.dynamicBlue.value,
              outlineOffset: 0,
            },
          }}
          id={id}
        />
        <label htmlFor={id} tabIndex={1}>
          {children}
        </label>
      </FlexRow>
    )
  },
)

function getMenuTitle(insertMenuMode: InsertMenuMode): string {
  switch (insertMenuMode) {
    case 'closed':
      return ''
    case 'convert':
      return 'Convert to'
    case 'insert':
      return 'Add Element'
    case 'wrap':
      return 'Wrap in'
  }
}

export var FloatingMenu = React.memo(() => {
  const colorTheme = useColorTheme()

  const [filterInputValue, setFilterInputValue] = React.useState('')
  const onInputValueChange = React.useCallback((newValue: string, actionMeta: InputActionMeta) => {
    // when the user "tabs out" to the checkboxes, prevent react-select from clearing the input text
    if (actionMeta.action !== 'input-blur' && actionMeta.action !== 'menu-close') {
      setFilterInputValue(newValue)
    }
  }, [])

  const floatingMenuState = useEditorState(
    Substores.restOfEditor,
    (store) => store.editor.floatingInsertMenu,
    'FloatingMenu floatingMenuState',
  )

  const showInsertionControls = floatingMenuState.insertMenuMode === 'insert'

  const menuTitle: string = getMenuTitle(floatingMenuState.insertMenuMode)

  const componentSelectorStyles = useComponentSelectorStyles()
  const dispatch = useDispatch()

  const projectContentsRef = useRefEditorState((store) => store.editor.projectContents)
  const selectedViewsRef = useRefEditorState((store) => store.editor.selectedViews)
  const jsxMetadata = useEditorState(
    Substores.metadata,
    (store) => store.editor.jsxMetadata,
    'FloatingMenu jsxMetadata',
  )

  const elementPathTree = useEditorState(
    Substores.metadata,
    (store) => store.editor.elementPathTree,
    'FloatingMenu elementPathTree',
  )

  const insertableComponents = useGetInsertableComponents(floatingMenuState.insertMenuMode)

  const [addContentForInsertion, setAddContentForInsertion] = React.useState(false)
  const [fixedSizeForInsertion, setFixedSizeForInsertion] = React.useState(false)

  const onChange = React.useCallback(
    (value: InsertMenuItem | null) => {
      if (value != null) {
        const pickedInsertableComponent = (value as InsertMenuItem).value

        const actionsToDispatch = getActionsToApplyChange(
          projectContentsRef.current,
          jsxMetadata,
          elementPathTree,
          selectedViewsRef.current,
          floatingMenuState,
          fixedSizeForInsertion,
          addContentForInsertion,
          pickedInsertableComponent,
        )

        dispatch([...actionsToDispatch, closeFloatingInsertMenu()])
      }
    },
    [
      addContentForInsertion,
      dispatch,
      elementPathTree,
      fixedSizeForInsertion,
      floatingMenuState,
      jsxMetadata,
      projectContentsRef,
      selectedViewsRef,
    ],
  )

  useHandleCloseOnESCOrEnter(
    React.useCallback(
      (key: 'Escape' | 'Enter') => {
        if (key === 'Escape') {
          dispatch([closeFloatingInsertMenu()])
        }
      },
      [dispatch],
    ),
  )

  return (
    <div
      style={{
        position: 'relative',
        fontSize: 11,
      }}
      data-testid={FloatingMenuTestId}
    >
      <FlexColumn
        style={{
          ...UtopiaStyles.popup,
          width: 280,
          height: 280,
          overflow: 'hidden',
          boxShadow: UtopiaTheme.panelStyles.shadows.medium,
        }}
      >
        <div
          style={{
            color: colorTheme.dynamicBlue.value,
            display: 'flex',
            paddingLeft: 9,
            paddingRight: 8,
            height: UtopiaTheme.layout.rowHeight.normal,
            alignItems: 'center',
          }}
        >
          <b>{menuTitle}</b>
        </div>

        <WindowedSelect
          inputValue={filterInputValue}
          onInputChange={onInputValueChange}
          autoFocus
          isMulti={false}
          controlShouldRenderValue={false}
          hideSelectedOptions={false}
          menuIsOpen
          onChange={onChange}
          options={insertableComponents}
          placeholder='Search...'
          styles={componentSelectorStyles}
          tabSelectsValue={false}
          components={{ Option: CustomComponentOption }}
        />
        {showInsertionControls ? (
          <FlexRow
            css={{
              height: UtopiaTheme.layout.rowHeight.smaller,
              paddingLeft: 8,
              paddingRight: 8,
              position: 'absolute',
              bottom: 0,
              width: '100%',
              background: colorTheme.bg1.value,
              borderRadius: '0 0 6px 6px',
              borderTop: '1px solid var(--utopitheme-border1)',
            }}
          >
            <CheckboxRow
              id='add-content-label'
              checked={addContentForInsertion}
              onChange={setAddContentForInsertion}
            >
              Add content
            </CheckboxRow>
            <CheckboxRow
              id='fixed-dimensions-label'
              checked={fixedSizeForInsertion}
              onChange={setFixedSizeForInsertion}
            >
              Fixed dimensions
            </CheckboxRow>
          </FlexRow>
        ) : null}
      </FlexColumn>
    </div>
  )
})

interface FloatingInsertMenuProps {}

export const FloatingInsertMenu = React.memo((props: FloatingInsertMenuProps) => {
  const dispatch = useDispatch()
  const isVisible = useEditorState(
    Substores.restOfEditor,
    (store) => store.editor.floatingInsertMenu.insertMenuMode !== 'closed',
    'FloatingInsertMenu insertMenuOpen',
  )
  const onClickOutside = React.useCallback(() => {
    dispatch([closeFloatingInsertMenu()])
  }, [dispatch])

  return isVisible && !isFeatureEnabled('Draggable Floating Panels') ? (
    <OnClickOutsideHOC onClickOutside={onClickOutside}>
      <div
        style={{
          pointerEvents: 'initial',
          position: 'absolute',
          left: '50%',
          top: '50%',
          transform: 'translateX(-50%) translateY(-50%)',
          zIndex: 30,
          // ^ above navigator
        }}
      >
        <FloatingMenu />
      </div>
    </OnClickOutsideHOC>
  ) : null
})
