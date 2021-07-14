/** @jsx jsx */
import * as React from 'react'
import { jsx } from '@emotion/react'
import WindowedSelect, {
  ActionMeta,
  InputActionMeta,
  StylesConfig,
  ValueType,
} from 'react-windowed-select'

import { betterReactMemo, getControlStyles } from '../../../uuiui-deps'
import { useEditorState, useRefEditorState } from '../../editor/store/store-hook'

import {
  FlexColumn,
  FlexRow,
  OnClickOutsideHOC,
  useColorTheme,
  UtopiaStyles,
  UtopiaTheme,
} from '../../../uuiui'
import { usePossiblyResolvedPackageDependencies } from '../../editor/npm-dependency/npm-dependency'
import {
  getComponentGroups,
  getInsertableGroupLabel,
  InsertableComponent,
  InsertableComponentGroup,
  InsertableComponentGroupType,
} from '../../shared/project-components'
import {
  closeFloatingInsertMenu,
  insertWithDefaults,
  updateJSXElementName,
  wrapInView,
} from '../../editor/actions/action-creators'
import { generateUidWithExistingComponents } from '../../../core/model/element-template-utils'
import {
  jsxAttributeValue,
  jsxElement,
  jsxTextBlock,
  setJSXAttributesAttribute,
} from '../../../core/shared/element-template'
import { emptyComments } from '../../../core/workers/parser-printer/parser-printer-comments'
import {
  getElementsToTarget,
  useHandleCloseOnESCOrEnter,
} from '../../inspector/common/inspector-utils'
import { EditorAction } from '../../editor/action-types'
import { InspectorInputEmotionStyle } from '../../../uuiui/inputs/base-input'

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

function useComponentSelectorStyles(): StylesConfig<InsertMenuItem, false> {
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
        paddingLeft: 0,
        paddingRight: 0,
        paddingTop: 0,
        paddingBottom: 0,
      }),
      indicatorsContainer: (styles) => ({
        // ...styles,
        display: 'none',
      }),

      multiValue: (styles, { data }) => {
        return {
          // ...styles,
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
        color: styles.color,
        ':hover': {
          opacity: 1,
          backgroundColor: styles.color,
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
          // ...styles,
          position: 'relative',
          maxHeight: 150,
          padding: 4,
          paddingLeft: 8,
          paddingRight: 8,
          overflowY: 'scroll',
        }
      },
      input: (styles) => {
        return {
          // ...styles,
          ...InspectorInputEmotionStyle({
            hasLabel: false,
            controlStyles: getControlStyles('simple'),
          }),
          paddingLeft: 4,
          backgroundColor: colorTheme.bg4.value,
          // color: 'black',
          // fontSize: 11,
          flexGrow: 1,
          // letterSpacing: 0.3,
          // background: 'transparent',
          display: 'flex',
          alignItems: 'center',
        }
      },
      placeholder: (styles) => {
        return { ...styles, marginLeft: 4 }
      },
      option: (styles, { data, isDisabled, isFocused, isSelected }) => {
        // a single entry in the options list

        return {
          // ...styles,
          height: UtopiaTheme.layout.rowHeight.smaller,
          display: 'flex',
          alignItems: 'center',
          paddingLeft: 4,
          paddingRight: 4,
          cursor: isDisabled ? 'not-allowed' : 'default',
          color: isFocused ? colorTheme.inverted.fg0.value : colorTheme.fg0.value,
          backgroundColor: isFocused ? colorTheme.primary.value : 'transparent',
          borderRadius: UtopiaTheme.inputBorderRadius,
        }
      },
      group: () => {
        return {
          // ...styles,
          paddingTop: 6,
        }
      },
      groupHeading: (styles) => {
        return {
          // ...styles,
          color: colorTheme.fg7.value,
          height: 25,
          right: 12,
          position: 'absolute',
          display: 'flex',
          alignItems: 'center',
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

const CheckboxRow = betterReactMemo<React.PropsWithChildren<CheckboxRowProps>>(
  'CheckboxRow',
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
              outlineColor: colorTheme.primary.value,
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

function getMenuTitle(insertMenuMode: 'closed' | 'insert' | 'convert' | 'wrap'): string {
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

export var FloatingMenu = betterReactMemo('FloatingMenu', () => {
  const colorTheme = useColorTheme()

  // This is a ref so that changing the highlighted element does not trigger a re-render loop
  // This is FINE because we only use the value in callbacks
  const activelySelectedInsertOptionRef = React.useRef<InsertMenuItem | null>(null)

  const ariaLiveMessages = React.useMemo(
    () => ({
      onFocus: ({ focused }: { focused: InsertMenuItem }) => {
        activelySelectedInsertOptionRef.current = focused
      },
    }),
    [],
  )

  const [filterInputValue, setFilterInputValue] = React.useState('')
  const onInputValueChange = React.useCallback((newValue, actionMeta: InputActionMeta) => {
    // when the user "tabs out" to the checkboxes, prevent react-select from clearing the input text
    if (actionMeta.action !== 'input-blur' && actionMeta.action !== 'menu-close') {
      setFilterInputValue(newValue)
    }
  }, [])

  const insertMenuMode = useEditorState(
    (store) => store.editor.floatingInsertMenu.insertMenuMode,
    'FloatingMenu insertMenuMode',
  )

  const showInsertionControls = insertMenuMode === 'insert'

  const menuTitle: string = getMenuTitle(insertMenuMode)

  const componentSelectorStyles = useComponentSelectorStyles()
  const dispatch = useEditorState((store) => store.dispatch, 'FloatingMenu dispatch')

  const projectContentsRef = useRefEditorState((store) => store.editor.projectContents)
  const selectedViewsref = useRefEditorState((store) => store.editor.selectedViews)
  const insertableComponents = useGetInsertableComponents()

  const [addContentForInsertion, setAddContentForInsertion] = React.useState(false)
  const [fixedSizeForInsertion, setFixedSizeForInsertion] = React.useState(false)

  const onChange = React.useCallback(
    (value: ValueType<InsertMenuItem, false>) => {
      if (value != null && !Array.isArray(value)) {
        const pickedInsertableComponent = (value as InsertMenuItem).value
        const selectedViews = selectedViewsref.current

        let actionsToDispatch: Array<EditorAction> = []
        if (insertMenuMode === 'wrap') {
          const newUID = generateUidWithExistingComponents(projectContentsRef.current)
          const newElement = jsxElement(
            pickedInsertableComponent.element.name,
            newUID,
            setJSXAttributesAttribute(
              pickedInsertableComponent.element.props,
              'data-uid',
              jsxAttributeValue(newUID, emptyComments),
            ),
            pickedInsertableComponent.element.children,
          )

          actionsToDispatch = [
            wrapInView(selectedViews, {
              element: newElement,
              importsToAdd: pickedInsertableComponent.importsToAdd,
            }),
          ]
        } else if (insertMenuMode === 'insert') {
          let elementToInsert = pickedInsertableComponent
          if (addContentForInsertion && pickedInsertableComponent.element.children.length === 0) {
            elementToInsert = {
              ...pickedInsertableComponent,
              element: {
                ...pickedInsertableComponent.element,
                children: [jsxTextBlock('Utopia')],
              },
            }
          }

          // TODO multiselect?
          actionsToDispatch = [
            insertWithDefaults(
              selectedViews[0],
              elementToInsert,
              fixedSizeForInsertion ? 'add-size' : 'do-not-add',
            ),
          ]
        } else if (insertMenuMode === 'convert') {
          // this is taken from render-as.tsx
          const targetsForUpdates = getElementsToTarget(selectedViews)
          actionsToDispatch = targetsForUpdates.flatMap((path) => {
            return updateJSXElementName(
              path,
              pickedInsertableComponent.element.name,
              pickedInsertableComponent.importsToAdd,
            )
          })
        }
        dispatch([...actionsToDispatch, closeFloatingInsertMenu()])
      }
    },
    [
      dispatch,
      insertMenuMode,
      projectContentsRef,
      selectedViewsref,
      fixedSizeForInsertion,
      addContentForInsertion,
    ],
  )

  useHandleCloseOnESCOrEnter(
    React.useCallback(
      (key: 'Escape' | 'Enter') => {
        if (key === 'Escape') {
          dispatch([closeFloatingInsertMenu()])
        } else {
          onChange(activelySelectedInsertOptionRef.current)
        }
      },
      [dispatch, onChange],
    ),
  )

  return (
    <div
      style={{
        position: 'relative',
        fontSize: 11,
      }}
    >
      <FlexColumn
        style={{
          ...UtopiaStyles.popup,
          width: 280,
          height: 250,
          overflow: 'hidden',
        }}
      >
        <div
          style={{
            color: colorTheme.primary.value,
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
          ariaLiveMessages={ariaLiveMessages}
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
        />
        {showInsertionControls ? (
          <FlexRow
            css={{
              height: UtopiaTheme.layout.rowHeight.normal,
              paddingLeft: 8,
              paddingRight: 8,
              borderTop: `1px solid ${colorTheme.border1.value}`,
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

export const FloatingInsertMenu = betterReactMemo(
  'FloatingInsertMenu',
  (props: FloatingInsertMenuProps) => {
    const dispatch = useEditorState((store) => store.dispatch, 'FloatingInsertMenu dispatch')
    const isVisible = useEditorState(
      (store) => store.editor.floatingInsertMenu.insertMenuMode !== 'closed',
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
