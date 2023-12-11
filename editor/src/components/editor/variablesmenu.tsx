/** @jsxRuntime classic */
/** @jsx jsx */
import type { CSSObject } from '@emotion/react'
import { jsx } from '@emotion/react'
import React, { useState } from 'react'
import type {
  InputActionMeta,
  InputProps,
  MenuListComponentProps,
  OptionProps,
  StylesConfig,
} from 'react-windowed-select'
import WindowedSelect, { components, createFilter } from 'react-windowed-select'
import { RightMenuTab } from './store/editor-state'
import { Icn, UIRow, UtopiaTheme, useColorTheme } from '../../uuiui'
import { getControlStyles } from '../../uuiui-deps'
import { InspectorInputEmotionStyle } from '../../uuiui/inputs/base-input'
import type { ProjectContentTreeRoot } from '../assets'
import type { InsertableComponent, InsertableComponentGroup } from '../shared/project-components'
import { getInsertableGroupLabel } from '../shared/project-components'
import { setRightMenuTab } from './actions/action-creators'
import type { Mode } from './editor-modes'
import { useDispatch } from './store/dispatch-context'
import { Substores, useEditorState } from './store/store-hook'
import type { AllVariablesInScope } from '../shared/scoped-variables'
import { convertVariablesToElements, getVariablesInScope } from '../shared/scoped-variables'
import { useToInsert } from './insert-callbacks'
import type {
  InsertMenuItem,
  InsertMenuVariableItem,
  InsertableComponentFlatList,
} from '../canvas/ui/floating-insert-menu'
import { optionalMap } from '../../core/shared/optional-utils'

export const VariablesMenuFilterTestId = 'insert-menu-filter'

interface VariablesMenuProps {
  mode: Mode
  currentlyOpenFilename: string | null
  projectContents: ProjectContentTreeRoot
  scopedVariables: AllVariablesInScope[]
}

export const VariablesMenu = React.memo(() => {
  const restOfEditorProps = useEditorState(
    Substores.restOfEditor,
    (store) => {
      return {
        mode: store.editor.mode,
      }
    },
    'VariablesMenu restOfEditorProps',
  )

  const selectedViews = useEditorState(
    Substores.selectedViews,
    (store) => store.editor.selectedViews,
    'VariablesMenu selectedViews',
  )

  const canvasProps = useEditorState(
    Substores.canvas,
    (store) => {
      return {
        currentlyOpenFilename: store.editor.canvas.openFile?.filename ?? null,
        canvasScale: store.editor.canvas.scale,
      }
    },
    'VariablesMenu canvasProps',
  )

  const projectContents = useEditorState(
    Substores.projectContents,
    (store) => store.editor.projectContents,
    'VariablesMenu projectContents',
  )

  const scopedVariables = useEditorState(
    Substores.variablesInScope,
    (store) =>
      getVariablesInScope(
        selectedViews[0],
        projectContents,
        store.editor.variablesInScope,
        store.editor.jsxMetadata,
      ),
    'VariablesMenu scopedVariables',
  )

  const propsWithDependencies: VariablesMenuProps = {
    ...restOfEditorProps,
    ...canvasProps,
    projectContents: projectContents,
    scopedVariables: scopedVariables,
  }

  return <VariablesMenuInner {...propsWithDependencies} />
})

const Input = (props: InputProps) => {
  return <components.Input {...props} data-testid={VariablesMenuFilterTestId} />
}

const BASE_PADDING = 4
const DEPTH_PADDING = 16
function paddingByDepth(insertMenuItem: InsertMenuVariableItem) {
  const depth = insertMenuItem.value.depth
  const depthValue = depth == null ? 0 : depth
  return {
    padding: BASE_PADDING,
    paddingLeft: BASE_PADDING + depthValue * DEPTH_PADDING,
  }
}

const iconByType = (insertMenuItem: InsertMenuVariableItem): string => {
  const variableType = insertMenuItem.value.variableType

  const iconsByType: Record<string, string> = {
    string: 'text',
    number: 'text',
    boolean: 'conditional',
    object: 'text-generated',
    array: 'lists',
    image: 'image',
  }
  return iconsByType[variableType] ?? 'component'
}

const Option = React.memo((props: OptionProps<ComponentOptionItem, false>) => {
  const colorTheme = useColorTheme()
  const [isHovered, setIsHovered] = useState(false)
  const setIsHoveredTrue = React.useCallback(() => {
    setIsHovered(true)
  }, [])

  const setIsHoveredFalse = React.useCallback(() => {
    setIsHovered(false)
  }, [])

  return (
    <div ref={props.innerRef} {...props.innerProps} style={{}}>
      <UIRow
        rowHeight={'smaller'}
        css={{
          borderRadius: 2,
          color: isHovered ? colorTheme.dynamicBlue.value : colorTheme.fg1.value,
          background: undefined,
          gap: 4,
          border: '1px solid transparent',
          ...paddingByDepth(props.data as InsertMenuVariableItem),
        }}
        onMouseEnter={setIsHoveredTrue}
        onMouseLeave={setIsHoveredFalse}
        data-testid={`insert-item-${props.label}`}
      >
        <Icn
          category='element'
          type={iconByType(props.data as InsertMenuVariableItem)}
          color={isHovered ? 'dynamic' : 'main'}
          width={18}
          height={18}
          style={{ transform: 'scale(0.8)' }}
        />
        <span>{props.label}</span>
      </UIRow>
    </div>
  )
})

type GroupOptionItem = {
  label: string
  options: ComponentOptionItem[]
}

type ComponentOptionItem = {
  label: string
  source: string
  value: InsertableComponent
}

function useSelectStyles(
  hasResults: boolean,
  hasOptions: boolean,
): StylesConfig<GroupOptionItem, false> {
  const colorTheme = useColorTheme()
  return React.useMemo(
    () => ({
      container: (styles): CSSObject => ({
        height: '100%',
      }),
      control: (styles): CSSObject => ({
        background: 'transparent',
        outline: 'none',
        ':focus-within': {
          outline: 'none',
          border: 'none',
        },
        padding: 8,
      }),
      valueContainer: (styles): CSSObject => ({
        display: 'flex',
        position: 'relative',
        flexGrow: 1,
        flexShrink: 0,
        alignItems: 'center',
        gap: 4,
      }),
      indicatorsContainer: (styles): CSSObject => ({
        display: 'none',
      }),
      menu: (styles): CSSObject => {
        return {
          marginTop: 8,
          paddingLeft: 8,
          paddingRight: 8,
          height: '100%',
        }
      },
      menuList: (styles): CSSObject => {
        return {
          height: '100%',
          overflow: 'scroll',
          paddingBottom: 100,
          display: 'flex',
          flexDirection: 'column',
          gap: 2,
          paddingLeft: 8,
          paddingRight: 8,
        }
      },
      input: (styles): CSSObject => {
        return {
          ...(InspectorInputEmotionStyle({
            hasLabel: false,
            controlStyles: getControlStyles(hasOptions ? 'simple' : 'disabled'),
          }) as CSSObject),
          paddingLeft: 4,
          backgroundColor: colorTheme.bg2.value,
          flexGrow: 1,
          display: 'flex',
          alignItems: 'center',
          cursor: hasOptions ? 'text' : 'default',
          border: `1px solid ${hasOptions && !hasResults ? colorTheme.error.value : 'transparent'}`,
          opacity: hasOptions ? 1 : 0.5,
        }
      },
      placeholder: (styles): CSSObject => {
        return {
          ...styles,
          position: 'absolute',
          marginLeft: 5,
        }
      },
      group: (): CSSObject => {
        return {
          display: 'flex',
          flexDirection: 'column',
          gap: 2,
        }
      },
      groupHeading: (styles): CSSObject => {
        return {
          display: 'flex',
          alignItems: 'center',
          height: UtopiaTheme.layout.rowHeight.smaller,
          fontWeight: 700,
        }
      },
    }),
    [colorTheme.bg2.value, colorTheme.error.value, hasOptions, hasResults],
  )
}

const MenuList = React.memo((menuListProps: MenuListComponentProps<ComponentOptionItem, false>) => {
  return <components.MenuList {...menuListProps} />
})

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

const VariablesMenuInner = React.memo((props: VariablesMenuProps) => {
  const toInsertCallback = useToInsert()

  const toInsertAndClose = React.useCallback(
    (toInsert: InsertMenuItem | null) => {
      toInsertCallback(toInsert)
    },
    [toInsertCallback],
  )

  const dispatch = useDispatch()
  const [filter, setFilter] = React.useState('')

  const insertableVariables = React.useMemo(() => {
    if (props.currentlyOpenFilename == null) {
      return []
    } else {
      return convertInsertableComponentsToFlatList(
        convertVariablesToElements(props.scopedVariables),
      )
    }
  }, [props.currentlyOpenFilename, props.scopedVariables])

  const filterOption = createFilter({
    ignoreAccents: true,
    stringify: (c: { data: InsertMenuVariableItem }) =>
      c.data.source + c.data.label + c.data.value.originalName,
    ignoreCase: true,
    trim: true,
    matchFrom: 'any',
  })

  const { hasResults } = React.useMemo(() => {
    const filteredOptions = insertableVariables
      .flatMap((g) => g.options)
      .filter((o) => filterOption({ data: o } as any, filter))
    return {
      hasResults: filteredOptions.length > 0,
    }
  }, [insertableVariables, filterOption, filter])

  const { hasOptions } = React.useMemo(() => {
    return {
      hasOptions: insertableVariables.some((g) => g.options.length > 0),
    }
  }, [insertableVariables])

  function onFilterChange(newValue: string, actionMeta: InputActionMeta) {
    if (actionMeta.action !== 'input-blur' && actionMeta.action !== 'menu-close') {
      setFilter(newValue.trim())
    }
  }

  const onKeyDown = React.useCallback(
    (e: React.KeyboardEvent) => {
      setIsActive(true)
      if (e.key === 'Escape') {
        dispatch([setRightMenuTab(RightMenuTab.Inspector)])
      }
    },
    [dispatch],
  )

  const onChange = React.useCallback(
    (e: InsertMenuItem) => {
      onFilterChange(e.label, { action: 'input-change' })
      toInsertAndClose(e)
    },
    [toInsertAndClose],
  )

  function alwaysTrue() {
    return true
  }

  function noVariablesMessage() {
    return hasOptions ? 'No results' : 'No variables in scope'
  }

  const styles = useSelectStyles(hasResults, hasOptions)

  const [isActive, setIsActive] = React.useState(true)
  function onMouseLeave() {
    setIsActive(false)
  }
  function onMouseEnter() {
    setIsActive(true)
  }

  return (
    <div style={{ height: '100%' }} onMouseLeave={onMouseLeave} onMouseEnter={onMouseEnter}>
      <WindowedSelect
        autoFocus
        key={'variables-menu-select'}
        isMulti={false}
        controlShouldRenderValue={false}
        hideSelectedOptions={false}
        menuIsOpen
        placeholder='Select…'
        tabSelectsValue={false}
        options={insertableVariables}
        onKeyDown={onKeyDown}
        mode={props.mode}
        noOptionsMessage={noVariablesMessage}
        components={{
          Option: Option,
          Input: Input,
          MenuList: MenuList,
        }}
        isActive={isActive}
        onChange={onChange}
        styles={styles}
        filterOption={!hasResults ? alwaysTrue : filterOption}
        isDisabled={!hasOptions}
      />
    </div>
  )
})
