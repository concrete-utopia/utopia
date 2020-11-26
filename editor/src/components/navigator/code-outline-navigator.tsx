import * as React from 'react'
import { BakedInStoryboardVariableName } from '../../core/model/scene-utils'
import { JSXElementChild } from '../../core/shared/element-template'
import { jsxSimpleAttributeToValue } from '../../core/shared/jsx-attributes'
import { FlexRow, Icn } from '../../uuiui'
import { betterReactMemo } from '../../uuiui-deps'
import { setFocus } from '../common/actions'
import { getOpenUtopiaJSXComponentsFromState } from '../editor/store/editor-state'
import { useEditorState } from '../editor/store/store-hook'
import { BasePaddingUnit } from './navigator-item/navigator-item'

interface CodeOutlineNavigatorRow {
  label: string
  icon: string
  padding: number
}

export const CodeOutlineNavigator = betterReactMemo('CodeOutlineNavigator', () => {
  const { dispatch, focusedPanel, components } = useEditorState((store) => {
    return {
      dispatch: store.dispatch,
      focusedPanel: store.editor.focusedPanel,
      components: getOpenUtopiaJSXComponentsFromState(store.editor),
    }
  }, 'NavigatorComponent')

  const onFocus = React.useCallback(
    (e: React.FocusEvent<HTMLElement>) => {
      if (focusedPanel !== 'navigator') {
        dispatch([setFocus('navigator')])
      }
    },
    [dispatch, focusedPanel],
  )

  const collectOutlineItems = (
    element: JSXElementChild,
    depth: number,
    navigatorItems: CodeOutlineNavigatorRow[],
  ): CodeOutlineNavigatorRow[] => {
    switch (element.type) {
      case 'JSX_ELEMENT':
        navigatorItems.push({
          label: element.name.baseVariable,
          icon: '□',
          padding: depth,
        })
        return element.children.reduce(
          (working, child) => collectOutlineItems(child, depth + 1, working),
          navigatorItems,
        )
      case 'JSX_FRAGMENT':
        navigatorItems.push({
          label: `<React.Fragment>`,
          icon: '',
          padding: depth,
        })
        const withChildElements = element.children.reduce(
          (working, child) => collectOutlineItems(child, depth + 1, working),
          navigatorItems,
        )
        withChildElements.push({
          label: `</React.Fragment>`,
          icon: '',
          padding: depth,
        })
        return withChildElements
      case 'JSX_TEXT_BLOCK':
        navigatorItems.push({
          label: element.text,
          icon: '𝐓',
          padding: depth,
        })
        break
      case 'JSX_CONDITIONAL_EXPRESSION':
        navigatorItems.push({
          label: 'Condition',
          padding: depth,
          icon: '',
        })
        const valueTrue = jsxSimpleAttributeToValue(element.whenTrue)
        if (element.whenTrue.type === 'ATTRIBUTE_OTHER_JAVASCRIPT') {
          navigatorItems.push({
            label: element.whenTrue.javascript,
            padding: depth,
            icon: '✓',
          })
        } else {
          navigatorItems.push({
            label: valueTrue.value ?? '',
            padding: depth,
            icon: '✓',
          })
        }
        const valueFalse = jsxSimpleAttributeToValue(element.whenFalse)
        if (element.whenFalse.type === 'ATTRIBUTE_OTHER_JAVASCRIPT') {
          navigatorItems.push({
            label: element.whenFalse.javascript,
            padding: depth,
            icon: '𝖷',
          })
        } else {
          navigatorItems.push({
            label: valueFalse.value ?? '',
            padding: depth,
            icon: '𝖷',
          })
        }
        return navigatorItems
      case 'JSX_ARBITRARY_BLOCK':
        navigatorItems.push({
          label: element.javascript,
          padding: depth,
          icon: '{}',
        })
        return navigatorItems
    }
    return navigatorItems
  }
  const componentsWithoutStoryBoard = components.filter(
    (c) => c.name !== BakedInStoryboardVariableName,
  )
  const rows = componentsWithoutStoryBoard.reduce((working, element) => {
    working.push({
      label: element.name,
      icon: '❑',
      padding: 0,
    })
    return collectOutlineItems(element.rootElement, 1, working)
  }, [] as CodeOutlineNavigatorRow[])

  return (
    <div style={{ color: 'rgb(26, 26, 26)' }} onFocus={onFocus}>
      <FlexRow
        style={{ paddingLeft: 10, lineHeight: '35px', fontWeight: 600, backgroundColor: '#c9c9c9' }}
      >
        Code Outline
      </FlexRow>
      {rows.map((row, i) => (
        <FlexRow
          key={row.label + i}
          style={{ paddingLeft: 10 + BasePaddingUnit * row.padding, height: 25 }}
        >
          {row.icon && <span style={{ width: 10 }}>{row.icon}</span>}
          <span style={{ paddingLeft: 5 }}>{row.label}</span>
        </FlexRow>
      ))}
    </div>
  )
})
