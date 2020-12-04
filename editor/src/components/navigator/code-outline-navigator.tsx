import * as React from 'react'
import { MetadataUtils } from '../../core/model/element-metadata-utils'
import { BakedInStoryboardVariableName } from '../../core/model/scene-utils'
import {
  ElementInstanceMetadata,
  isJSXElement,
  JSXElementChild,
} from '../../core/shared/element-template'
import { jsxSimpleAttributeToValue } from '../../core/shared/jsx-attributes'
import { FlexRow } from '../../uuiui'
import * as TP from '../../core/shared/template-path'
import { betterReactMemo } from '../../uuiui-deps'
import { setFocus } from '../common/actions'
import { getOpenUtopiaJSXComponentsFromState } from '../editor/store/editor-state'
import { useEditorState } from '../editor/store/store-hook'
import { BasePaddingUnit } from './navigator-item/navigator-item'
import { isRight } from '../../core/shared/either'
import { stripNulls } from '../../core/shared/array-utils'

interface CodeOutlineNavigatorRow {
  label: string
  icon: string
  padding: number
  iconColor?: string
}

export const CodeOutlineNavigator = betterReactMemo('CodeOutlineNavigator', () => {
  const { dispatch, focusedPanel, components, metadata } = useEditorState((store) => {
    return {
      dispatch: store.dispatch,
      focusedPanel: store.editor.focusedPanel,
      components: getOpenUtopiaJSXComponentsFromState(store.editor),
      metadata: store.editor.jsxMetadataKILLME,
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
    rootElementProps: { [key: string]: any },
  ): CodeOutlineNavigatorRow[] => {
    switch (element.type) {
      case 'JSX_ELEMENT':
        navigatorItems.push({
          label: element.name.baseVariable,
          icon: '□',
          padding: depth,
        })
        return element.children.reduce(
          (working, child) => collectOutlineItems(child, depth + 1, working, rootElementProps),
          navigatorItems,
        )
      case 'JSX_FRAGMENT':
        navigatorItems.push({
          label: `<React.Fragment>`,
          icon: '',
          padding: depth,
        })
        const withChildElements = element.children.reduce(
          (working, child) => collectOutlineItems(child, depth + 1, working, rootElementProps),
          navigatorItems,
        )
        return withChildElements
      case 'JSX_TEXT_BLOCK':
        if (element.text.trim().length > 0) {
          navigatorItems.push({
            label: element.text,
            icon: '𝐓',
            padding: depth,
          })
        }
        break
      case 'JSX_CONDITIONAL_EXPRESSION':
        navigatorItems.push({
          label: 'Condition',
          padding: depth,
          icon: '',
        })
        const conditionValue =
          element.condition.type === 'ATTRIBUTE_OTHER_JAVASCRIPT'
            ? rootElementProps[element.condition.javascript.replace('props.', '')]
            : false
        const valueTrue = jsxSimpleAttributeToValue(element.whenTrue)
        if (element.whenTrue.type === 'ATTRIBUTE_OTHER_JAVASCRIPT') {
          navigatorItems.push({
            label: element.whenTrue.javascript,
            padding: depth,
            icon: '✓',
            iconColor: conditionValue ? 'green' : undefined,
          })
        } else {
          navigatorItems.push({
            label: valueTrue.value ?? '',
            padding: depth,
            icon: '✓',
            iconColor: conditionValue ? 'green' : undefined,
          })
        }
        const valueFalse = jsxSimpleAttributeToValue(element.whenFalse)
        if (element.whenFalse.type === 'ATTRIBUTE_OTHER_JAVASCRIPT') {
          navigatorItems.push({
            label: element.whenFalse.javascript,
            padding: depth,
            icon: '𝖷',
            iconColor: conditionValue ? undefined : 'red',
          })
        } else {
          navigatorItems.push({
            label: valueFalse.value ?? '',
            padding: depth,
            icon: '𝖷',
            iconColor: conditionValue ? undefined : 'red',
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
  const rootElementsMetadata = MetadataUtils.getCanvasRootScenesAndElements(metadata)
  const rows = componentsWithoutStoryBoard.reduce((working, element) => {
    const possibleRootElementProps = stripNulls(
      rootElementsMetadata.map((root) => {
        let rootElement: ElementInstanceMetadata | null
        if ((root as any).scenePath != null) {
          rootElement = MetadataUtils.getElementByInstancePathMaybe(
            metadata.elements,
            TP.instancePath([], TP.elementPathForPath((root as any).scenePath)),
          )
        } else {
          rootElement = root as ElementInstanceMetadata
        }
        if (
          rootElement != null &&
          isRight(rootElement.element) &&
          isJSXElement(rootElement.element.value)
        ) {
          if ((rootElement.element.value.props.component as any).javascript === element.name) {
            return rootElement.props
          } else {
            return null
          }
        } else {
          return null
        }
      }),
    )
    const rootElementProps = possibleRootElementProps.length > 0 ? possibleRootElementProps[0] : {}
    working.push({
      label: element.name,
      icon: '❑',
      padding: 0,
    })
    return collectOutlineItems(element.rootElement, 1, working, rootElementProps)
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
          {row.icon && (
            <span style={{ width: 10, color: row.iconColor ?? undefined }}>{row.icon}</span>
          )}
          <span style={{ paddingLeft: 5, color: row.iconColor ?? undefined }}>{row.label}</span>
        </FlexRow>
      ))}
    </div>
  )
})
