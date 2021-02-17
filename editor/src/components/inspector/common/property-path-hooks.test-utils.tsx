import * as React from 'react'
import {
  ComputedStyle,
  isJSXElement,
  isUtopiaJSXComponent,
  JSXAttributes,
  StyleAttributeMetadata,
} from '../../../core/shared/element-template'
import { isParseSuccess, TemplatePath } from '../../../core/shared/project-file-types'
import { testParseCode } from '../../../core/workers/parser-printer/parser-printer.test-utils'
import { InspectorPropsContext } from './property-path-hooks'

export const makeInspectorHookContextProvider = (
  selectedViews: Array<TemplatePath>,
  multiselectAttributes: JSXAttributes[],
  targetPath: string[],
  spiedProps: Array<{ [key: string]: any }>,
  computedStyles: Array<ComputedStyle>,
  attributeMetadatas: Array<StyleAttributeMetadata>,
) => ({ children }: any) => (
  <InspectorPropsContext.Provider
    value={{
      selectedViews: selectedViews,
      editedMultiSelectedProps: multiselectAttributes,
      targetPath,
      spiedProps: spiedProps,
      computedStyles: computedStyles,
      selectedAttributeMetadatas: attributeMetadatas,
    }}
  >
    {children}
  </InspectorPropsContext.Provider>
)

export function getPropsForStyleProp(
  targetPropExpression: string,
  target: string[],
): JSXAttributes | null {
  // this test starts with real code, and uses the parser
  // the aim here is to capture a vertical understanding from code -> UI

  const targetExprPrefix1 = `${target[0]}={`
  const targetExprPrefix2 = target
    .slice(1)
    .map((t) => `{${t}:`)
    .join('\n')

  const targetExprPostfix = target.map((t) => `}`).join('\n')
  const code = `import * as React from "react";
  import {
    Ellipse,
    Image,
    Rectangle,
    Text,
    View
  } from "utopia-api";
  import { cake } from 'cake'
  
  export var App = (props) => {
    return (
      <View
        uid={'aaa'} 
        ${targetExprPrefix1}
        ${targetExprPrefix2}
          ${targetPropExpression}
        ${targetExprPostfix}
      />
    )
  }`

  const parseResult = testParseCode(code)
  if (!isParseSuccess(parseResult)) {
    fail('expected parseResult to be Right')
  }
  const appComponent = parseResult.topLevelElements.find(isUtopiaJSXComponent)

  if (appComponent == null || !isUtopiaJSXComponent(appComponent) || appComponent.name !== `App`) {
    fail('expected the second topLevelElement to be the App component')
  }
  if (!isJSXElement(appComponent.rootElement)) {
    fail(`expected the App component's root element to be a JSXElement`)
  }

  return appComponent.rootElement.props
}
