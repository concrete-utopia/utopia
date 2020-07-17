import {
  JSXElement,
  jsxElement,
  jsxAttributeValue,
  jsxElementName,
  jsxAttributeOtherJavaScript,
} from '../../core/shared/element-template'
import { LayoutSystem, NormalisedFrame } from 'utopia-api'

export function defaultSceneElement(
  uid: string,
  componentName: string | null,
  frame: NormalisedFrame,
  label: string,
): JSXElement {
  const component = componentName == null ? 'null' : componentName
  const props = {
    'data-uid': jsxAttributeValue(uid),
    'data-label': jsxAttributeValue(label),
    component: jsxAttributeOtherJavaScript(component, `return ${componentName}`, [], null),
    style: jsxAttributeValue({
      position: 'absolute',
      ...frame,
    }),
  }

  return jsxElement(jsxElementName('Scene', []), props, [], null)
}

export function defaultViewElement(uid: string): JSXElement {
  return jsxElement(
    jsxElementName('View', []),
    {
      style: jsxAttributeValue({
        backgroundColor: '#0091FFAA',
      }),
      'data-uid': jsxAttributeValue(uid),
      layout: jsxAttributeValue({
        layoutSystem: 'pinSystem',
      }),
    },
    [],
    null,
  )
}

export function defaultAnimatedDivElement(uid: string): JSXElement {
  return jsxElement(
    jsxElementName('animated', ['div']),
    {
      style: jsxAttributeValue({
        backgroundColor: '#0091FFAA',
      }),
      'data-uid': jsxAttributeValue(uid),
    },
    [],
    null,
  )
}

export function defaultTransparentViewElement(uid: string, layoutSystem: LayoutSystem): JSXElement {
  return jsxElement(
    jsxElementName('View', []),
    {
      layout: jsxAttributeValue({
        layoutSystem: layoutSystem,
      }),
      style: jsxAttributeValue({}),
      'data-uid': jsxAttributeValue(uid),
    },
    [],
    null,
  )
}

export function defaultTextElement(uid: string): JSXElement {
  return jsxElement(
    jsxElementName('Text', []),
    {
      style: jsxAttributeValue({
        fontSize: 16,
      }),
      text: jsxAttributeValue('Text'),
      'data-uid': jsxAttributeValue(uid),
    },
    [],
    null,
  )
}

export function defaultRectangleElement(uid: string): JSXElement {
  return jsxElement(
    jsxElementName('Rectangle', []),
    {
      style: jsxAttributeValue({
        backgroundColor: '#0091FFAA',
      }),
      'data-uid': jsxAttributeValue(uid),
    },
    [],
    null,
  )
}

export function defaultEllipseElement(uid: string): JSXElement {
  return jsxElement(
    jsxElementName('Ellipse', []),
    {
      style: jsxAttributeValue({
        backgroundColor: '#0091FFAA',
      }),
      'data-uid': jsxAttributeValue(uid),
    },
    [],
    null,
  )
}
