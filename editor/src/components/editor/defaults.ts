import {
  JSXElement,
  jsxElement,
  jsxAttributeValue,
  jsxElementName,
  jsxAttributeOtherJavaScript,
} from '../../core/shared/element-template'
import { LayoutSystem, NormalisedFrame } from 'utopia-api'
import { PathForResizeContent } from '../../core/model/scene-utils'
import * as PP from '../../core/shared/property-path'

export function defaultSceneElement(
  uid: string,
  componentName: string | null,
  frame: NormalisedFrame | any,
  label: string,
  resizeContent: boolean = true,
  componentProps: any = null,
): JSXElement {
  const component = componentName == null ? 'null' : componentName
  const props = {
    'data-uid': jsxAttributeValue(uid),
    'data-label': jsxAttributeValue(label),
    component: jsxAttributeOtherJavaScript(component, `return ${componentName}`, [], null),
    [PP.toString(PathForResizeContent)]: jsxAttributeValue(resizeContent),
    style: jsxAttributeValue({
      position: 'absolute',
      ...frame,
    }),
  }
  if (componentProps != null) {
    props['props'] = jsxAttributeValue(componentProps)
  }

  return jsxElement(jsxElementName('Scene', []), props, [])
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
  )
}

export function defaultDivElement(uid: string): JSXElement {
  return jsxElement(
    jsxElementName('div', []),
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
  )
}
