import {
  JSXElement,
  jsxElement,
  jsxAttributeValue,
  jsxElementName,
  jsxAttributeOtherJavaScript,
} from '../../core/shared/element-template'
import { NormalisedFrame } from 'utopia-api'
import { PathForResizeContent } from '../../core/model/scene-utils'
import * as PP from '../../core/shared/property-path'

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
    [PP.toString(PathForResizeContent)]: jsxAttributeValue(true),
    style: jsxAttributeValue({
      position: 'absolute',
      ...frame,
    }),
  }

  return jsxElement(jsxElementName('Scene', []), props, [])
}

export function defaultViewElement(uid: string): JSXElement {
  return jsxElement(
    jsxElementName('View', []),
    {
      style: jsxAttributeValue({
        backgroundColor: '#0091FFAA',
        position: 'absolute',
      }),
      'data-uid': jsxAttributeValue(uid),
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

export function defaultTransparentViewElement(uid: string): JSXElement {
  return jsxElement(
    jsxElementName('View', []),
    {
      style: jsxAttributeValue({
        position: 'absolute',
      }),
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
        position: 'absolute',
      }),
      'data-uid': jsxAttributeValue(uid),
    },
    [],
  )
}
