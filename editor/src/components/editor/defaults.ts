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

  return jsxElement(jsxElementName('Scene', []), props, [], null)
}

export function defaultViewElement(uid: string): JSXElement {
  return jsxElement(
    jsxElementName('View', []),
    {
      style: jsxAttributeValue({
        position: 'absolute',
        backgroundColor: '#0091FFAA',
      }),
      'data-uid': jsxAttributeValue(uid),
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

export function defaultTransparentViewElement(
  uid: string,
  layoutSystem: LayoutSystem | null,
): JSXElement {
  if (layoutSystem != null) {
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
  } else {
    return jsxElement(
      jsxElementName('View', []),
      {
        style: jsxAttributeValue({
          position: 'absolute',
        }),
        'data-uid': jsxAttributeValue(uid),
      },
      [],
      null,
    )
  }
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

export function defaultDivElement(uid: string): JSXElement {
  return jsxElement(
    jsxElementName('div', []),
    {
      style: jsxAttributeValue({
        position: 'absolute',
        backgroundColor: '#0091FFAA',
      }),
      'data-uid': jsxAttributeValue(uid),
    },
    [],
    null,
  )
}

export function defaultInsertableDivElement(uid: string): JSXElement {
  return jsxElement(
    jsxElementName('div', []),
    {
      style: jsxAttributeValue({
        backgroundColor: '#0091FFAA',
        width: 50,
        height: 50,
      }),
      'data-uid': jsxAttributeValue(uid),
    },
    [],
    null,
  )
}
