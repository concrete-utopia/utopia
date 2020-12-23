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
import {emptyComments} from "../../core/workers/parser-printer/parser-printer-comments";

export function defaultSceneElement(
  uid: string,
  componentName: string | null,
  frame: NormalisedFrame,
  label: string,
): JSXElement {
  const component = componentName == null ? 'null' : componentName
  const props = {
    'data-uid': jsxAttributeValue(uid, emptyComments),
    'data-label': jsxAttributeValue(label, emptyComments),
    component: jsxAttributeOtherJavaScript(component, `return ${componentName}`, [], null),
    [PP.toString(PathForResizeContent)]: jsxAttributeValue(true, emptyComments),
    style: jsxAttributeValue({
        position: 'absolute',
        ...frame,
    }, emptyComments),
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
      }, emptyComments),
      'data-uid': jsxAttributeValue(uid, emptyComments),
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
      }, emptyComments),
      'data-uid': jsxAttributeValue(uid, emptyComments),
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
      }, emptyComments),
      'data-uid': jsxAttributeValue(uid, emptyComments),
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
      }, emptyComments),
      text: jsxAttributeValue('Text', emptyComments),
      'data-uid': jsxAttributeValue(uid, emptyComments),
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
      }, emptyComments),
      'data-uid': jsxAttributeValue(uid, emptyComments),
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
      }, emptyComments),
      'data-uid': jsxAttributeValue(uid, emptyComments),
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
      }, emptyComments),
      'data-uid': jsxAttributeValue(uid, emptyComments),
    },
    [],
  )
}
