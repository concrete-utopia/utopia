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
import { omit } from '../../core/shared/object-utils'
import { fastForEach } from '../../core/shared/utils'

export function defaultSceneElement(
  uid: string,
  componentName: string | null,
  frame: NormalisedFrame,
  label: string,
): JSXElement {
  const component = componentName == null ? 'null' : componentName
  const definedElsewhere = componentName == null ? [] : [componentName]
  const props = {
    'data-uid': jsxAttributeValue(uid),
    'data-label': jsxAttributeValue(label),
    component: jsxAttributeOtherJavaScript(
      component,
      `return ${componentName}`,
      definedElsewhere,
      null,
    ),
    [PP.toString(PathForResizeContent)]: jsxAttributeValue(true),
    style: jsxAttributeValue({
      position: 'absolute',
      ...frame,
    }),
  }

  return jsxElement(jsxElementName('Scene', []), props, [])
}

export function isolatedComponentSceneElement(
  componentName: string,
  frame: NormalisedFrame,
  componentProps: any,
): JSXElement {
  const definedElsewhere = [componentName]
  let componentPropsToUse: any = {}
  fastForEach(Object.keys(componentProps), (key) => {
    if (!['data-uid', 'skipDeepFreeze'].includes(key)) {
      if (key === 'style') {
        const styleToUse = omit(['left', 'top', 'right', 'bottom'], componentProps[key])
        componentPropsToUse[key] = styleToUse
      } else {
        componentPropsToUse[key] = componentProps[key]
      }
    }
  })

  const props = {
    'data-uid': jsxAttributeValue('TRANSIENT_SCENE'),
    'data-label': jsxAttributeValue(`Isolated ${componentName}`),
    component: jsxAttributeOtherJavaScript(
      componentName,
      `return ${componentName}`,
      definedElsewhere,
      null,
      'TRANSIENT_SCENE_COMPONENT',
    ),
    props: jsxAttributeValue(componentPropsToUse),
    [PP.toString(PathForResizeContent)]: jsxAttributeValue(true),
    style: jsxAttributeValue({
      position: 'absolute',
      boxShadow: '0px 0px 6px 4px rgb(0, 0, 0, 0.29)',
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
