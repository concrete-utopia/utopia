import {
  FlexAlignment,
  FlexElementProps,
  FlexLength,
  FlexStretch,
  FramePoint,
  getUnstretchedWidthHeight,
  LayoutSystem,
  UtopiaComponentProps,
} from 'utopia-api'
import { FullFrame } from '../../components/frame'
import {
  applicative2Either,
  defaultEither,
  Either,
  flatMapEither,
  foldEither,
  isRight,
  joinEither,
  left,
  mapEither,
  reduceWithEither,
  right,
  eitherToMaybe,
  eitherFromMaybe,
} from '../shared/either'
import {
  ElementInstanceMetadata,
  isJSXElement,
  JSXAttributes,
  jsxAttributeValue,
  JSXElement,
  UtopiaJSXComponent,
  ElementInstanceMetadataMap,
} from '../shared/element-template'
import {
  setJSXValueAtPath,
  setJSXValuesAtPaths,
  unsetJSXValuesAtPaths,
  ValueAtPath,
} from '../shared/jsx-attributes'
import { Imports, NodeModules, PropertyPath, ElementPath } from '../shared/project-file-types'
import { createLayoutPropertyPath, pinnedPropForFramePoint } from './layout-helpers-new'
import { getLayoutProperty, getLayoutPropertyOr } from './getLayoutProperty'
import { PropsOrJSXAttributes, getSimpleAttributeAtPath } from '../model/element-metadata-utils'
import { EdgePosition } from '../../components/canvas/canvas-types'
import { getPropertyControlsForTarget } from '../property-controls/property-controls-utils'
import { PropertyControlsInfo } from '../../components/custom-code/code-file'
import { emptyComments } from '../workers/parser-printer/parser-printer-comments'
import { ProjectContentTreeRoot } from '../../components/assets'

export function targetRespectsLayout(
  target: ElementPath,
  propertyControlsInfo: PropertyControlsInfo,
  openFilePath: string | null,
  projectContents: ProjectContentTreeRoot,
  nodeModules: NodeModules,
): boolean {
  const propControls = getPropertyControlsForTarget(
    target,
    propertyControlsInfo,
    openFilePath,
    projectContents,
    nodeModules,
  )
  return propControls?.style?.type === 'styleobject'
}

export const PinLayoutHelpers = {
  setLayoutPropsToPinsWithFrame(
    props: JSXAttributes,
    frame: Partial<FullFrame>,
  ): Either<string, JSXAttributes> {
    return reduceWithEither(
      (workingProps, frameProp: FramePoint) => {
        return setJSXValueAtPath(
          workingProps,
          createLayoutPropertyPath(pinnedPropForFramePoint(frameProp)),
          jsxAttributeValue(frame[frameProp], emptyComments),
        )
      },
      props,
      Object.keys(frame) as FramePoint[],
    )
  },
  updateLayoutPropsWithFrame(
    props: JSXAttributes,
    frame: Partial<FullFrame>,
  ): Either<string, JSXAttributes> {
    // like setLayoutPropsToPinsWithFrame, but preserves the original layout props
    return reduceWithEither(
      (updatedAttributes, frameProp) => {
        return setJSXValueAtPath(
          updatedAttributes,
          createLayoutPropertyPath(pinnedPropForFramePoint(frameProp as FramePoint)),
          jsxAttributeValue(frame[frameProp], emptyComments),
        )
      },
      props,
      Object.keys(frame) as Array<keyof Partial<FullFrame>>,
    )
  },
}

export type FixedAndBases = {
  flexBasis: number | null
  width: number | null
  height: number | null
}

export type TopLeftWidthHeight = Pick<FullFrame, 'top' | 'left' | 'width' | 'height'>

export const FlexBasisPath = createLayoutPropertyPath('flexBasis')

export const FlexLayoutHelpers = {
  updateLayoutPropsToFlexWithFrame(
    parentProps: PropsOrJSXAttributes,
    elementProps: JSXAttributes,
    width: number,
    height: number,
  ): Either<string, JSXAttributes> {
    // Remove all of the properties so that old values don't mess with this.
    const withoutLayoutProps = unsetJSXValuesAtPaths(elementProps, [FlexBasisPath])

    return joinEither(
      applicative2Either(
        (props, { flexBasis, width: widthToSet, height: heightToSet }) => {
          // Build the new properties.
          let propsToSet: Array<ValueAtPath> = []
          function addPropToSet(path: PropertyPath, value: string | number | undefined): void {
            if (value != null) {
              propsToSet.push({ path: path, value: jsxAttributeValue(value, emptyComments) })
            }
          }
          if (flexBasis != null) {
            addPropToSet(FlexBasisPath, flexBasis)
          }
          if (widthToSet != null) {
            addPropToSet(createLayoutPropertyPath('Width'), widthToSet)
          }
          if (heightToSet != null) {
            addPropToSet(createLayoutPropertyPath('Height'), heightToSet)
          }

          // Assign the new properties
          return setJSXValuesAtPaths(props, propsToSet)
        },
        withoutLayoutProps,
        FlexLayoutHelpers.convertWidthHeightToFlex(
          width,
          height,
          elementProps,
          parentProps,
          eitherToMaybe(FlexLayoutHelpers.getMainAxis(parentProps)),
          null,
        ),
      ),
    )
  },
  getFlexDirectionFromProps(
    props: JSXAttributes,
  ): 'row' | 'row-reverse' | 'column' | 'column-reverse' {
    return getLayoutPropertyOr(
      'row', // TODO read this value from spy
      'flexDirection',
      right(props),
    )
  },
  getFlexWrap: function (props: JSXAttributes): 'wrap' | 'wrap-reverse' | 'nowrap' {
    return getLayoutPropertyOr(
      'nowrap', // TODO read this value from spy
      'flexWrap',
      right(props),
    )
  },
  setTopLeftAttributes: (pos: { top: number; left: number }) => (props: JSXAttributes) => {
    return reduceWithEither(
      (workingProps, propToSet) => {
        return setJSXValueAtPath(
          workingProps,
          createLayoutPropertyPath(propToSet),
          jsxAttributeValue(pos[propToSet], emptyComments),
        )
      },
      props,
      ['top', 'left'] as const,
    )
  },
  setWidthHeightAttributes: (size: { width: number; height: number }) => (props: JSXAttributes) => {
    return reduceWithEither(
      (workingProps, propToSet) => {
        return setJSXValueAtPath(
          workingProps,
          createLayoutPropertyPath(pinnedPropForFramePoint(propToSet)),
          jsxAttributeValue(size[propToSet], emptyComments),
        )
      },
      props,
      [FramePoint.Width, FramePoint.Height] as const,
    )
  },
  setFlexAlignSelf: (newValue: FlexAlignment) => (
    props: JSXAttributes,
  ): Either<string, JSXAttributes> => {
    return setJSXValueAtPath(
      props,
      createLayoutPropertyPath('alignSelf'),
      jsxAttributeValue(newValue, emptyComments),
    )
  },
  getMainAxis(props: PropsOrJSXAttributes): Either<string, 'horizontal' | 'vertical'> {
    return mapEither((fd) => {
      if (fd === 'column' || fd === 'column-reverse') {
        return 'vertical'
      } else {
        return 'horizontal'
      }
    }, getSimpleAttributeAtPath(props, createLayoutPropertyPath('flexDirection')))
  },
  getCrossAxis(props: PropsOrJSXAttributes): Either<string, 'horizontal' | 'vertical'> {
    return mapEither(
      (fd) => (fd === 'vertical' ? 'horizontal' : 'vertical'),
      this.getMainAxis(props),
    )
  },
  convertWidthHeightToFlex: (
    width: number,
    height: number,
    props: JSXAttributes,
    parentProps: PropsOrJSXAttributes,
    possibleMainAxis: 'horizontal' | 'vertical' | null,
    edgePosition: EdgePosition | null,
  ): Either<string, FixedAndBases> => {
    const currentFlexBasis = eitherToMaybe(getSimpleAttributeAtPath(right(props), FlexBasisPath))
    const currentWidth = eitherToMaybe(
      getSimpleAttributeAtPath(right(props), createLayoutPropertyPath('Width')),
    )
    const currentHeight = eitherToMaybe(
      getSimpleAttributeAtPath(right(props), createLayoutPropertyPath('Height')),
    )
    return mapEither((mainAxis) => {
      const flexBasis = mainAxis === 'horizontal' ? width : height
      const crossBasis = mainAxis === 'vertical' ? height : width
      let shouldSetFlexBasis = true
      let shouldSetCrossBasis = true
      if (edgePosition != null) {
        shouldSetFlexBasis =
          mainAxis === 'horizontal' ? edgePosition.y === 0.5 : edgePosition.x === 0.5
        shouldSetCrossBasis =
          mainAxis === 'vertical' ? edgePosition.y === 0.5 : edgePosition.x === 0.5
      }
      return {
        flexBasis: currentFlexBasis != null || shouldSetFlexBasis ? flexBasis : null,
        width:
          mainAxis === 'vertical' && (currentWidth != null || shouldSetCrossBasis) ? width : null,
        height:
          mainAxis === 'horizontal' && (currentHeight != null || shouldSetCrossBasis)
            ? height
            : null,
      }
    }, eitherFromMaybe('no main axis provided', possibleMainAxis))
  },
  getUnstretchedWidthHeight: (
    props: UtopiaComponentProps,
    parentProps: UtopiaComponentProps,
  ): { width: FlexLength; height: FlexLength } => {
    return getUnstretchedWidthHeight((props.style as any) ?? {}, (parentProps.style as any) ?? {})
  },
}

export const LayoutHelpers = {
  updateLayoutPropsWithFrame(
    parentIsFlex: boolean,
    parentProps: PropsOrJSXAttributes | null,
    elementProps: JSXAttributes,
    frame: TopLeftWidthHeight,
  ): Either<string, JSXAttributes> {
    if (parentIsFlex && parentProps != null) {
      return FlexLayoutHelpers.updateLayoutPropsToFlexWithFrame(
        parentProps,
        elementProps,
        frame.width,
        frame.height,
      )
    } else {
      return PinLayoutHelpers.updateLayoutPropsWithFrame(elementProps, frame)
    }
  },
  getLayoutSystemFromAttributes(props: PropsOrJSXAttributes): Either<string, LayoutSystem> {
    return getSimpleAttributeAtPath(props, createLayoutPropertyPath('LayoutSystem')) // TODO LAYOUT investigate if we should use the DOM walker results here
  },
  getLayoutSystemFromProps(props: PropsOrJSXAttributes): LayoutSystem {
    const parsedLayoutSystem = this.getLayoutSystemFromAttributes(props)
    return isRight(parsedLayoutSystem) ? parsedLayoutSystem.value : LayoutSystem.PinSystem
  },
  stretchesChild(
    parentProps: PropsOrJSXAttributes,
    childProps: JSXAttributes,
  ): Either<string, boolean> {
    return applicative2Either(
      (alignSelf, alignItems) => {
        const childrenStretchesSelf = alignSelf === 'stretch'
        const childrenAlignsSelfAuto = alignSelf === 'auto' || alignSelf == null
        const parentStretchesChildren = alignItems === 'stretch'
        return childrenStretchesSelf || (parentStretchesChildren && childrenAlignsSelfAuto)
      },
      getLayoutProperty('alignSelf', right(childProps)),
      getLayoutProperty('alignItems', parentProps),
    )
  },
  getFlexStretchForChild(
    parent: ElementInstanceMetadata,
    child: ElementInstanceMetadata,
  ): Either<string, FlexStretch> {
    const parentScenePropsOrElementAttributes: PropsOrJSXAttributes | null = foldEither(
      (_) => null,
      (success) => {
        if (isJSXElement(success)) {
          return right(success.props)
        } else {
          return null
        }
      },
      parent.element,
    )
    if (parentScenePropsOrElementAttributes == null) {
      return left('Unknown parent attributes.')
    } else {
      if (isRight(child.element) && isJSXElement(child.element.value)) {
        const childJSXElement: JSXElement = child.element.value
        return flatMapEither((stretched) => {
          if (stretched) {
            return FlexLayoutHelpers.getCrossAxis(parentScenePropsOrElementAttributes)
          } else {
            return right('none')
          }
        }, this.stretchesChild(parentScenePropsOrElementAttributes, childJSXElement.props))
      } else {
        return left('Unknown child attributes.')
      }
    }
  },
  getElementSizePropertyPaths(
    element: ElementInstanceMetadata,
  ): {
    horizontal: PropertyPath
    vertical: PropertyPath
  } {
    if (element.specialSizeMeasurements.parentLayoutSystem === 'flex') {
      if (element.specialSizeMeasurements.parentFlexDirection === 'row') {
        return {
          horizontal: createLayoutPropertyPath('flexBasis'),
          vertical: createLayoutPropertyPath('Height'),
        }
      } else {
        return {
          horizontal: createLayoutPropertyPath('Width'),
          vertical: createLayoutPropertyPath('flexBasis'),
        }
      }
    } else {
      return {
        horizontal: createLayoutPropertyPath('Width'),
        vertical: createLayoutPropertyPath('Height'),
      }
    }
  },
}
