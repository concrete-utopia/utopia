import { BASE_URL } from '../common/env-vars'
import {
  ElementInstanceMetadata,
  emptyComments,
  jsxAttributesFromMap,
  jsxAttributeValue,
  JSXElement,
  jsxElement,
  jsxElementName,
  setJSXAttributesAttribute,
} from '../core/shared/element-template'
import { isImageFile } from '../core/model/project-file-utils'
import { ProjectContents, ElementPath } from '../core/shared/project-file-types'
import Utils from '../utils/utils'
import {
  Size,
  CanvasRectangle,
  CanvasPoint,
  canvasRectangle,
  resizeCanvasRectangle,
} from '../core/shared/math-utils'
import { EditorAction } from './editor/action-types'
import { insertJSXElement } from './editor/actions/action-creators'
import { forceNotNull } from '../core/shared/optional-utils'
import { AllElementProps } from './editor/store/editor-state'
import * as EP from '../core/shared/element-path'
import { isFeatureEnabled } from '../utils/feature-switches'

export function getImageSrc(
  projectId: string | null,
  projectContents: ProjectContents,
  imageId: string,
): string | null {
  const image = projectContents[imageId]
  if (image == null) {
    return null
  } else {
    if (isImageFile(image)) {
      if (image.base64 != null && image.base64 !== '') {
        const dataType =
          image.type == null || image.type.endsWith('.svg') ? 'image/svg+xml' : 'text/plain'
        return `data:${dataType};base64,${image.base64}`
      } else if (projectId != null) {
        return encodeURI(`${BASE_URL}project/${projectId}/${imageId}`)
      } else {
        console.error(
          `Unabled to find image source as project ID is null: ${JSON.stringify(image)}`,
        )
        return null
      }
    } else {
      throw new Error(`Invalid image for ${imageId}: ${JSON.stringify(image)}`)
    }
  }
}

export function parseImageMultiplier(imagePath: string): number {
  const imageMultiplierRegex = /.*@(\d*)x\..*/
  const imageMultiplierResult = imageMultiplierRegex.exec(imagePath)
  let multiplier: number = 1
  if (imageMultiplierResult != null && imageMultiplierResult.length === 2) {
    multiplier = Number.parseInt(imageMultiplierResult[1])
  }
  return multiplier
}

interface FrameAndMultiplier {
  multiplier: number
  frame: CanvasRectangle
}

export function getFrameAndMultiplier(
  centerPoint: CanvasPoint,
  filename: string,
  size: Size,
  overrideDefaultMultiplier: number | null,
): FrameAndMultiplier {
  const multiplier =
    overrideDefaultMultiplier == null ? parseImageMultiplier(filename) : overrideDefaultMultiplier
  const scaledSize = scaleImageDimensions(size, multiplier)
  const frame: CanvasRectangle = canvasRectangle({
    x: centerPoint.x - scaledSize.width / 2,
    y: centerPoint.y - scaledSize.height / 2,
    width: scaledSize.width,
    height: scaledSize.height,
  })

  return {
    multiplier: multiplier,
    frame: frame,
  }
}

export function getFrameAndMultiplierWithResize(
  centerPoint: CanvasPoint,
  filename: string,
  size: Size,
  scale: number,
): FrameAndMultiplier {
  const { frame, multiplier } = getFrameAndMultiplier(centerPoint, filename, size, null)
  const [defaultWidth, defaultHeight] = [640 / scale, 640 / scale]
  const adjustedFrame = isFeatureEnabled('Resize image on drop')
    ? resizeCanvasRectangle(frame, {
        centerPoint: centerPoint,
        keepAspectRatio: true,
        desiredHeight: Math.min(frame.height, defaultWidth),
        desiredWidth: Math.min(frame.width, defaultHeight),
      })
    : frame

  return { frame: adjustedFrame, multiplier: multiplier }
}

export function createInsertImageAction(
  projectContents: ProjectContents,
  centerPoint: CanvasPoint,
  imagePath: string,
  parentPath: ElementPath,
  newUID: string,
): EditorAction {
  const imageDetails = forceNotNull(
    `Unable to find asset ${imagePath}.`,
    projectContents[imagePath],
  )
  if (imageDetails.type === 'IMAGE_FILE') {
    const srcAttribute = jsxAttributeValue(`.${imagePath}`, emptyComments)
    const width = imageDetails.width ?? 100
    const height = imageDetails.height ?? 100
    const { frame } = getFrameAndMultiplier(
      centerPoint,
      imagePath,
      { width: width, height: height },
      null,
    )

    const imageElement = jsxElement(
      jsxElementName('img', []),
      newUID,
      jsxAttributesFromMap({
        alt: jsxAttributeValue('', emptyComments),
        src: srcAttribute,
        style: jsxAttributeValue(
          {
            left: frame.x,
            top: frame.y,
            width: frame.width,
            height: frame.height,
          },
          emptyComments,
        ),
        'data-uid': jsxAttributeValue(newUID, emptyComments),
      }),
      [],
    )
    return insertJSXElement(imageElement, parentPath, {})
  } else {
    throw new Error(`Attempting to insert ${imagePath} as an image when it is not stored as such.`)
  }
}

export function getImageSizeFromProps(props: any): Size {
  const width = Utils.pathOr(1, ['imageSize', 'width'], props)
  const height = Utils.pathOr(1, ['imageSize', 'height'], props)
  return {
    width: width,
    height: height,
  }
}

export function getImageSize(
  allElementProps: AllElementProps,
  component: ElementInstanceMetadata,
): Size {
  return getImageSizeFromProps(allElementProps[EP.toString(component.elementPath)])
}

export function getImageSizeFromMetadata(
  allElementProps: AllElementProps,
  instance: ElementInstanceMetadata,
): Size {
  return getImageSizeFromProps(allElementProps[EP.toString(instance.elementPath)])
}

export function getImageSizeMultiplierFromProps(props: any): number {
  return Utils.pathOr(1, ['imageSize', 'multiplier'], props)
}

export function scaleImageDimensions(size: Size, multiplier: number): Size {
  return {
    width: size.width / multiplier,
    height: size.height / multiplier,
  }
}

export function getScaledImageDimensionsFromProps(props: any): Size {
  const imageSizeMultiplier = getImageSizeMultiplierFromProps(props)
  const imageSize = getImageSizeFromProps(props)
  return scaleImageDimensions(imageSize, imageSizeMultiplier)
}

export interface JSXImageOptions {
  width: number
  height: number
  top: number
  left: number
  src: string
}

export function createJsxImage(uid: string, options: Partial<JSXImageOptions>): JSXElement {
  const propsForElement = jsxAttributesFromMap({
    src: jsxAttributeValue(options.src, emptyComments),
    style: jsxAttributeValue(
      {
        position: 'absolute',
        width: options.width,
        height: options.height,
        top: options.top,
        left: options.left,
      },
      emptyComments,
    ),
  })

  return jsxElement(
    'img',
    uid,
    setJSXAttributesAttribute(propsForElement, 'data-uid', jsxAttributeValue(uid, emptyComments)),
    [],
  )
}

export const MultipliersForImages: Array<number> = [1, 2]
