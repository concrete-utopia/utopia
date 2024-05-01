import { BASE_URL } from '../common/env-vars'
import type { ElementInstanceMetadata, JSXElement } from '../core/shared/element-template'
import {
  emptyComments,
  jsxAttributesFromMap,
  jsExpressionValue,
  jsxElement,
  jsxElementName,
  setJSXAttributesAttribute,
} from '../core/shared/element-template'
import type { ProjectContents, ElementPath } from '../core/shared/project-file-types'
import { isImageFile } from '../core/shared/project-file-types'
import Utils from '../utils/utils'
import type { Size, CanvasRectangle, CanvasPoint } from '../core/shared/math-utils'
import { canvasRectangle } from '../core/shared/math-utils'
import type { EditorAction } from './editor/action-types'
import { insertJSXElement } from './editor/actions/action-creators'
import { forceNotNull, optionalMap } from '../core/shared/optional-utils'
import type { AllElementProps } from './editor/store/editor-state'
import * as EP from '../core/shared/element-path'
import { identity } from '../core/shared/utils'

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

export interface FilenameParts {
  filename: string
  extension: string
  multiplier?: number
  deduplicationSeqNumber?: number
}

export function filenameFromParts(parts: FilenameParts): string {
  const { filename, multiplier, extension, deduplicationSeqNumber } = parts
  const multiplierString = optionalMap((m) => `@${m}x`, multiplier) ?? ''
  const dedupeString = optionalMap((n) => `_${n}`, deduplicationSeqNumber) ?? ''
  return `${filename}${dedupeString}${multiplierString}.${extension}`
}

export interface LastPartSeparatedByResult<T> {
  part: T
  rest: string
}

interface LastPartSeparatedByParams<T> {
  separator: string
  raw: string
  make: (_: string) => T | null
}

function lastPartSeparatedBy<T>(
  params: LastPartSeparatedByParams<T>,
): LastPartSeparatedByResult<T> | null {
  const { separator, make, raw } = params
  const parts = raw.split(separator)
  if (raw.length < 2) {
    return null
  }

  const made = make(parts[parts.length - 1])
  if (made == null) {
    return null
  }

  return {
    rest: parts.slice(0, -1).join(separator),
    part: made,
  }
}

const parseNumber =
  (re: RegExp) =>
  (raw: string): number | null => {
    const match = re.exec(raw)
    if (match == null || match.length < 2) {
      return null
    }
    return Utils.safeParseInt(match[1])
  }

export const parseMultiplier = parseNumber(/^(\d+)x$/)
export const parseDedupeId = parseNumber(/^(\d+)$/)

export function getFilenameParts(filename: string): FilenameParts | null {
  const extensionResult = lastPartSeparatedBy<string>({
    separator: '.',
    make: identity,
    raw: filename,
  })
  if (extensionResult == null) {
    return null
  }

  const { part: extension, rest: restFromExtension } = extensionResult

  const { part: multiplier, rest: restFromMultiplier } = lastPartSeparatedBy<number | undefined>({
    separator: '@',
    make: parseMultiplier,
    raw: restFromExtension,
  }) ?? { rest: restFromExtension }

  const { part: dedupSeqNumber, rest: restFromDedupe } = lastPartSeparatedBy<number | undefined>({
    separator: '_',
    make: parseDedupeId,
    raw: restFromMultiplier,
  }) ?? { rest: restFromMultiplier }

  return {
    extension: extension,
    multiplier: multiplier,
    deduplicationSeqNumber: dedupSeqNumber,
    filename: restFromDedupe,
  }
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
    overrideDefaultMultiplier == null
      ? getFilenameParts(filename)?.multiplier ?? 1
      : overrideDefaultMultiplier
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
    const srcAttribute = jsExpressionValue(`.${imagePath}`, emptyComments)
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
        alt: jsExpressionValue('', emptyComments),
        src: srcAttribute,
        style: jsExpressionValue(
          {
            left: frame.x,
            top: frame.y,
            width: frame.width,
            height: frame.height,
          },
          emptyComments,
        ),
        'data-uid': jsExpressionValue(newUID, emptyComments),
      }),
      [],
    )
    return insertJSXElement(imageElement, parentPath, {}, 'insert-as-child')
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
  opacity: number
  width: number
  height: number
  top: number
  left: number
  src: string
}

export function createJsxImage(uid: string, options: Partial<JSXImageOptions>): JSXElement {
  const propsForElement = jsxAttributesFromMap({
    'data-aspect-ratio-locked': jsExpressionValue(true, emptyComments),
    src: jsExpressionValue(options.src, emptyComments),
    style: jsExpressionValue(
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
    setJSXAttributesAttribute(propsForElement, 'data-uid', jsExpressionValue(uid, emptyComments)),
    [],
  )
}

export const MultipliersForImages: Array<number> = [1, 2]
