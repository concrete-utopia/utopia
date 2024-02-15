import React from 'react'
import sanitizeHtml from 'sanitize-html'
import { isImage } from '../../../../core/shared/utils'
export const ImagePreviewTestId = 'image-preview'

interface ImagePreviewProps {
  url: string
}
export const ImagePreview = React.memo(({ url }: ImagePreviewProps) => {
  const [imageCanBeLoaded, setImageCanBeLoaded] = React.useState(isImage(url))

  // we need to track if the url has changed so we retry loading the image even if it failed before
  const urlRef = React.useRef<string>(url)
  if (urlRef.current !== url) {
    setImageCanBeLoaded(isImage(url))
    urlRef.current = url
  }

  // don't render the img when it can not be loaded
  const onImageError = React.useCallback(() => {
    setImageCanBeLoaded(false)
  }, [setImageCanBeLoaded])

  if (!imageCanBeLoaded) {
    return null
  }

  return (
    <img
      data-testid={ImagePreviewTestId}
      src={url}
      style={{ width: '100%' }}
      onError={onImageError}
    />
  )
})
ImagePreview.displayName = 'ImagePreview'

interface HtmlPreviewProps {
  html: string
}

export const HtmlPreview = React.memo(({ html }: HtmlPreviewProps) => {
  const sanitizedHtml = sanitizeHtml(html)
  return (
    <div
      style={{
        maxHeight: 100,
        overflow: 'hidden',
        padding: 5,
      }}
      dangerouslySetInnerHTML={{ __html: sanitizedHtml }}
    />
  )
})
HtmlPreview.displayName = 'HtmlPreview'
