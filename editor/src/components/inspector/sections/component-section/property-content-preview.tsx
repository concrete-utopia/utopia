import React from 'react'
import sanitizeHtml from 'sanitize-html'
import { isImage } from '../../../../core/shared/utils'
export const ImagePreviewTestId = 'image-preview'

interface ContentPreviewProps {
  text: string
}
export const ContentPreview = React.memo(({ text }: ContentPreviewProps) => {
  if (isImage(text)) {
    return <ImagePreview url={text} />
  }
  // maybe later we can check more about whether this is an html or not
  return <HtmlPreview html={text} />
})
ContentPreview.displayName = 'ContentPreview'

interface ImagePreviewProps {
  url: string
}
const ImagePreview = React.memo(({ url }: ImagePreviewProps) => {
  const [imageCanBeLoaded, setImageCanBeLoaded] = React.useState(true)

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

const HtmlPreview = React.memo(({ html }: HtmlPreviewProps) => {
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
