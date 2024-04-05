import { motion } from 'framer-motion'
import React from 'react'
import { getProjectID } from '../../common/env-vars'
import { Button, Icons, useColorTheme, UtopiaStyles } from '../../uuiui'
import { GithubSpinner } from '../navigator/left-pane/github-pane/github-spinner'
import { useDispatch } from './store/dispatch-context'
import { useEditorState, Substores } from './store/store-hook'
import { unless, when } from '../../utils/react-conditionals'
import { setSharingDialogOpen } from './actions/action-creators'
import { editorShareDialogIframeUrl } from '../../common/server'

const BaseIframeHeight = 80
const LoadedIframeHeight = 300
const RadixDialogSpacing = 135
const SharingIframeWidth = 580 // px
const LoadingDialogWidth = 260 // px

export const SharingDialog = React.memo(() => {
  const dispatch = useDispatch()
  const colorTheme = useColorTheme()

  const projectId = getProjectID()

  const sharingDialogOpen: boolean = useEditorState(
    Substores.restOfEditor,
    (store) => store.editor.sharingDialogOpen,
    'SharingDialog sharingDialogOpen',
  )

  const [iframeLoaded, setIframeLoaded] = React.useState(false)
  const [iframeHeight, setIframeHeight] = React.useState(BaseIframeHeight)

  const sharingIframeRef = React.useRef<HTMLIFrameElement | null>(null)

  const onSharingIframeLoaded = React.useCallback(() => {
    setIframeLoaded(true)
    setIframeHeight(LoadedIframeHeight + RadixDialogSpacing)
  }, [])

  const handleDismiss = React.useCallback(() => {
    dispatch([setSharingDialogOpen(false)])
    setIframeLoaded(false)
    setIframeHeight(BaseIframeHeight)
  }, [dispatch])

  const stopPropagation = React.useCallback((e: React.MouseEvent) => {
    e.stopPropagation()
  }, [])

  if (projectId == null) {
    return null
  }

  return (
    <div
      style={{
        position: 'fixed',
        top: 0,
        left: 0,
        bottom: 0,
        right: 0,
        pointerEvents: !sharingDialogOpen ? 'none' : 'all',
        display: 'flex',
        alignItems: 'center',
        justifyContent: 'center',
        backgroundColor: !sharingDialogOpen ? 'transparent' : '#00000033',
      }}
      onClick={handleDismiss}
    >
      {when(
        sharingDialogOpen,
        <motion.div
          initial={{ opacity: 0, height: BaseIframeHeight }}
          animate={{ opacity: 1, transition: { duration: 0.1 }, height: iframeHeight }}
          exit={{ opacity: 0, transition: { duration: 0.1 } }}
          style={{
            background: colorTheme.bg0.value,
            boxShadow: UtopiaStyles.popup.boxShadow,
            borderRadius: 10,
            width: '100%',
            maxWidth: iframeLoaded ? SharingIframeWidth : LoadingDialogWidth,
            position: 'relative',
            display: 'flex',
            alignItems: 'center',
            justifyContent: 'center',
          }}
          onClick={stopPropagation}
        >
          {when(
            iframeLoaded,
            <Button
              highlight
              style={{
                position: 'absolute',
                top: 14,
                right: 14,
                width: 22,
                height: 22,
              }}
              onClick={handleDismiss}
            >
              <Icons.Cross />
            </Button>,
          )}
          <iframe
            ref={sharingIframeRef}
            onLoad={onSharingIframeLoaded}
            style={{
              background: colorTheme.bg0.value,
              border: 'none',
              opacity: iframeLoaded ? 1 : 0,
              width: '100%',
              height: '100%',
              borderRadius: 10,
            }}
            src={editorShareDialogIframeUrl(projectId)}
          />
          {unless(
            iframeLoaded,
            <div
              style={{
                fontStyle: 'italic',
                position: 'absolute',
                top: 0,
                left: 0,
                right: 0,
                bottom: 0,
                display: 'flex',
                alignItems: 'center',
                justifyContent: 'center',
                borderRadius: 10,
                gap: 6,
              }}
            >
              <GithubSpinner stroke='#000' />
              Loading share details…
            </div>,
          )}
        </motion.div>,
      )}
    </div>
  )
})
SharingDialog.displayName = 'SharingDialog'
