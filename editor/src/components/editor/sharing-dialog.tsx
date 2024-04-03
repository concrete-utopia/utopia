import { AnimatePresence, motion } from 'framer-motion'
import React from 'react'
import { getProjectID } from '../../common/env-vars'
import { Icons, useColorTheme, UtopiaStyles } from '../../uuiui'
import { GithubSpinner } from '../navigator/left-pane/github-pane/github-spinner'
import { useDispatch } from './store/dispatch-context'
import { useEditorState, Substores } from './store/store-hook'
import { when } from '../../utils/react-conditionals'
import { setSharingDialogOpen } from './actions/action-creators'

const BaseIframeWidth = 270
const BaseIframeHeight = 100
const TopBarHeight = 38

export const SharingDialog = React.memo(() => {
  const dispatch = useDispatch()
  const colorTheme = useColorTheme()

  const projectId = getProjectID()

  const sharingDialogOpen = useEditorState(
    Substores.restOfEditor,
    (store) => store.editor.sharingDialogOpen,
    'SharingDialog sharingDialogOpen',
  )

  const [iframeLoaded, setIframeLoaded] = React.useState(false)
  const [iframeWidth, setIframeWidth] = React.useState(BaseIframeWidth)
  const [iframeHeight, setIframeHeight] = React.useState(BaseIframeHeight)

  const sharingIframeRef = React.useRef<HTMLIFrameElement | null>(null)

  const onSharingIframeLoaded = React.useCallback(() => {
    setIframeLoaded(true)
    if (sharingIframeRef.current != null && sharingIframeRef.current.contentWindow != null) {
      const body = sharingIframeRef.current.contentWindow.document.body

      const width = Math.max(body.scrollWidth) + 20 // add horizontal padding which is getting chomped
      const height = Math.max(body.scrollHeight) + TopBarHeight + 20 // add vertical padding

      setIframeWidth(width)
      setIframeHeight(height)
    }
  }, [])

  const handleDismiss = React.useCallback(() => {
    dispatch([setSharingDialogOpen(false)])
    setIframeLoaded(false)
    setIframeWidth(BaseIframeWidth)
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
      }}
      onClick={handleDismiss}
    >
      <AnimatePresence>
        {when(
          sharingDialogOpen,
          <motion.div
            initial={{ opacity: 0, width: iframeWidth, height: iframeHeight }}
            animate={{
              opacity: 1,
              width: iframeWidth,
              height: iframeHeight,
              transition: { duration: 0.1 },
            }}
            exit={{ opacity: 0, transition: { duration: 0.1 } }}
            onLoad={onSharingIframeLoaded}
            style={{
              background: colorTheme.bg0.value,
              border: `1px solid ${colorTheme.primary30.value}`,
              boxShadow: UtopiaStyles.popup.boxShadow,
              borderRadius: 10,
              position: 'absolute',
              top: 8,
              right: 8,
            }}
            onClick={stopPropagation}
          >
            <div
              style={{
                display: 'flex',
                alignItems: 'center',
                justifyContent: 'flex-end',
                padding: '7px 8px',
                height: TopBarHeight,
              }}
            >
              <div
                onClick={handleDismiss}
                style={{
                  display: 'flex',
                  alignItems: 'center',
                  background: colorTheme.primary10.value,
                  borderRadius: 24,
                  height: 24,
                  border: `1px solid transparent`,
                  width: 71,
                  padding: 2,
                  gap: 4,
                }}
              >
                <div
                  style={{
                    width: 21,
                    height: 21,
                    display: 'flex',
                    alignItems: 'center',
                    justifyContent: 'center',
                  }}
                >
                  <Icons.Cross />
                </div>
                <div style={{ flex: 1, fontWeight: 500 }}>Close</div>
              </div>
            </div>
            {when(
              !iframeLoaded,
              <div
                style={{
                  background: colorTheme.bg0.value,
                  top: 0,
                  left: 0,
                  bottom: 0,
                  right: 0,
                  borderRadius: 10,
                  display: 'flex',
                  alignItems: 'center',
                  justifyContent: 'center',
                  position: 'absolute',
                  color: colorTheme.fg0.value,
                  fontStyle: 'italic',
                  flexDirection: 'column',
                  gap: 10,
                }}
              >
                <GithubSpinner stroke='#000' />
                Loading share details…
              </div>,
            )}
            <motion.iframe
              ref={sharingIframeRef}
              style={{
                background: colorTheme.bg0.value,
                borderRadius: 10,
                border: 'none',
                opacity: iframeLoaded ? 1 : 0,
                boxSizing: 'border-box',
                width: '100%',
                height: `calc(100% - ${TopBarHeight}px)`,
              }}
              src={`http://localhost:8000/iframe/project/${projectId}/sharing`}
            />
          </motion.div>,
        )}
      </AnimatePresence>
    </div>
  )
})
SharingDialog.displayName = 'SharingDialog'
