import React from 'react'
import { CanvasContextMenuPortalTargetID, NO_OP, assertNever } from '../../../../core/shared/utils'
import { InspectorModal } from '../../widgets/inspector-modal'
import { FlexColumn, FlexRow, UtopiaStyles, UtopiaTheme, useColorTheme } from '../../../../uuiui'

type SubmissionState = 'draft' | 'confirmed' | 'publishing'

interface DataEditorModalProps {
  closePopup: () => void
  style: React.CSSProperties
}

export const DataEditorModal = React.memo(
  React.forwardRef<HTMLDivElement, DataEditorModalProps>(({ closePopup, style }, forwardedRef) => {
    const catchClick = React.useCallback((e: React.MouseEvent) => {
      e.stopPropagation()
      e.preventDefault()
    }, [])

    const requestUpdate = React.useCallback(() => {}, [])

    const [submissionState, setSubmissionState] = React.useState<SubmissionState>('draft')

    const onMainButtonClick = React.useCallback(() => {
      switch (submissionState) {
        case 'draft':
          setSubmissionState('confirmed')
          break
        case 'confirmed':
          requestUpdate()
          break
        case 'publishing':
          break
        default:
          assertNever(submissionState)
      }
    }, [requestUpdate, submissionState])

    const [value, setValue] = React.useState<string>('')
    const updateValue = React.useCallback(
      (e: React.ChangeEvent<HTMLTextAreaElement>) => setValue(e.target.value),
      [],
    )

    const colorTheme = useColorTheme()

    const mainButtonText =
      submissionState === 'draft'
        ? 'Confirm'
        : submissionState === 'confirmed'
        ? 'Publish'
        : submissionState === 'publishing'
        ? 'Publishing'
        : assertNever(submissionState)

    const mainButtonColor =
      submissionState === 'draft' ? colorTheme.black.value : colorTheme.error.value

    return (
      <InspectorModal
        offsetX={20}
        offsetY={0}
        closePopup={closePopup}
        style={{
          zIndex: 1,
        }}
        closePopupOnUnmount={false}
        portalTarget={document.getElementById(CanvasContextMenuPortalTargetID) as HTMLElement}
        outsideClickIgnoreClass={'ignore-react-onclickoutside-data-picker'}
      >
        <div // this entire wrapper div was made before using the InspectorModal, so it should be re-done
          style={{
            background: 'transparent',
            position: 'fixed',
            top: 0,
            left: 0,
            right: 0,
            bottom: 0,
            zIndex: 1, // so it's above the inspector
          }}
          onClick={closePopup}
        >
          <FlexColumn
            ref={forwardedRef}
            onClick={catchClick}
            style={{
              minWidth: 550,
              height: 300,
              backgroundColor: colorTheme.inspectorBackground.value,
              color: colorTheme.fg1.value,
              overflow: 'hidden',
              borderRadius: UtopiaTheme.panelStyles.panelBorderRadius,
              boxShadow: UtopiaStyles.shadowStyles.highest.boxShadow,
              border: `1px solid ${colorTheme.fg0Opacity10.value}`,
              gap: 24,
              ...style,
            }}
          >
            <FlexRow style={{ padding: '24px 24px 0px 24px' }}>
              <span style={{ fontWeight: 700, fontSize: 18 }}>Editing content of</span>
              <span>label</span>
            </FlexRow>
            <div
              style={{
                display: 'grid',
                gap: 12,
                gridTemplateColumns: 'auto 1fr',
                gridTemplateRows: 'max-content',
                flex: 1,
                borderBottom: `1px solid ${colorTheme.subduedBorder.cssValue}`,
                padding: '8px 24px 0px 24px',
                fontSize: 12,
              }}
            >
              <span>Data</span>
              <span style={{ color: colorTheme.green.value }}>Gid</span>
              <span>Content</span>
              <textarea
                value={value}
                onChange={updateValue}
                style={{
                  resize: 'none',
                  outline: 'none',
                  border: `1px solid ${colorTheme.subduedBorder.cssValue}`,
                }}
              />
              <span>Source</span>
              <span style={{ color: colorTheme.green.value }}>Shopify: Meatobjects</span>
            </div>
            <FlexRow style={{ justifyContent: 'flex-end', gap: 8, padding: '0px 24px 24px 24px' }}>
              <div
                style={{
                  borderRadius: 4,
                  backgroundColor: colorTheme.white.value,
                  color: colorTheme.fg0.value,
                  border: `1px solid ${colorTheme.subduedBorder.value}`,
                  padding: 3,
                  fontSize: 11,
                  fontWeight: 400,
                  height: 24,
                  width: 81,
                  textAlign: 'center',
                }}
                onClick={closePopup}
              >
                Cancel
              </div>
              <div
                style={{
                  borderRadius: 4,
                  backgroundColor: mainButtonColor,
                  color: 'white',
                  padding: 3,
                  fontSize: 11,
                  fontWeight: 400,
                  height: 24,
                  width: 81,
                  textAlign: 'center',
                }}
                onClick={NO_OP}
              >
                {mainButtonText}
              </div>
            </FlexRow>
          </FlexColumn>
        </div>
      </InspectorModal>
    )
  }),
)
