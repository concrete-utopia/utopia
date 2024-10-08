import React from 'react'
import { CanvasContextMenuPortalTargetID, assertNever } from '../../../../core/shared/utils'
import { InspectorModal } from '../../widgets/inspector-modal'
import { FlexColumn, FlexRow, UtopiaStyles, UtopiaTheme, useColorTheme } from '../../../../uuiui'
import { unless } from '../../../../utils/react-conditionals'
import { HEADERS, MODE } from '../../../../common/server'
import urljoin from 'url-join'
import { RemixNavigationAtom } from '../../../canvas/remix/utopia-remix-root-component'
import { useRefAtom } from '../../../editor/hook-utils'
import { useDispatch } from '../../../editor/store/dispatch-context'
import { getProjectID } from '../../../../common/env-vars'
import { removeToast, showToast } from '../../../editor/actions/action-creators'
import { notice } from '../../../common/notice'
import { atom, useAtom } from 'jotai'
import type { EditorState } from '../../../editor/store/editor-state'
import { Substores, useEditorState } from '../../../editor/store/store-hook'
import { wait } from '../../../../core/model/performance-scripts'

interface DataEditModalContext {
  cartoucheComponent: React.ReactElement | null
  dataPath: string[] | null
}

export const DataEditModalContextAtom = atom<DataEditModalContext>({
  cartoucheComponent: null,
  dataPath: null,
})

type SubmissionState = 'draft' | 'confirmed' | 'publishing'

interface DataUpdateModalProps {
  closePopup: () => void
  style: React.CSSProperties
}

export const DataUpdateModal = React.memo(
  React.forwardRef<HTMLDivElement, DataUpdateModalProps>(({ closePopup, style }, forwardedRef) => {
    const catchClick = React.useCallback((e: React.MouseEvent) => {
      e.stopPropagation()
      e.preventDefault()
    }, [])

    const [{ cartoucheComponent, dataPath }] = useAtom(DataEditModalContextAtom)

    const allVariablesInScope = useEditorState(
      Substores.variablesInScope,
      (store) => store.editor.variablesInScope,
      'DataUpdateModal allVariablesInScope',
    )

    const remix = useRefAtom(RemixNavigationAtom)
    const revalidateAllLoaders = React.useCallback(() => {
      // TODO: only revalidate the router that points to the route being edited
      Object.values(remix.current).forEach((router) => router?.revalidate())
    }, [remix])

    const dispatch = useDispatch()

    const data = dataPath == null ? null : getReviewData(dataPath, allVariablesInScope)

    const [value, setValue] = React.useState<string>(data?.value ?? '')
    const updateValue = React.useCallback((e: React.ChangeEvent<HTMLTextAreaElement>) => {
      e.stopPropagation()
      e.preventDefault()
      setValue(e.target.value)
    }, [])

    const onKeyDown = React.useCallback((e: React.KeyboardEvent<HTMLTextAreaElement>) => {
      e.stopPropagation()
    }, [])

    const requestUpdate = React.useCallback(async () => {
      const projectId = getProjectID()
      if (projectId == null) {
        dispatch([
          showToast(
            notice('Cannot find project id', 'ERROR', false, 'DataUpdateModal-no-project-id'),
          ),
        ])
        return
      }

      const id = data?.gid
      const key = data?.metafield
      if (id == null || key == null) {
        dispatch([
          showToast(
            notice(
              'This metafield cannot be edited (yet)',
              'ERROR',
              false,
              'DataUpdateModal-metafield-cannot-be-edited',
            ),
          ),
        ])
        return
      }

      await updateData(projectId, {
        query: METAOBJECT_UPDATE_MUTATION,
        variables: {
          id: id,
          metaobject: { fields: [{ key, value }] },
        },
      })
      dispatch([
        showToast(
          notice(
            'Updating metaobject...',
            'INFO',
            false,
            'DataUpdateModal-metaobject-update-in-progress',
          ),
        ),
      ])
      await wait(10000) // adjust as needed
      revalidateAllLoaders()
      setSubmissionState('draft')
    }, [data?.gid, data?.metafield, dispatch, revalidateAllLoaders, value])

    const [submissionState, setSubmissionState] = React.useState<SubmissionState>('draft')

    const onMainButtonClick = React.useCallback(() => {
      switch (submissionState) {
        case 'draft':
          setSubmissionState('confirmed')
          break
        case 'confirmed':
          void requestUpdate()
          setSubmissionState('publishing')
          break
        case 'publishing':
          break
        default:
          assertNever(submissionState)
      }
    }, [requestUpdate, submissionState])

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
              width: 550,
              minHeight: 300,
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
            <FlexRow style={{ padding: '24px 24px 0px 24px', gap: 8, alignItems: 'center' }}>
              <span style={{ fontWeight: 700, fontSize: 18 }}>Editing content of</span>
              {cartoucheComponent}
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
              <span style={{ color: colorTheme.green.value }}>{data?.gid ?? ''}</span>
              <span>Content</span>
              <textarea
                value={value}
                onChange={updateValue}
                onKeyDown={onKeyDown}
                style={{
                  height: 80,
                  resize: 'none',
                  outline: 'none',
                  border: `1px solid ${colorTheme.subduedBorder.cssValue}`,
                }}
              />
              <span>Source</span>
              <span style={{ color: colorTheme.green.value }}>Shopify: Meatobjects</span>
            </div>
            {unless(
              submissionState === 'draft',
              <FlexRow
                style={{
                  gap: 8,
                  padding: '0px 24px 0px 24px',
                  color: colorTheme.error.value,
                  fontWeight: 600,
                  fontSize: 12,
                  whiteSpace: 'initial',
                }}
              >
                Your changes will be visible immediately anywhere this data is used. This may
                include your production site or store.
              </FlexRow>,
            )}
            <FlexRow style={{ justifyContent: 'flex-end', gap: 8, padding: '0px 24px 12px 24px' }}>
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
                  cursor: 'pointer',
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
                  cursor: 'pointer',
                }}
                onClick={onMainButtonClick}
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

interface RequestUpdateData {
  query: string
  variables: { id: string; metaobject: { fields: Array<{ key: string; value: string }> } }
}

function updateData(projectId: string, data: RequestUpdateData): Promise<void> {
  return fetch(urljoin('/internal/metaobjectupdate', projectId), {
    method: 'POST',
    credentials: 'include',
    headers: HEADERS,
    mode: MODE,
    body: JSON.stringify(data),
  })
    .then((res) => res.json())
    .then((c) => console.info(c))
    .catch((e) => console.error(e))
}

const METAOBJECT_UPDATE_MUTATION = `#graphql
mutation UpdateMetaobject($id: ID!, $metaobject: MetaobjectUpdateInput!) {
  metaobjectUpdate(id: $id, metaobject: $metaobject) {
    metaobject {
      handle
    }
    userErrors {
      field
      message
      code
    }
  }
}`

// TODO: get rid of the hardcoding here
function getReviewData(
  dataPath: string[],
  allVariablesInScope: EditorState['variablesInScope'],
): { gid: string; metafield: string; value: string } | null {
  const [root, indexString, propName, value] = dataPath
  if (
    root !== 'reviewsWithBg' ||
    isNaN(parseInt(indexString)) ||
    propName == null ||
    propName !== 'quote' ||
    value !== 'value'
  ) {
    return null
  }
  const customerReviews = Object.values(allVariablesInScope).find(
    (scope) => scope['customerReviews'] != null,
  )?.['customerReviews']?.spiedValue

  if (customerReviews == null) {
    return null
  }
  const idx = parseInt(indexString)
  const gid = (customerReviews as any)[idx].id
  const metafieldValue = (customerReviews as any)[idx].quote.value

  return { gid: gid, metafield: propName, value: metafieldValue }
}
