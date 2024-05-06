import React from 'react'
import { getProjectID } from '../../../../common/env-vars'
import { HEADERS, MODE } from '../../../../common/server'
import { isLeft } from '../../../../core/shared/either'
import {
  FlexRow,
  colorTheme,
  UtopiaStyles,
  StringInput,
  Icn,
  useColorTheme,
  FlexColumn,
} from '../../../../uuiui'
import { notice } from '../../../common/notice'
import { showToast } from '../../../editor/actions/action-creators'
import { useDispatch } from '../../../editor/store/dispatch-context'
import { REQUEST_UPDATE_CONTEXT_GLOABAL_HACKED } from '../../../editor/store/remix-derived-data'
import { InspectorModal } from '../../widgets/inspector-modal'
import urljoin from 'url-join'
import { usePopper } from 'react-popper'
import { stopPropagation } from '../../common/inspector-utils'
import { optionalMap } from '../../../../core/shared/optional-utils'
import type { ElementPath, PropertyPath } from '../../../../core/shared/project-file-types'
import { Substores, useEditorState } from '../../../editor/store/store-hook'
import * as EP from '../../../../core/shared/element-path'
import * as PP from '../../../../core/shared/property-path'

export type MetaObjectDataPath = (string | number)[]

interface MetaobjectEditPopupProps {
  close: () => void
  dataPath: MetaObjectDataPath
  style: React.CSSProperties
  currentValue: string
}

export const MetaObjectUpdatePopup = React.forwardRef<HTMLDivElement, MetaobjectEditPopupProps>(
  (props, forwardedRef) => {
    const dispatch = useDispatch()

    const [currentValue, setCurrentValue] = React.useState<string>(props.currentValue)

    const updateCurrentValue = React.useCallback(
      (e: React.ChangeEvent<HTMLInputElement>) => {
        setCurrentValue(e.target.value)
      },
      [setCurrentValue],
    )

    const callUpdateMetaobjectApi = React.useCallback(
      (newValue: any) => {
        const projectId = getProjectID()
        if (projectId == null) {
          throw new Error('projectId == null')
        }

        const routeId_HARDCODED = 'routes/_index'

        const requestUpdateData = REQUEST_UPDATE_CONTEXT_GLOABAL_HACKED[routeId_HARDCODED]
        const lastLoaderResult =
          REQUEST_UPDATE_CONTEXT_GLOABAL_HACKED[routeId_HARDCODED].lastLoaderResult
        if (lastLoaderResult == null) {
          console.error('lastLoaderResult is null')
          return
        }

        const requestUpdateResult = requestUpdateData.requestUpdateCallback?.(
          props.dataPath,
          lastLoaderResult,
          newValue,
        )
        if (requestUpdateResult == null || isLeft(requestUpdateResult)) {
          console.error('Request update failed', requestUpdateResult)
          return
        }

        void fetch(urljoin('/internal/metaobjectupdate', projectId), {
          method: 'POST',
          credentials: 'include',
          headers: HEADERS,
          mode: MODE,
          body: JSON.stringify(requestUpdateResult.value),
        })
          .then((res) => res.json())
          .then((data) => {
            // console.log(data)
            if (data.result.data.metaobjectUpdate.userErrors.length > 0) {
              dispatch([
                showToast(
                  notice(
                    `Metaobject update failed: ${JSON.stringify(
                      data.result.data.metaobjectUpdate.userErrors,
                    )}`,
                    'ERROR',
                    false,
                    'metaobject-update-error',
                  ),
                ),
              ])
            } else {
              dispatch([
                showToast(
                  notice(
                    `Metaobject update requested`,
                    'SUCCESS',
                    false,
                    'metaobject-update-success',
                  ),
                ),
              ])
            }
          })
      },
      [dispatch, props.dataPath],
    )

    const onSubmitValue = React.useCallback(
      (newValue: any) => {
        callUpdateMetaobjectApi(newValue)
        props.close()
      },
      [callUpdateMetaobjectApi, props],
    )

    const onSubmitClick = React.useCallback(() => {
      onSubmitValue(currentValue)
    }, [currentValue, onSubmitValue])

    return (
      <InspectorModal
        offsetX={0}
        offsetY={0}
        closePopup={props.close}
        style={{
          zIndex: 1,
        }}
        closePopupOnUnmount={false}
        outsideClickIgnoreClass={`ignore-react-onclickoutside-${props.dataPath.join('-')}`}
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
          onClick={props.close}
        >
          <FlexRow
            ref={forwardedRef}
            tabIndex={0}
            style={{
              ...props.style,
              left: -5, // to make it align with the inspector
              backgroundColor: colorTheme.neutralBackground.value,
              boxShadow: UtopiaStyles.shadowStyles.mid.boxShadow,
              border: '1px solid lightgrey',
              borderRadius: 4,
              alignItems: 'center',
              maxWidth: '260px',
            }}
          >
            <FlexColumn style={{ padding: '8px 4px', gap: 8, maxWidth: '260px' }}>
              {/* <FlexRow>
                <a
                  href='https://admin.shopify.com/store/praiseful-pear/content/metaobjects/entries/product_ratings/8960933910'
                  target='_blank'
                  rel='noopener noreferrer'
                  style={{ padding: '2px 6px', textDecoration: 'underline' }}
                >
                  View in Shopify Admin
                </a>
                <Icn
                  category='semantic'
                  type='externallink'
                  color='dynamic'
                  width={16}
                  height={16}
                />
              </FlexRow> */}
              <FlexRow style={{ alignItems: 'center', gap: 8 }}>
                <div
                  style={{
                    borderRadius: 4,
                    border: `1px solid ${colorTheme.subduedBorder.value}`,
                    overflow: 'hidden',
                  }}
                >
                  <StringInput
                    onClick={stopPropagation}
                    placeholder='Type new value here'
                    focusOnMount
                    value={currentValue}
                    onChange={updateCurrentValue}
                    growInputAutomatically={true}
                    includeBoxShadow={false}
                    onSubmitValue={onSubmitValue}
                    onEscape={props.close}
                    testId={`metaobject-update-${props.dataPath.join('-')}`}
                  />
                </div>
                <Button onClick={onSubmitClick} text='Save' />
              </FlexRow>
            </FlexColumn>
          </FlexRow>
        </div>
      </InspectorModal>
    )
  },
)

export function useMetaobjectEditPopup(
  elementPath: ElementPath,
  propertyPath: PropertyPath,
  dataPath: MetaObjectDataPath | null,
  testId: string,
) {
  const [referenceElement, setReferenceElement] = React.useState<HTMLDivElement | null>(null)
  const [popperElement, setPopperElement] = React.useState<HTMLDivElement | null>(null)
  const popper = usePopper(referenceElement, popperElement, {
    modifiers: [
      {
        name: 'offset',
        options: {
          offset: [0, 8],
        },
      },
    ],
  })

  const [editPopupVisible, setEditPopupVisible] = React.useState(false)
  const closeMetaObjectEditPopup = React.useCallback(() => {
    setEditPopupVisible(false)
  }, [])

  const openEditPopupWithClick = React.useCallback((e: React.MouseEvent) => {
    e.stopPropagation()
    e.preventDefault()
    setEditPopupVisible(true)
  }, [])

  const currentValue = useEditorState(
    Substores.metadata,
    (store) =>
      store.editor.allElementProps[EP.toString(elementPath)]?.[PP.toString(propertyPath)] ?? null,
    'useMetaobjectEditPopup currentValue',
  )

  if (dataPath == null) {
    return null
  }

  const currentValueString = optionalMap((v) => `${v}`, currentValue)

  const Opener = (
    <div style={{ cursor: 'pointer' }} ref={setReferenceElement}>
      <Icn
        category='semantic'
        type='editpencil-larger'
        color='main'
        width={18}
        height={18}
        data-testid={`edit-${testId}`}
        onClick={openEditPopupWithClick}
      />
    </div>
  )

  const Popup = (
    <MetaObjectUpdatePopup
      {...popper.attributes.popper}
      style={popper.styles.popper}
      ref={setPopperElement}
      close={closeMetaObjectEditPopup}
      dataPath={dataPath}
      currentValue={currentValueString ?? ''}
    />
  )

  return {
    Opener: Opener,
    Popup: Popup,
    editPopupVisible: editPopupVisible,
  }
}

function Button({ onClick, text }: { onClick: () => void; text: string }) {
  const theme = useColorTheme()
  return (
    <div
      onClick={onClick}
      style={{
        cursor: 'pointer',
        borderRadius: 4,
        padding: '4px 8px',
        color: theme.white.value,
        backgroundColor: theme.primary.value,
      }}
    >
      {text}
    </div>
  )
}
