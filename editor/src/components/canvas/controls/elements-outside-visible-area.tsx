/** @jsxRuntime classic */
/** @jsx jsx */
/** @jsxFrag React.Fragment */
import { jsx } from '@emotion/react'
import React from 'react'
import type { ElementPath } from '../../../core/shared/project-file-types'
import { useColorTheme } from '../../../uuiui'
import { useDispatch } from '../../editor/store/dispatch-context'
import {
  ElementOutisdeVisibleAreaIndicatorSize,
  getIndicatorClusterLabel,
  useElementsOutsideVisibleArea,
} from './elements-outside-visible-area-hooks'
import { scrollToElement } from '../../editor/actions/action-creators'
import { when } from '../../../utils/react-conditionals'

export const ElementsOutsideVisibleAreaIndicators = React.memo(
  ({
    canvasRef,
    localSelectedViews,
    localHighlightedViews,
  }: {
    canvasRef: React.MutableRefObject<HTMLDivElement | null>
    localSelectedViews: ElementPath[]
    localHighlightedViews: ElementPath[]
  }) => {
    const dispatch = useDispatch()
    const colorTheme = useColorTheme()

    const scrollTo = React.useCallback(
      (path: ElementPath) => () => {
        dispatch([scrollToElement(path, 'to-center')])
      },
      [dispatch],
    )

    const indicators = useElementsOutsideVisibleArea(
      canvasRef,
      localHighlightedViews,
      localSelectedViews,
    )

    return (
      <>
        {indicators.map((indicator) => {
          const color = colorTheme.dynamicBlue.value
          const clusterRightMargin = indicator.cluster < 10 ? 7 : indicator.cluster === 10 ? 14 : 19
          const isCluster = indicator.cluster > 1
          const testId = indicator.id + (isCluster ? `-cluster-${indicator.cluster}` : '')

          return (
            <div
              key={indicator.id}
              data-testid={testId}
              title='Scroll to element'
              style={{
                position: 'absolute',
                left: indicator.position.x,
                top: indicator.position.y,
                transform: `rotate(${indicator.angle}rad)`,
                color: color,
                fontWeight: 'bolder',
                display: 'flex',
                alignItems: 'center',
                justifyContent: 'center',
                backgroundColor: 'transparent',
                width: ElementOutisdeVisibleAreaIndicatorSize,
                height: ElementOutisdeVisibleAreaIndicatorSize,
              }}
              onClick={scrollTo(indicator.path)}
            >
              <div
                style={{
                  display: 'flex',
                  position: 'relative',
                  alignItems: 'center',
                  justifyContent: 'center',
                  cursor: 'pointer',
                }}
              >
                <div
                  style={{
                    fontSize: 15,
                    width: ElementOutisdeVisibleAreaIndicatorSize - 5,
                    height: ElementOutisdeVisibleAreaIndicatorSize - 5,
                    display: 'flex',
                    marginRight: 2,
                    alignItems: 'center',
                    justifyContent: 'center',
                    paddingBottom: 1,
                    borderRadius: '100%',
                    boxShadow: `${colorTheme.canvasControlsSizeBoxShadowColor20.value} 0px 0px 3px`,
                  }}
                  css={{
                    backgroundColor: colorTheme.bg0.value,
                    '&:hover': {
                      color: 'white',
                      backgroundColor: color,
                    },
                  }}
                >
                  ←
                </div>
                {when(
                  isCluster,
                  <div
                    style={{
                      position: 'absolute',
                      right: -clusterRightMargin,
                      fontSize: 9,
                      transform: `rotate(${Math.PI * 2 - indicator.angle}rad)`, // rotate the label back so it always "faces" the reading direction
                    }}
                  >
                    {getIndicatorClusterLabel(indicator)}
                  </div>,
                )}
              </div>
            </div>
          )
        })}
      </>
    )
  },
)

ElementsOutsideVisibleAreaIndicators.displayName = 'ElementsOutsideVisibleAreaIndicators'
