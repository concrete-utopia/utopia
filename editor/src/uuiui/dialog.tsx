import styled from '@emotion/styled'
import * as React from 'react'
import { Isolator } from './isolator'
import { FlexColumn } from './widgets/layout/flex-column'
import { FlexRow } from './widgets/layout/flex-row'
import { colorTheme, UtopiaTheme } from './styles/theme'

interface DialogProps {
  title: string
  content: React.ReactElement
  defaultButton: React.ReactElement
  secondaryButton: React.ReactElement
  subduedButton?: React.ReactElement
  closeCallback: () => void
}

export const Dialog = (props: DialogProps) => (
  <Isolator onAbandonIntent={props.closeCallback}>
    <ScreenCenter>
      <div
        id='outerDialog'
        onMouseDown={(e) => e.stopPropagation()}
        onMouseUp={(e) => e.stopPropagation()}
        onClick={(e) => e.stopPropagation()}
        style={{
          width: '450px',
          height: '220px',
          transform: 'translateY(-150px)',
          backgroundColor: colorTheme.emphasizedBackground.value,
          overflow: 'hidden',
          borderRadius: '4px',
          boxShadow: '0px 10px 30px 0px hsla(0,0%,30%,.5)',
        }}
      >
        <FlexColumn
          style={{
            height: '200px',
            whiteSpace: 'initial',
            alignItems: 'stretch',
          }}
        >
          <FlexRow
            id='DialogTitleRow'
            style={{
              padding: '16px 16px',
              whiteSpace: 'initial',
              minHeight: UtopiaTheme.layout.rowHeight.large,
            }}
          >
            <span
              style={{
                fontSize: '14px',
                fontWeight: 600,
                color: colorTheme.emphasizedForeground.value,
              }}
            >
              {props.title}
            </span>
          </FlexRow>
          <div
            className='DialogContent'
            style={{
              padding: '16px 16px',
              flexGrow: 1,
              maxHeight: '400px',
              overflow: 'scroll',
              width: '100%',
              whiteSpace: 'initial',
            }}
          >
            <div
              style={{
                fontSize: '12px',
                fontWeight: 400,
                color: colorTheme.neutralForeground.value,
                lineHeight: '1.4',
              }}
            >
              {props.content}
            </div>
          </div>
          <FlexRow
            id='DialogButtonRow'
            style={{
              height: '40px',
              padding: '16px',
              flexShrink: 0,
              width: '100%',
              whiteSpace: 'initial',
              justifyContent: 'flex-end',
            }}
          >
            <span>{props.subduedButton}</span>
            <span style={{ marginLeft: 8 }}>{props.secondaryButton}</span>
            <span style={{ marginLeft: 8 }}>{props.defaultButton}</span>
          </FlexRow>
        </FlexColumn>
      </div>
    </ScreenCenter>
  </Isolator>
)

const ScreenCenter = styled.div({
  position: 'fixed',
  left: '0px',
  top: '0px',
  right: '0px',
  bottom: '0px',
  display: 'flex',
  alignItems: 'center',
  justifyContent: 'center',
})
