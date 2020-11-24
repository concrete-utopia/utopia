/** @jsx jsx */
import { jsx } from '@emotion/core'
import * as React from 'react'
import {
  getJSXElementNameAsString,
  isJSXArbitraryBlock,
  isJSXElement,
  isJSXFragment,
  JSXElement,
  JSXElementChild,
  jsxElementName,
  JSXElementName,
  UtopiaJSXComponent,
} from '../../../core/shared/element-template'
import { Imports } from '../../../core/shared/project-file-types'
import { fastForEach, NO_OP } from '../../../core/shared/utils'
import { isFeatureEnabled } from '../../../utils/feature-switches'
import {
  colorTheme,
  FlexRow,
  Section,
  SectionBodyArea,
  SectionTitleRow,
  Title,
  UtopiaTheme,
} from '../../../uuiui'
import { betterReactMemo } from '../../../uuiui-deps'
import { setFocus } from '../../common/actions'
import { useEditorState } from '../../editor/store/store-hook'
import { ItemPreview } from './item-preview'

interface SelectedComponentNavigatorProps {
  selectedComponent: UtopiaJSXComponent | null
  imports: Imports
  utopiaComponentNames: string[]
}

const ShowComponentOutlineTitle = isFeatureEnabled('Component Navigator Component Title')

export const SelectedComponentNavigator = betterReactMemo(
  'SelectedComponentNavigator',
  (props: SelectedComponentNavigatorProps) => {
    const { selectedComponent, imports, utopiaComponentNames } = props

    const { dispatch, focusedPanel } = useEditorState((store) => {
      return {
        dispatch: store.dispatch,
        focusedPanel: store.editor.focusedPanel,
      }
    }, 'NavigatorComponent')

    const onFocus = React.useCallback(
      (e: React.FocusEvent<HTMLElement>) => {
        if (focusedPanel !== 'navigator') {
          dispatch([setFocus('navigator')])
        }
      },
      [dispatch, focusedPanel],
    )

    const [minimised, setMinimised] = React.useState(false)
    const toggleMinimised = React.useCallback(() => setMinimised((m) => !m), [])

    let rows: ComponentNavigatorRowProps[] = []

    function appendRowForElementChild(elementChild: JSXElementChild, indentationLevel: number) {
      if (isJSXElement(elementChild)) {
        const stringName = getJSXElementNameAsString(elementChild.name)
        rows.push({
          name: stringName,
          indentationLevel: indentationLevel,
          staticElementName: elementChild.name,
          componentInstance: utopiaComponentNames.includes(stringName),
          imports: imports,
        })
      } else if (isJSXFragment(elementChild)) {
        rows.push({
          name: '<>',
          indentationLevel: indentationLevel,
          staticElementName: jsxElementName('div', []),
          componentInstance: false,
          imports: imports,
        })
      } else if (isJSXArbitraryBlock(elementChild)) {
        rows.push({
          name: `{${elementChild.originalJavascript}}`,
          indentationLevel: indentationLevel,
          staticElementName: jsxElementName('div', []),
          componentInstance: false,
          imports: imports,
        })
      }

      if (isJSXElement(elementChild) || isJSXFragment(elementChild)) {
        fastForEach(elementChild.children, (e) => appendRowForElementChild(e, indentationLevel + 1))
      }
    }

    if (selectedComponent != null) {
      appendRowForElementChild(selectedComponent.rootElement, ShowComponentOutlineTitle ? 1 : 0)
    }

    const title =
      ShowComponentOutlineTitle || selectedComponent == null
        ? 'Component Outline'
        : `${selectedComponent.name} (Component Outline)`

    return (
      <React.Fragment>
        <Section
          data-name='ComponentNavigator'
          onFocus={onFocus}
          onMouseLeave={NO_OP}
          onContextMenu={NO_OP}
          id={'component-navigator'}
          tabIndex={-1}
          style={{ backgroundColor: '#F3F3F3' }}
        >
          <SectionTitleRow
            minimised={minimised}
            toggleMinimised={toggleMinimised}
            dontPad={!ShowComponentOutlineTitle}
            overrideStyle={{ backgroundColor: 'lightgrey' }}
          >
            <FlexRow flexGrow={1}>
              {ShowComponentOutlineTitle ? null : (
                <ItemPreview
                  isAutosizingView={false}
                  collapsed={false}
                  isFlexLayoutedContainer={false}
                  yogaDirection={'row'}
                  yogaWrap={'wrap'}
                  staticElementName={null}
                  componentInstance={true}
                  color={'black'}
                  imports={imports}
                />
              )}
              <Title>{title}</Title>
            </FlexRow>
          </SectionTitleRow>
          {selectedComponent == null ? null : (
            <SectionBodyArea minimised={minimised}>
              {ShowComponentOutlineTitle ? (
                <ComponentRenderRow title={selectedComponent.name} />
              ) : null}
              {rows.map((row, index) => (
                <ComponentNavigatorRow key={`${row.name}-${index}`} {...row} />
              ))}
            </SectionBodyArea>
          )}
        </Section>
      </React.Fragment>
    )
  },
)

interface ComponentNavigatorRowProps {
  name: string
  indentationLevel: number
  staticElementName: JSXElementName | null
  componentInstance: boolean
  imports: Imports
}

const BasePaddingUnit = 20
const InitialPad = 10

const ComponentNavigatorRow = betterReactMemo(
  'ComponentNavigatorRow',
  (props: ComponentNavigatorRowProps) => {
    const { name, indentationLevel, staticElementName, componentInstance, imports } = props

    return (
      <FlexRow
        style={{
          background: 'transparent',
          color: colorTheme.neutralForeground.value,
          height: UtopiaTheme.layout.rowHeight.smaller,
          paddingLeft: InitialPad + indentationLevel * BasePaddingUnit,
        }}
      >
        <FlexRow
          style={{
            overflowY: 'hidden',
            overflowX: 'scroll',
            flexGrow: 1,
          }}
        >
          <ItemPreview
            isAutosizingView={false}
            collapsed={false}
            isFlexLayoutedContainer={false}
            yogaDirection={'row'}
            yogaWrap={'wrap'}
            staticElementName={staticElementName}
            componentInstance={componentInstance}
            color={'black'}
            imports={imports}
          />
          <div
            style={{
              backgroundColor: 'transparent',
              paddingTop: 3,
              paddingBottom: 3,
              marginLeft: 4,
              overflow: 'hidden',
              textOverflow: 'ellipsis',
              whiteSpace: 'nowrap',
            }}
          >
            {name}
          </div>
        </FlexRow>
      </FlexRow>
    )
  },
)

const ComponentRenderRow = betterReactMemo('ComponentRenderRow', ({ title }: { title: string }) => {
  return (
    <FlexRow
      style={{
        background: 'transparent',
        color: colorTheme.neutralForeground.value,
        height: UtopiaTheme.layout.rowHeight.smaller,
      }}
    >
      <FlexRow
        style={{
          overflowY: 'hidden',
          overflowX: 'scroll',
          flexGrow: 1,
          paddingLeft: InitialPad,
        }}
      >
        <ItemPreview
          isAutosizingView={false}
          collapsed={false}
          isFlexLayoutedContainer={false}
          yogaDirection={'row'}
          yogaWrap={'wrap'}
          staticElementName={jsxElementName('seriously-hacky-name', [])}
          componentInstance={true}
          color={'black'}
          imports={{}}
        />
        <div
          style={{
            backgroundColor: 'transparent',
            paddingTop: 3,
            paddingBottom: 3,
            marginLeft: 4,
            overflow: 'hidden',
            textOverflow: 'ellipsis',
            whiteSpace: 'nowrap',
          }}
        >
          {title}
        </div>
      </FlexRow>
    </FlexRow>
  )
})
