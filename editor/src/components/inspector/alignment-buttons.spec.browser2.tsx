import { MetadataUtils } from '../../core/model/element-metadata-utils'
import * as EP from '../../core/shared/element-path'
import { renderTestEditorWithCode } from '../canvas/ui-jsx.test-utils'
import { isAlignmentGroupDisabled } from './use-disable-alignment'

describe('alignment buttons', () => {
  describe('isAlignmentGroupDisabled', () => {
    it('enables buttons for grid cells', async () => {
      const renderResult = await renderTestEditorWithCode(projectCode, 'await-first-dom-report')
      const metadata = renderResult.getEditorState().editor.jsxMetadata

      expect(
        isAlignmentGroupDisabled(
          MetadataUtils.findElementByElementPath(metadata, EP.fromString('sb/grid/grid-child')),
          MetadataUtils.findElementByElementPath(metadata, EP.fromString('sb/grid')),
          'horizontal',
          [],
        ),
      ).toBe(false)
    })
    it('enables buttons for flex only in the opposite direction', async () => {
      const renderResult = await renderTestEditorWithCode(projectCode, 'await-first-dom-report')
      const metadata = renderResult.getEditorState().editor.jsxMetadata

      // flex-direction: row
      {
        expect(
          isAlignmentGroupDisabled(
            MetadataUtils.findElementByElementPath(
              metadata,
              EP.fromString('sb/flex-horiz/flex-horiz-child'),
            ),
            MetadataUtils.findElementByElementPath(metadata, EP.fromString('sb/flex-horiz')),
            'vertical',
            [],
          ),
        ).toBe(false)
        expect(
          isAlignmentGroupDisabled(
            MetadataUtils.findElementByElementPath(
              metadata,
              EP.fromString('sb/flex-horiz/flex-horiz-child'),
            ),
            MetadataUtils.findElementByElementPath(metadata, EP.fromString('sb/flex-horiz')),
            'horizontal',
            [],
          ),
        ).toBe(true)
      }

      // flex-direction: column
      {
        expect(
          isAlignmentGroupDisabled(
            MetadataUtils.findElementByElementPath(
              metadata,
              EP.fromString('sb/flex-vert/flex-vert-child'),
            ),
            MetadataUtils.findElementByElementPath(metadata, EP.fromString('sb/flex-vert')),
            'horizontal',
            [],
          ),
        ).toBe(false)
        expect(
          isAlignmentGroupDisabled(
            MetadataUtils.findElementByElementPath(
              metadata,
              EP.fromString('sb/flex-vert/flex-vert-child'),
            ),
            MetadataUtils.findElementByElementPath(metadata, EP.fromString('sb/flex-vert')),
            'vertical',
            [],
          ),
        ).toBe(true)
      }
    })
    it('enables buttons for absolute only if not storyboard children', async () => {
      const renderResult = await renderTestEditorWithCode(projectCode, 'await-first-dom-report')
      const metadata = renderResult.getEditorState().editor.jsxMetadata

      expect(
        isAlignmentGroupDisabled(
          MetadataUtils.findElementByElementPath(
            metadata,
            EP.fromString('sb/flow/flow-child-absolute'),
          ),
          MetadataUtils.findElementByElementPath(metadata, EP.fromString('sb/flow')),
          'horizontal',
          [],
        ),
      ).toBe(false)

      expect(
        isAlignmentGroupDisabled(
          MetadataUtils.findElementByElementPath(metadata, EP.fromString('sb/flow')),
          MetadataUtils.findElementByElementPath(metadata, EP.fromString('sb')),
          'horizontal',
          [],
        ),
      ).toBe(true)

      expect(
        isAlignmentGroupDisabled(
          MetadataUtils.findElementByElementPath(metadata, EP.fromString('sb/flow')),
          MetadataUtils.findElementByElementPath(metadata, EP.fromString('sb')),
          'horizontal',
          [EP.fromString('sb/flow'), EP.fromString('sb/grid')],
        ),
      ).toBe(false)
    })
    it('disables buttons for flow', async () => {
      const renderResult = await renderTestEditorWithCode(projectCode, 'await-first-dom-report')
      const metadata = renderResult.getEditorState().editor.jsxMetadata

      expect(
        isAlignmentGroupDisabled(
          MetadataUtils.findElementByElementPath(
            metadata,
            EP.fromString('sb/flow/flow-child-relative'),
          ),
          MetadataUtils.findElementByElementPath(metadata, EP.fromString('sb/flow')),
          'horizontal',
          [],
        ),
      ).toBe(true)
    })
  })
})

const projectCode = `import * as React from 'react'
import { Storyboard } from 'utopia-api'

export var storyboard = (
  <Storyboard data-uid='sb'>
    <div
      data-uid='grid'
      style={{
        backgroundColor: '#aaaaaa33',
        position: 'absolute',
        width: 318,
        height: 288,
        display: 'grid',
        gridTemplateColumns: '1fr 1fr 1fr',
        gridTemplateRows: '1fr 1fr 1fr',
        padding: 20,
        gridGap: 20,
        justifyItems: 'flex-end',
        alignContent: 'center',
      }}
    >
      <div
        data-uid='grid'data-uid='grid-child'
        style={{
          backgroundColor: '#aaaaaa33',
          justifySelf: 'stretch',
          gridColumn: 3,
          gridRow: 2,
          alignSelf: 'stretch',
        }}
      />
    </div>
    <div
      data-uid='flow'
      style={{
        backgroundColor: '#aaaaaa33',
        position: 'absolute',
        left: 339,
        top: 0,
        width: 282,
        height: 247,
        contain: 'layout',
      }}
    >
      <div
        data-uid='flow-child-relative'
        style={{
          backgroundColor: '#aaaaaa33',
          left: 21,
          top: 31,
          width: 57,
          height: 59,
        }}
      />
      <div
        style={{
          backgroundColor: '#aaaaaa33',
          left: 22,
          top: 99,
          width: 57,
          height: 59,
        }}
      />
      <div
        data-uid='flow-child-absolute'
        style={{
          backgroundColor: '#aaaaaa33',
          position: 'absolute',
          left: 153,
          top: 94,
          width: 57,
          height: 59,
        }}
      />
    </div>
    <div
      data-uid='flex-horiz'
      style={{
        backgroundColor: '#aaaaaa33',
        position: 'absolute',
        left: 0,
        top: 302,
        width: 318,
        height: 155,
        display: 'flex',
        alignItems: 'center',
        justifyContent: 'center',
        gap: 10,
      }}
    >
      <div
        data-uid='flex-horiz-child'
        style={{
          backgroundColor: '#aaaaaa33',
          width: 57,
          height: 37,
          contain: 'layout',
        }}
      />
      <div
        style={{
          backgroundColor: '#aaaaaa33',
          width: 57,
          height: 37,
          contain: 'layout',
        }}
      />
      <div
        style={{
          backgroundColor: '#aaaaaa33',
          width: 57,
          height: 37,
          contain: 'layout',
        }}
      />
    </div>
    <div
      data-uid='flex-vert'
      style={{
        backgroundColor: '#aaaaaa33',
        position: 'absolute',
        left: 339,
        top: 307,
        width: 164,
        height: 300,
        display: 'flex',
        alignItems: 'center',
        justifyContent: 'center',
        flexDirection: 'column',
        gap: 10,
      }}
    >
      <div
        data-uid='flex-vert-child'
        style={{
          backgroundColor: '#aaaaaa33',
          width: 54.5,
          height: 66,
          contain: 'layout',
        }}
      />
      <div
        style={{
          backgroundColor: '#aaaaaa33',
          width: 54.5,
          height: 66,
          contain: 'layout',
        }}
      />
      <div
        style={{
          backgroundColor: '#aaaaaa33',
          width: 54.5,
          height: 66,
          contain: 'layout',
        }}
      />
    </div>
  </Storyboard>
)
`
