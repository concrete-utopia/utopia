import { renderTestEditorWithCode } from '../../canvas/ui-jsx.test-utils'
import type { NavigatorEntry } from '../../editor/store/editor-state'
import { itemLabelTestIdForEntry } from './item-label'
import { layoutIconTestIdForEntry } from './layout-icon'

describe('Navigator item row icons', () => {
  const testProjectCode = `
    import * as React from 'react'
    import { Group, Scene, Storyboard } from 'utopia-api'

    const Card = (props) => (
      <div style={props.style} data-uid='c2b' />
    )

    export var storyboard = (
      <Storyboard data-uid='sb'>
        <div
          style={{
            backgroundColor: '#aaaaaa33',
            position: 'absolute',
            left: 10,
            top: 10,
            width: 50,
            height: 50,
          }}
          data-uid='aaa'
          data-testid='aaa'
        >
          <div
            style={{
              backgroundColor: '#aaaaaa33',
              position: 'absolute',
              left: 183,
              top: 531,
              width: 77,
              height: 100,
            }}
            data-uid='4ce'
          >
            div with text
          </div>
          <div
            style={{
              backgroundColor: '#aaaaaa33',
              display: 'inline-block',
              width: 31,
              height: 32,
            }}
            data-uid='25c'
          >
            Display Inline
          </div>
          <span data-uid='text'>Some text</span>
          <Group
            style={{
              position: 'absolute',
              left: 119,
              top: 288,
              width: 156,
              height: 140,
            }}
            data-uid='97e'
          >
            <Card
              style={{
                backgroundColor: '#aaaaaa33',
                position: 'absolute',
                left: 0,
                top: 0,
                width: 71,
                height: 68,
              }}
              data-uid='9b8'
            />
            <button
              style={{
                backgroundColor: '#aaaaaa33',
                position: 'absolute',
                left: 92,
                top: 66,
                width: 64,
                height: 74,
              }}
              data-uid='285'
            />
          </Group>
        </div>
        {
          // @utopia/uid=cond
          true ? (
            <div
              style={{
                backgroundColor: '#aaaaaa33',
                display: 'grid',
                position: 'absolute',
                left: 70,
                top: 10,
                width: 50,
                height: 50,
              }}
              data-uid='bbb'
              data-testid='bbb'
            />
          ) : null
        }
        {
          <div
            style={{
              backgroundColor: '#aaaaaa33',
              position: 'absolute',
              left: 130,
              top: 10,
              width: 50,
              height: 50,
              display: 'flex',
            }}
            data-uid='ccc'
            data-testid='ccc'
          />
        }
        {[0].map(() => (
          <div
            style={{
              backgroundColor: '#aaaaaa33',
              position: 'absolute',
              left: 190,
              top: 10,
              width: 50,
              height: 50,
              display: 'flex',
              flexDirection: 'column',
            }}
            data-uid='ddd'
            data-testid='ddd'
          />
        ))}
        <React.Fragment />
        <div data-uid='72d'>
          <div
            style={{
              backgroundColor: '#aaaaaa33',
              position: 'absolute',
              left: 215,
              top: 136,
              width: 90,
              height: 68,
            }}
            data-uid='b4c'
          >
            {'text'}
          </div>
        </div>
        <Scene
          style={{
            backgroundColor: '#aaaaaa33',
            position: 'absolute',
            left: -37,
            top: 474,
            width: 116,
            height: 86,
          }}
          data-uid='a9c'
        />
        <img
          src='https://github.com/concrete-utopia/utopia/blob/master/editor/resources/editor/pyramid_fullsize@2x.jpg?raw=true'
          alt='Utopia logo'
          style={{ height: 100, width: 100 }}
          data-uid='922'
        />
      </Storyboard>
    )
  `

  // The navigator appears to be a little buggy:
  // https://github.com/concrete-utopia/utopia/issues/4773
  it('Should show the correct icons for each type of row', async () => {
    const editor = await renderTestEditorWithCode(testProjectCode, 'await-first-dom-report')
    const visibleNavigatorTargets = editor.getEditorState().derived.visibleNavigatorTargets

    async function checkNavigatorIcon(
      description: string,
      expectedIcnProps: { category: string; type: string; color: string } | 'no-icon',
      navigatorEntry: NavigatorEntry,
      expectWarningIcon: 'expect-warning-icon' | 'expect-normal-icon' = 'expect-normal-icon',
    ) {
      const testId = layoutIconTestIdForEntry(navigatorEntry)

      if (expectedIcnProps === 'no-icon') {
        await expect(async () => editor.renderedDOM.getByTestId(testId)).rejects.toThrow()
      } else {
        const imgElement = editor.renderedDOM.getByTestId(testId)
        const imgData = imgElement.dataset

        const expectedCategory =
          expectWarningIcon === 'expect-warning-icon' ? undefined : expectedIcnProps.category
        const expectedType =
          expectWarningIcon === 'expect-warning-icon' ? 'warningtriangle' : expectedIcnProps.type
        const expectedColor = expectedIcnProps.color
        expect({
          description: description,
          category: imgData['category'],
          type: imgData['type'],
          color: imgData['color'],
        }).toEqual({
          description: description,
          category: expectedCategory,
          type: expectedType,
          color: expectedColor,
        })
      }
    }

    expect(visibleNavigatorTargets.length).toEqual(21)

    await checkNavigatorIcon(
      'Regular div',
      { category: 'navigator-element', type: 'div', color: 'black' },
      visibleNavigatorTargets[0],
    )
    await checkNavigatorIcon(
      'Div with text',
      { category: 'navigator-element', type: 'div', color: 'black' },
      visibleNavigatorTargets[1],
    )
    await checkNavigatorIcon(
      'Display Inline',
      { category: 'navigator-element', type: 'pure-text', color: 'black' },
      visibleNavigatorTargets[2],
    )
    await checkNavigatorIcon(
      'Span',
      { category: 'navigator-element', type: 'pure-text', color: 'black' },
      visibleNavigatorTargets[3],
    )
    await checkNavigatorIcon(
      'Group',
      { category: 'navigator-element', type: 'group-closed', color: 'black' },
      visibleNavigatorTargets[4],
    )
    await checkNavigatorIcon(
      'Component',
      { category: 'navigator-element', type: 'div', color: 'black' },
      visibleNavigatorTargets[5],
    )
    await checkNavigatorIcon(
      'Button',
      { category: 'navigator-element', type: 'clickable', color: 'black' },
      visibleNavigatorTargets[6],
    )
    await checkNavigatorIcon(
      'Conditional',
      { category: 'navigator-element', type: 'conditional', color: 'black' },
      visibleNavigatorTargets[7],
    )
    // No icon for conditional labels
    await checkNavigatorIcon('Conditional label: True', 'no-icon', visibleNavigatorTargets[8])
    await checkNavigatorIcon(
      'Grid layout',
      { category: 'navigator-element', type: 'grid', color: 'blue' },
      visibleNavigatorTargets[9],
    )
    // No icon for conditional labels
    await checkNavigatorIcon('Conditional label: False', 'no-icon', visibleNavigatorTargets[10])
    await checkNavigatorIcon('Empty Slot', 'no-icon', visibleNavigatorTargets[11])
    await checkNavigatorIcon(
      'Code element',
      { category: 'navigator-element', type: 'genericcode', color: 'black' },
      visibleNavigatorTargets[12],
    )
    await checkNavigatorIcon(
      'Flex row',
      { category: 'navigator-element', type: 'flex-row', color: 'blue' },
      visibleNavigatorTargets[13],
    )
    await checkNavigatorIcon(
      'Map',
      { category: 'navigator-element', type: 'lists', color: 'black' },
      visibleNavigatorTargets[14],
    )
    await checkNavigatorIcon(
      'Flex column',
      { category: 'navigator-element', type: 'flex-column', color: 'blue' },
      visibleNavigatorTargets[15],
    )
    await checkNavigatorIcon(
      'Fragment',
      { category: 'navigator-element', type: 'fragment', color: 'black' },
      visibleNavigatorTargets[16],
    )
    await checkNavigatorIcon(
      'Sizeless div',
      { category: 'navigator-element', type: 'zerosized-div', color: 'lightgray' },
      visibleNavigatorTargets[17],
    )
    await checkNavigatorIcon(
      'Generated text',
      { category: 'navigator-element', type: 'div', color: 'black' },
      visibleNavigatorTargets[18],
    )
    await checkNavigatorIcon(
      'Scene',
      { category: 'navigator-element', type: 'scene', color: 'black' },
      visibleNavigatorTargets[19],
    )
    await checkNavigatorIcon(
      'Img',
      { category: 'navigator-element', type: 'image', color: 'black' },
      visibleNavigatorTargets[20],
    )
  })

  // The navigator appears to be a little buggy:
  // https://github.com/concrete-utopia/utopia/issues/4773
  it('Should show the correct labels for each type of row', async () => {
    const editor = await renderTestEditorWithCode(testProjectCode, 'await-first-dom-report')
    const visibleNavigatorTargets = editor.getEditorState().derived.visibleNavigatorTargets

    async function checkNavigatorLabel(
      navigatorEntry: NavigatorEntry,
      expectedLabel: string | null,
    ) {
      const testId = itemLabelTestIdForEntry(navigatorEntry)
      if (expectedLabel != null) {
        const labelElement = editor.renderedDOM.getByTestId(testId)
        expect(labelElement.innerText).toEqual(expectedLabel)
      } else {
        expect(() => editor.renderedDOM.getByTestId(testId)).toThrow()
      }
    }

    expect(visibleNavigatorTargets.length).toEqual(21)

    await checkNavigatorLabel(visibleNavigatorTargets[0], 'div')
    await checkNavigatorLabel(visibleNavigatorTargets[1], 'div with text')
    await checkNavigatorLabel(visibleNavigatorTargets[2], 'Display Inline')
    await checkNavigatorLabel(visibleNavigatorTargets[3], 'Some text')
    await checkNavigatorLabel(visibleNavigatorTargets[4], 'Group')
    await checkNavigatorLabel(visibleNavigatorTargets[5], 'Card')
    await checkNavigatorLabel(visibleNavigatorTargets[6], 'button')
    await checkNavigatorLabel(visibleNavigatorTargets[7], 'CONDITIONAL')
    await checkNavigatorLabel(visibleNavigatorTargets[8], 'TRUE')
    await checkNavigatorLabel(visibleNavigatorTargets[9], 'div')
    await checkNavigatorLabel(visibleNavigatorTargets[10], 'FALSE')
    await checkNavigatorLabel(visibleNavigatorTargets[11], null)
    await checkNavigatorLabel(visibleNavigatorTargets[12], 'CODE')
    await checkNavigatorLabel(visibleNavigatorTargets[13], 'div')
    await checkNavigatorLabel(visibleNavigatorTargets[14], 'List')
    await checkNavigatorLabel(visibleNavigatorTargets[15], 'div')
    await checkNavigatorLabel(visibleNavigatorTargets[16], 'Fragment')
    await checkNavigatorLabel(visibleNavigatorTargets[17], 'div')
    await checkNavigatorLabel(visibleNavigatorTargets[18], 'text')
    await checkNavigatorLabel(visibleNavigatorTargets[19], 'Scene')
    await checkNavigatorLabel(visibleNavigatorTargets[20], 'Utopia logo')
  })
})
