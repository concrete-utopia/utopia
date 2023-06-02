import { BakedInStoryboardUID } from '../../../../core/model/scene-utils'
import { fromString } from '../../../../core/shared/element-path'
import { windowPoint } from '../../../../core/shared/math-utils'
import { emptyModifiers } from '../../../../utils/modifiers'
import { selectComponentsForTest, wait } from '../../../../utils/utils.test-utils'
import { EdgePositionBottomRight } from '../../canvas-types'
import { TestAppUID } from '../../ui-jsx.test-utils'
import { TestSceneUID } from '../../ui-jsx.test-utils'
import { makeTestProjectCodeWithSnippet, renderTestEditorWithCode } from '../../ui-jsx.test-utils'
import { resizeElement } from './absolute-resize.test-utils'

function makeCodeSnippetForGroups(code: string) {
  return `
  <div data-uid='root-div' style={{width: 400, height: 400, position: 'relative'}}>
    ${code}
  </div>
`
}

async function renderProjectWithGroup(code: string) {
  const editor = await renderTestEditorWithCode(
    makeTestProjectCodeWithSnippet(makeCodeSnippetForGroups(code)),
    'await-first-dom-report',
  )

  return editor
}

describe('Groups behaviors', () => {
  describe('Absolute Positioned Groups', () => {
    describe('Various Group Configurations', () => {
      it('single child with top,left,width,height pins', async () => {
        const editor = await renderProjectWithGroup(`
          <Group data-testid='group' style={{position: 'absolute', left: 50, top: 50}}>
            <div 
              style={{
                backgroundColor: 'red',
                position: 'absolute',
                top: 0,
                left: 0,
                width: 100,
                height: 100,
              }}
            />
          </Group>
        `)
        const groupDiv = editor.renderedDOM.getByTestId('group')

        expect(groupDiv.style.width).toBe('100px')
        expect(groupDiv.style.height).toBe('100px')
      })

      it('multiple children with top,left,width,height pins', async () => {
        const editor = await renderProjectWithGroup(`
          <Group data-testid='group' style={{position: 'absolute', left: 50, top: 50}}>
            <div 
              style={{
                backgroundColor: 'red',
                position: 'absolute',
                top: 0,
                left: 0,
                width: 100,
                height: 100,
              }}
            />
            <div 
              style={{
                backgroundColor: 'red',
                position: 'absolute',
                top: 100,
                left: 100,
                width: 100,
                height: 100,
              }}
            />
          </Group>
        `)
        const groupDiv = editor.renderedDOM.getByTestId('group')

        expect(groupDiv.style.width).toBe('200px')
        expect(groupDiv.style.height).toBe('200px')
      })

      it('single child with OFFSET top,left,width,height pins', async () => {
        const editor = await renderProjectWithGroup(`
          <Group data-testid='group' style={{position: 'absolute', left: 50, top: 50}}>
            <div 
              style={{
                backgroundColor: 'red',
                position: 'absolute',
                top: 50,
                left: 50,
                width: 100,
                height: 100,
              }}
            />
          </Group>
        `)
        const groupDiv = editor.renderedDOM.getByTestId('group')

        expect(groupDiv.style.width).toBe('150px')
        expect(groupDiv.style.height).toBe('150px')
      })

      it('children with bottom,right,width,height pins', async () => {
        const editor = await renderProjectWithGroup(`
          <Group data-testid='group' style={{position: 'absolute', left: 50, top: 50}}>
            <div 
              style={{
                backgroundColor: 'red',
                position: 'absolute',
                right: 0,
                bottom: 0,
                width: 100,
                height: 100,
              }}
            />
            <div 
              style={{
                backgroundColor: 'red',
                position: 'absolute',
                right: 100,
                bottom: 100,
                width: 100,
                height: 100,
              }}
            />
          </Group>
        `)
        const groupDiv = editor.renderedDOM.getByTestId('group')

        expect(groupDiv.style.width).toBe('200px')
        expect(groupDiv.style.height).toBe('200px')
      })
      it('child with top,left,bottom,right pins, no width,height pins', async () => {
        const editor = await renderProjectWithGroup(`
          <Group data-testid='group' style={{position: 'absolute', left: 50, top: 50}}>
            <div 
              style={{
                backgroundColor: 'red',
                position: 'absolute',
                top: 50,
                left: 50,
                right: 100,
                bottom: 100,
              }}
            />
          </Group>
        `)
        const groupDiv = editor.renderedDOM.getByTestId('group')

        // notice that the child ends up with zero width and height because it was set to auto

        expect(groupDiv.style.width).toBe('150px')
        expect(groupDiv.style.height).toBe('150px')
      })

      it('child with top,left,bottom,right, width, height (!!!!!!) pins', async () => {
        const editor = await renderProjectWithGroup(`
          <Group data-testid='group' style={{position: 'absolute', left: 50, top: 50}}>
            <div 
              style={{
                backgroundColor: 'red',
                position: 'absolute',
                top: 50,
                left: 50,
                right: 100,
                bottom: 100,
                width: 50,
                height: 50,
              }}
            />
          </Group>
        `)
        const groupDiv = editor.renderedDOM.getByTestId('group')

        // notice that the child ends up with zero width and height because it was set to auto

        expect(groupDiv.style.width).toBe('200px')
        expect(groupDiv.style.height).toBe('200px')
      })

      it('children with nested Fragments', async () => {
        const editor = await renderProjectWithGroup(`
          <Group data-testid='group' style={{position: 'absolute', left: 50, top: 50}}>
            <div 
              style={{
                backgroundColor: 'red',
                position: 'absolute',
                top: 0,
                left: 0,
                width: 100,
                height: 100,
              }}
            />
            <>
              <div 
                style={{
                  backgroundColor: 'red',
                  position: 'absolute',
                  top: 100,
                  left: 100,
                  width: 100,
                  height: 100,
                }}
              />
            </>
          </Group>
        `)
        const groupDiv = editor.renderedDOM.getByTestId('group')

        expect(groupDiv.style.width).toBe('200px')
        expect(groupDiv.style.height).toBe('200px')
      })
      it('children with nested Groups', async () => {
        const editor = await renderProjectWithGroup(`
        <Group data-testid='group' style={{position: 'absolute', left: 50, top: 50}}>
          <div 
            style={{
              backgroundColor: 'red',
              position: 'absolute',
              top: 0,
              left: 0,
              width: 100,
              height: 100,
            }}
          />
          <Group style={{position: 'absolute', left: 100, top: 100}}>
            <div 
              style={{
                backgroundColor: 'red',
                position: 'absolute',
                top: 0,
                left: 0,
                width: 100,
                height: 100,
              }}
            />
          </Group>
        </Group>
      `)
        const groupDiv = editor.renderedDOM.getByTestId('group')

        expect(groupDiv.style.width).toBe('200px')
        expect(groupDiv.style.height).toBe('200px')
      })
      it('nested group with bottom,right,width,height pins', async () => {
        const editor = await renderProjectWithGroup(`
        <Group data-testid='group' style={{position: 'absolute', left: 50, top: 50}}>
          <div 
            style={{
              backgroundColor: 'red',
              position: 'absolute',
              bottom: 100,
              right: 100,
              width: 100,
              height: 100,
            }}
          />
          <Group style={{position: 'absolute', bottom: 0, right: 0}}>
            <div 
              style={{
                backgroundColor: 'red',
                position: 'absolute',
                top: 0,
                left: 0,
                width: 100,
                height: 100,
              }}
            />
          </Group>
        </Group>
      `)
        const groupDiv = editor.renderedDOM.getByTestId('group')

        expect(groupDiv.style.width).toBe('200px')
        expect(groupDiv.style.height).toBe('200px')
      })

      it('nested group with top,left,right,bottom pins', async () => {
        const editor = await renderProjectWithGroup(`
            <Group data-testid='group' style={{position: 'absolute', left: 50, top: 50}}>
              <div 
                style={{
                  backgroundColor: 'red',
                  position: 'absolute',
                  top: 0,
                  left: 0,
                  width: 100,
                  height: 100,
                }}
              />
              <Group 
                style={{
                  backgroundColor: 'red',
                  position: 'absolute',
                  top: 100,
                  left: 100,
                  right: 50,
                  bottom: 50,
                }}
              >
                <div 
                  style={{
                    backgroundColor: 'red',
                    position: 'absolute',
                    top: 0,
                    left: 0,
                    width: 100,
                    height: 100,
                  }}
                />
              </Group>
            </Group>
          `)
        const groupDiv = editor.renderedDOM.getByTestId('group')

        expect(groupDiv.style.width).toBe('250px')
        expect(groupDiv.style.height).toBe('250px')
      })

      it('IGNORED: Any percentage pins are treated as zero', async () => {
        const editor = await renderProjectWithGroup(`
          <Group data-testid='group' style={{position: 'absolute', left: 50, top: 50}}>
            <div 
              style={{
                backgroundColor: 'red',
                position: 'absolute',
                top: 0,
                left: 0,
                width: 100,
                height: 100,
              }}
            />
            <div 
              style={{
                backgroundColor: 'red',
                position: 'absolute',
                top: 100,
                left: 100,
                width: '50%',
                height: '50%',
              }}
            />
          </Group>
        `)
        const groupDiv = editor.renderedDOM.getByTestId('group')

        expect(groupDiv.style.width).toBe('100px')
        expect(groupDiv.style.height).toBe('100px')
      })

      it('IGNORED: group with static child', async () => {
        const editor = await renderProjectWithGroup(`
          <Group data-testid='group' style={{position: 'absolute', left: 50, top: 50}}>
            <div 
              style={{
                backgroundColor: 'red',
                position: 'absolute',
                top: 0,
                left: 0,
                width: 100,
                height: 100,
              }}
            />
            <div 
              style={{
                backgroundColor: 'red',
                // position: 'absolute', // this is static!
                top: 100,
                left: 100,
                width: 100,
                height: 100,
              }}
            />
          </Group>
        `)
        const groupDiv = editor.renderedDOM.getByTestId('group')

        expect(groupDiv.style.width).toBe('100px')
        expect(groupDiv.style.height).toBe('100px')
      })
    })

    describe('Resizing The Group', () => {
      xit('if the group has no width/height prop, resize it in a fragment-like manner', async () => {
        const editor = await renderProjectWithGroup(`
        <Group data-testid='group' style={{position: 'absolute', left: 50, top: 50}}>
          <div 
            style={{
              backgroundColor: 'red',
              position: 'absolute',
              top: 0,
              left: 0,
              width: 100,
              height: 100,
            }}
          />
          <div 
            style={{
              backgroundColor: 'red',
              position: 'absolute',
              top: 150,
              left: 150,
              width: 50,
              height: 50,
            }}
          />
          <div 
            style={{
              backgroundColor: 'red',
              position: 'absolute',
              right: 0,
              top: 0,
              width: 25,
              height: 25,
            }}
          />
        </Group>
      `)
        const groupDiv = editor.renderedDOM.getByTestId('group')

        expect(groupDiv.style.width).toBe('200px')
        expect(groupDiv.style.height).toBe('200px')

        await selectComponentsForTest(editor, [
          fromString(`${BakedInStoryboardUID}/${TestSceneUID}/${TestAppUID}:root-div/group`),
        ])

        await resizeElement(
          editor,
          windowPoint({ x: 100, y: 100 }),
          EdgePositionBottomRight,
          emptyModifiers,
        )

        await wait(100000000)

        expect(groupDiv.style.width).toBe('300px')
        expect(groupDiv.style.height).toBe('300px')
      })

      it(
        'if the group does have width/height prop, resize it, and update the children in a fragment-like manner',
      )
      it('nested Fragments inside the group exhibit proper fragment-like resize behavior')
      it('nested Groups inside the group exhibit proper group-like resize behavior')
      it('an accidental static child disables all Group-like features')
    })

    describe('Moving The Group', () => {
      it('if it has left/top/right/bottom pins, it moves like a regular Frame')
      it('if it has no left/top/right/bottom pins, moving adds left/top')
    })

    describe('Moving one Child', () => {
      it(
        'moving a child inside the group move the child and offsets the group and all group children so they stay within the bounds',
      )
      it('moving a child to expand bottom-right')
      it('moving a child to expand top-left')
      it('group with width/height prop gets updated in the editor')
      it('children with pins to the right and bottom work properly')
      it('children offset behavior works with nested Fragments')
      it('an accidental static child disables all Group-like features')
    })

    describe('Resizing one Child', () => {
      it(
        'resizing a child inside the group move the child and offsets the group and all group children so they stay within the bounds',
      )
      it('resizing a child to expand bottom-right')
      it('resizing a child to expand top-left')
      it('group with width/height prop gets updated in the editor')
      it('children with pins to the right and bottom work properly')
      it('children offset behavior works with nested Fragments')
      it('an accidental static child disables all Group-like features')
    })

    describe('Inserting into the Grouop', () => {
      it('draw-to-insert into the group is not possible')
      it('canvas drag to reparent into the group is not possible')
    })

    describe('Navigator shows Warning for non group-like Group components', () => {
      it('warns if group has a static child')
      it('warns if group has a child with percentage pin')
    })
  })
})
