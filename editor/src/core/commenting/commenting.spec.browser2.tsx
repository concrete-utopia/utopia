import { setMockData } from '../../../liveblocks.mock.config'
import { CommentIndicatorUITestId } from '../../components/canvas/controls/comment-indicator'
import {
  makeTestProjectCodeWithSnippet,
  renderTestEditorWithCode,
} from '../../components/canvas/ui-jsx.test-utils'
import { setLoginState } from '../../components/editor/actions/action-creators'
import { setFeatureForBrowserTestsUseInDescribeBlockOnly } from '../../utils/utils.test-utils'
import { wait } from '../model/performance-scripts'
import { possiblyUniqueColor } from '../shared/multiplayer'

describe('commenting', () => {
  setMockData(
    [
      {
        type: 'thread',
        id: '22',
        roomId: 'mock-room-id',
        createdAt: new Date('05 October 2011 14:48 UTC').toISOString(),
        comments: [
          {
            type: 'comment',
            id: '33',
            threadId: '22',
            roomId: 'mock-room-id',
            userId: 'user-123',
            createdAt: new Date('05 October 2011 14:48 UTC').toISOString(),
            reactions: [],
            body: {
              version: 1,
              content: [
                {
                  type: 'paragraph',
                  children: [
                    {
                      text: 'comment content',
                    },
                  ],
                },
              ],
            },
          },
        ],
        metadata: {
          type: 'canvas',
          x: 22,
          y: 22,
          resolved: false,
        },
      },
    ],
    {
      collaborators: {
        'user-123': {
          id: 'user-123',
          name: 'John Doe',
          avatar: null,
          colorIndex: possiblyUniqueColor([1, 2, 3]),
        },
      },
    },
  )

  setFeatureForBrowserTestsUseInDescribeBlockOnly('Collaboration', true)
  setFeatureForBrowserTestsUseInDescribeBlockOnly('Commenting', true)

  it('can mock', async () => {
    const editor = await renderTestEditorWithCode(
      makeTestProjectCodeWithSnippet('<h1>Zomblocks</h1>'),
      'await-first-dom-report',
    )

    await editor.dispatch([setLoginState({ type: 'LOGGED_IN', user: { userId: 'user' } })], true)

    await wait(100000)

    const commentIndicators = editor.renderedDOM.queryAllByTestId(CommentIndicatorUITestId('22'))

    expect(commentIndicators).toHaveLength(1)
  })
})
