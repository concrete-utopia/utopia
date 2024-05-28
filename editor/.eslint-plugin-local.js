module.exports = {
  rules: {
    'no-dispatch-use-effect': {
      meta: {
        type: 'problem',
        docs: {
          description: "Enforce wrapping 'dispatch' in 'queueMicrotask' inside useEffect",
        },
        // fixable: 'code',
        schema: [],
      },
      create: (context) => {
        return {
          CallExpression(node) {
            if (node.callee.name === 'dispatch') {
              // check recursively if it's somewhere inside useEffect
              let parent = node.parent
              while (parent != null) {
                if (parent.type === 'CallExpression') {
                  if (
                    parent.callee.name === 'useEffect' ||
                    parent.callee.property?.name === 'useEffect'
                  ) {
                    context.report({
                      node: node,
                      message:
                        'Do not use dispatch directly inside useEffect, please wrap in an async function like `queueMicrotask`, `.then()`, etc.',
                    })
                  }
                  // we assume that any other call expression is async, so we break
                  // we can strengthen this by only allowing async functions like setTimeout, setInterval, queueMicrotask, then, etc.
                  break
                }
                parent = parent.parent
              }
            }
          },
        }
      },
      tests: {
        valid: [
          {
            code: `useEffect(() => {
              queueMicrotask(() => {
                dispatch({ type: 'INCREMENT' })
              })
            }, [])`,
          },
          {
            code: `useEffect(() => {
              void GithubHelpers.getUserDetailsFromServer().then((userDetails) => {
                dispatch(
                  userDetails == null
                    ? resetGithubStateAndDataActions()
                    : updateUserDetailsFromServerActions(userDetails),
                )
              })
            }, [])`,
          },
        ],
        invalid: [
          {
            code: `useEffect(() => {
              dispatch({ type: 'INCREMENT' })
            }, [])`,
            errors: [
              {
                message:
                  'Do not use dispatch directly inside useEffect, please wrap in an async function like `queueMicrotask`, `.then()`, etc.',
              },
            ],
          },
        ],
      },
    },
  },
}
