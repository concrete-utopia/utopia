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
                if (
                  parent.type === 'FunctionDeclaration' &&
                  parent.parent?.type === 'BlockStatement'
                ) {
                  // we assume that if it's inside a function declaration,
                  // it will be used in an async manner as a callback
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
          {
            code: `React.useEffect(() => {
              function handleKeyDown(event) {
                if (isDismissKey) {
                  dispatch([clearPostActionData()])
                }
              }
        
              if (isPostActionMenuActive(postActionSessionChoices)) {
                window.addEventListener('keydown', handleKeyDown, true)
              }
        
              return function cleanup() {
                window.removeEventListener('keydown', handleKeyDown, true)
              }
            }, [dispatch, postActionSessionChoices, open])`,
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
          {
            code: `React.useEffect(function() {
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
