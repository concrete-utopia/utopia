import * as Y from 'yjs'

/**
 * Context: https://github.com/yjs/yjs/issues/438
 * Multiple imports of YJS in separate files can cause compile issues, so just import this exported module
 * where needed to avoid that.
 */

export { Y }
