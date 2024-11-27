import { convertCssToUtopia, changeMediaQueryToContainer } from './css-utils'
import * as csstree from 'css-tree'
import * as prettier from 'prettier'
import { SceneContainerName } from '../../components/canvas/canvas-types'

function formatCss(css: string): string {
  return prettier.format(css, { parser: 'css' })
}

describe('rescopeCSSToTargetCanvasOnly', () => {
  it('Handles the default project CSS', () => {
    const input = `
      body {
        font-family: San Francisco, SF UI, -apple-system, BlinkMacSystemFont, "Segoe UI", Roboto, sans-serif, "Apple Color Emoji", "Segoe UI Emoji", "Segoe UI Symbol";
        width: 100%;
        height: 100%;
      }

      @font-face {
        font-family: 'ITC Garamond ';
        font-style: normal;
        font-weight: 400;
        font-display: swap;
        src: local(ITC Garamond ) format('ttf');
      }

      .appheadercontainer, .listcardcontainer, .gridcardcontainer {
        container-type: inline-size;
      }

      @container (min-width:700px) {
        .apptitle {
            font-size: 3.5em;
        }

        .listcard {
            height: 180px
        }   
        .gridcard {
            height: 325px
        }   
      }

      @container (max-width:700px) {
        .gridcard {
            height: 215px
        }   
      }
    `

    const output = formatCss(convertCssToUtopia(input))
    expect(output).toEqual(
      formatCss(`
      @scope (#canvas-container) {
        :scope {
          font-family: San Francisco, SF UI, -apple-system, BlinkMacSystemFont, "Segoe UI", Roboto, sans-serif, "Apple Color Emoji", "Segoe UI Emoji", "Segoe UI Symbol";
        }
        @font-face {
          font-family: 'ITC Garamond ';
          font-style: normal;
          font-weight: 400;
          font-display: swap;
          src: local(ITC Garamond ) format('ttf');
        }
        .appheadercontainer,  .listcardcontainer,  .gridcardcontainer {
          container-type: inline-size;
        }
        @container (min-width:700px) {
          .apptitle {
              font-size: 3.5em;
          }
          .listcard {
              height: 180px
          }   
          .gridcard {
              height: 325px
          }   
        }
        @container (max-width:700px) {
          .gridcard {
              height: 215px
          }   
        }
      }
    `),
    )
  })

  it('Handles the sample remix project css', () => {
    const input = `
      @font-face {
        font-family: primary;
        src: url(https://cdn.utopia.pizza/editor/sample-assets/stretchpro.woff);
      }

      @font-face {
        font-family: primary-basic;
        src: url(https://cdn.utopia.pizza/editor/sample-assets/stretchpro-basic.woff);
      }

      :root {
        --off-white: #ece6b0;
        --purple: #7cf08c;
        --orange: #0f7e6b;
        --yellow: #dd4a76;
        --primary: primary;
        --primary-basic: primary-basic;
        --secondary: 'Roboto Mono';
        --safety: 'sans-serif';
      }

      #my-thing {
        view-transition-name: main-header;
      }

      .my-class {
        view-transition-name: main-header;
        contain: layout;
      }

      @keyframes fade-in {
        from { opacity: 0; }
      }

      @keyframes fade-out {
        to { opacity: 0; }
      }

      @keyframes slide-from-right {
        from { transform: translateX(30px); }
      }

      @keyframes slide-to-left {
        to { transform: translateX(-30px); }
      }

      ::view-transition-old(root) {
        animation: 90ms cubic-bezier(0.4, 0, 1, 1) both fade-out,
          300ms cubic-bezier(0.4, 0, 0.2, 1) both slide-to-left;
      }

      ::view-transition-new(root) {
        animation: 210ms cubic-bezier(0, 0, 0.2, 1) 90ms both fade-in,
          300ms cubic-bezier(0.4, 0, 0.2, 1) both slide-from-right;
      }
    `

    const output = formatCss(convertCssToUtopia(input))
    expect(output).toEqual(
      formatCss(`
      @scope (#canvas-container) {
        @font-face {
          font-family: primary;
          src: url(https://cdn.utopia.pizza/editor/sample-assets/stretchpro.woff);
        }
        @font-face {
          font-family: primary-basic;
          src: url(https://cdn.utopia.pizza/editor/sample-assets/stretchpro-basic.woff);
        }
        :scope {
          --off-white: #ece6b0;
          --purple: #7cf08c;
          --orange: #0f7e6b;
          --yellow: #dd4a76;
          --primary: primary;
          --primary-basic: primary-basic;
          --secondary: 'Roboto Mono';
          --safety: 'sans-serif';
        }
        #my-thing {
          view-transition-name: main-header;
        }
        .my-class {
          view-transition-name: main-header;
          contain: layout;
        }
        @keyframes fade-in {
          from { opacity: 0; }
        }
        @keyframes fade-out {
          to { opacity: 0; }
        }
        @keyframes slide-from-right {
          from { transform: translateX(30px); }
        }
        @keyframes slide-to-left {
          to { transform: translateX(-30px); }
        }
        ::view-transition-old(root) {
          animation: 90ms cubic-bezier(0.4, 0, 1, 1) both fade-out,
            300ms cubic-bezier(0.4, 0, 0.2, 1) both slide-to-left;
        }
        ::view-transition-new(root) {
          animation: 210ms cubic-bezier(0, 0, 0.2, 1) 90ms both fade-in,
            300ms cubic-bezier(0.4, 0, 0.2, 1) both slide-from-right;
        }
      }
      `),
    )
  })
})

describe('changeMediaQueryToContainer', () => {
  it('converts a simple @media to @container', () => {
    const css = `
    @media (max-width: 700px) {
      .my-class {
        color: red;
      }
    }
    `
    const ast = csstree.parse(css) as csstree.StyleSheet
    changeMediaQueryToContainer(ast.children.first as csstree.Rule, SceneContainerName)
    const output = formatCss(csstree.generate(ast))
    expect(output).toEqual(
      formatCss(`
      @container ${SceneContainerName} (max-width:700px) {
        .my-class {
          color: red;
        }
      }
    `),
    )
  })

  it('converts a @media with multiple conditions to @container', () => {
    const css = `
    @media (max-width: 700px) and (min-width: 500px) {
      .my-class {
        color: red;
      }
    }
    `
    const ast = csstree.parse(css) as csstree.StyleSheet
    changeMediaQueryToContainer(ast.children.first as csstree.Rule, SceneContainerName)
    const output = formatCss(csstree.generate(ast))
    expect(output).toEqual(
      formatCss(`
      @container ${SceneContainerName} (max-width:700px) and (min-width:500px) {
        .my-class {
          color: red;
        }
      }
    `),
    )
  })
  it('converts a @media with conditions that are not only width to @container', () => {
    const css = `
    @media (prefers-color-scheme: dark) and (min-width: 500px) {
      .my-class {
        color: red;
      }
    }
    `
    const ast = csstree.parse(css) as csstree.StyleSheet
    changeMediaQueryToContainer(ast.children.first as csstree.Rule, SceneContainerName)
    const output = formatCss(csstree.generate(ast))
    expect(output).toEqual(
      formatCss(`
      @media (prefers-color-scheme: dark) {
        @container ${SceneContainerName} (min-width:500px) {
          .my-class {
            color: red;
          }
        }
      }
    `),
    )
  })
})
