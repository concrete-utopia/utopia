import { rescopeCSSToTargetCanvasOnly } from './css-utils'
import * as prettier from 'prettier'

function formatCss(css: string): string {
  return prettier.format(css, { parser: 'css' })
}

describe('rescopeCSSToTargetCanvasOnly', () => {
  it('Handles the default project CSS', () => {
    const input = `
      body {
        font-family: San Francisco, SF UI, -apple-system, BlinkMacSystemFont, "Segoe UI", Roboto, sans-serif, "Apple Color Emoji", "Segoe UI Emoji", "Segoe UI Symbol";
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

      @container (min-width: 700px) {
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

      @container (max-width: 700px) {
        .gridcard {
            height: 215px
        }   
      }
    `

    const output = formatCss(rescopeCSSToTargetCanvasOnly(input))
    expect(output).toEqual(
      formatCss(`
      #canvas-container {
        font-family: San Francisco, SF UI, -apple-system, BlinkMacSystemFont, "Segoe UI", Roboto, sans-serif, "Apple Color Emoji", "Segoe UI Emoji", "Segoe UI Symbol";
      }
      @font-face {
        font-family: 'ITC Garamond ';
        font-style: normal;
        font-weight: 400;
        font-display: swap;
        src: local(ITC Garamond ) format('ttf');
      }
      #canvas-container .appheadercontainer, #canvas-container .listcardcontainer, #canvas-container .gridcardcontainer {
        container-type: inline-size;
      }
      @container (min-width: 700px) {
        #canvas-container .apptitle {
            font-size: 3.5em;
        }
        #canvas-container .listcard {
            height: 180px
        }   
        #canvas-container .gridcard {
            height: 325px
        }   
      }
      @container (max-width: 700px) {
        #canvas-container .gridcard {
            height: 215px
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

    const output = formatCss(rescopeCSSToTargetCanvasOnly(input))
    expect(output).toEqual(
      formatCss(`
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
      #canvas-container #my-thing {
        view-transition-name: main-header;
      }
      #canvas-container .my-class {
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
      `),
    )
  })

  it('Handles container queries', () => {
    const input = `
      /* Default heading styles for the card title */
      .card h2 {
        font-size: 1em;
      }

      /* If the container is larger than 700px */
      @container (min-width: 700px) {
        .card h2 {
          font-size: 2em;
        }
      }
    `

    const output = formatCss(rescopeCSSToTargetCanvasOnly(input))
    expect(output).toEqual(
      formatCss(`
      #canvas-container .card h2 {
        font-size: 1em;
      }
      @container (min-width: 700px) {
        #canvas-container .card h2 {
          font-size: 2em;
        }
      }
    `),
    )
  })

  it('Handles media queries', () => {
    const input = `
      /* At the top level of your code */
      @media screen and (min-width: 900px) {
        article {
          padding: 1rem 3rem;
        }
      }

      /* Nested within another conditional at-rule */
      @supports (display: flex) {
        @media screen and (min-width: 900px) {
          article {
            display: flex;
          }
        }
      }
    `

    const output = formatCss(rescopeCSSToTargetCanvasOnly(input))
    expect(output).toEqual(
      formatCss(`
      @media screen and (min-width: 900px) {
        #canvas-container article {
          padding: 1rem 3rem;
        }
      }
      @supports (display: flex) {
        @media screen and (min-width: 900px) {
          #canvas-container article {
            display: flex;
          }
        }
      }
    `),
    )
  })

  it('Handles keyframes', () => {
    const input = `
      @keyframes identifier {
        from {
          top: 0;
          left: 0;
        }
        30% {
          top: 50px;
        }
        68%,
        72% {
          left: 50px;
        }
        to {
          top: 100px;
          left: 100%;
        }
      }
    `

    const output = formatCss(rescopeCSSToTargetCanvasOnly(input))
    expect(output).toEqual(
      formatCss(`
        @keyframes identifier {
          from {
            top: 0;
            left: 0;
          }
          30% {
            top: 50px;
          }
          68%,
          72% {
            left: 50px;
          }
          to {
            top: 100px;
            left: 100%;
          }
        }
      `),
    )
  })

  it('Skips selectors inside pseudo-class selectors, with the exception of :is()', () => {
    const input = `
      .myClass:has(.x) {
        background-color: aqua;
      }

      :is(h1, h2, h3):has(+ :is(h2, h3, h4)) {
        margin: 0 0 0.25rem 0;
      }
    `

    const output = formatCss(rescopeCSSToTargetCanvasOnly(input))
    expect(output).toEqual(
      formatCss(`
        #canvas-container .myClass:has(.x) {
          background-color: aqua;
        }
        :is(#canvas-container h1, #canvas-container h2, #canvas-container h3):has(+ :is(h2, h3, h4)) {
          margin: 0 0 0.25rem 0;
        }
      `),
    )
  })
})
