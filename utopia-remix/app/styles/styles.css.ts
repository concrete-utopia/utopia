import { style } from "@vanilla-extract/css";
import { sprinkles } from "./sprinkles.css";

export const styles = {
  root: style([
    {
      fontFamily: "Inter, sans-serif",
      fontSize: "11px",
    },
    sprinkles({
      background: {
        lightMode: "white",
        darkMode: "darkModeBlack",
      },
      color: {
        lightMode: "lightModeBlack",
        darkMode: "white",
      },
    }),
  ]),
};

const myStyle = style({
  background: 'transparent',
  ':hover': {
   background: '#0075F910'
  },
  '::before': {
    background: '#0075F9'
  }
});