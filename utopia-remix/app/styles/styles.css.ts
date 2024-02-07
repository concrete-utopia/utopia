import { style } from "@vanilla-extract/css";
import { sprinkles } from "./sprinkles.css";

export const styles = {
  root: style([
    {
      fontFamily: "sans-serif",
    },
    sprinkles({
      background: {
        lightMode: "white",
        darkMode: "black",
      },
      color: {
        lightMode: "black",
        darkMode: "white",
      },
    }),
  ]),
};
