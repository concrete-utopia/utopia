import { recipe } from "@vanilla-extract/recipes";
import { sprinkles } from "./sprinkles.css";

export const projectCategoryButton = recipe({
  base: [
    sprinkles({
      borderRadius: "large",
    }),
    {
      border: "none",
      cursor: "pointer",
      display: "flex",
      alignItems: "center",
      gap: 10,
      padding: "0 14px",
      height: 30,
      color: "gray", //fix me to be theme responsive
    },
  ],

  variants: {
    color: {
      neutral: {
        background: "transparent",
        ":hover": {
          background: "#0075F910",
        },
      },
      selected: { background: "#0075F9", color: "white" },
    },
  },

  defaultVariants: {
    color: "neutral",
  },
});
