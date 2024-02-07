import { recipe } from "@vanilla-extract/recipes";
import { sprinkles } from "./sprinkles.css";

export const projectCategoryButton = recipe({
  base: [
    sprinkles({
      borderRadius: "buttonRound",
      color: "lightModeBlack",
    }),
    {
      border: "none",
      cursor: "pointer",
      display: "flex",
      alignItems: "center",
      gap: 10,
    },
  ],

  variants: {
    color: {
      neutral: { background: "transparent" },
      selected: { background: "#0075F9", color: 'white' },
    },
    size: {
      small: { padding: 8 },
      medium: { padding: '0 14px', height: 30 },
      large: { padding: 16 },
    },
  },

  compoundVariants: [
    {
      variants: {
        color: "neutral",
        size: "large",
      },
      style: {
        background: "ghostwhite",
      },
    },
  ],

  defaultVariants: {
    color: "neutral",
    size: "medium",
  },
});
