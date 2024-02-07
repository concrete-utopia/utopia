import { recipe } from "@vanilla-extract/recipes";
import { sprinkles } from "./sprinkles.css";

export const button = recipe({
  base: [
    sprinkles({
      borderRadius: "rounded",
      color: "white",
      boxShadow: "shadow",
    }),
    {
      border: "none",
      cursor: "pointer",
      display: "flex",
      alignItems: "center",
      justifyContent: "center",
      gap: 4,
    },
  ],

  variants: {
    color: {
      neutral: { background: "whitesmoke" },
      brand: { background: "blueviolet" },
      accent: { background: "slateblue" },
    },
    size: {
      small: { padding: 8 },
      medium: { padding: 10 },
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
    color: "accent",
    size: "medium",
  },
});
