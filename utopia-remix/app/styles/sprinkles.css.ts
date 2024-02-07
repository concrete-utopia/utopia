import { defineProperties, createSprinkles } from "@vanilla-extract/sprinkles";

const colors = {
  black: "#000",
  white: "#fff",
  blue: "#09f",
  aqua: "#00E3E3"
};

const colorProperties = defineProperties({
  conditions: {
    lightMode: {},
    darkMode: { "@media": "(prefers-color-scheme: dark)" },
  },
  defaultCondition: "lightMode",
  properties: {
    color: colors,
    background: colors,
  },
});

const borderRadii = {
  rounded: 10,
  roundedFull: "100%",
};

const borderProperties = defineProperties({
  properties: {
    borderRadius: borderRadii,
  },
});

const shadows = {
  shadow: "0 1px 3px 0 rgb(0 0 0 / 0.1), 0 1px 2px -1px rgb(0 0 0 / 0.1)",
};

const shadowProperties = defineProperties({
  properties: {
    boxShadow: shadows,
  },
});

const marginProperties = defineProperties({
  properties: {
    boxShadow: shadows,
  },
});

export const sprinkles = createSprinkles(
  colorProperties,
  borderProperties,
  shadowProperties,
  marginProperties,
);

export type Sprinkles = Parameters<typeof sprinkles>[0];
