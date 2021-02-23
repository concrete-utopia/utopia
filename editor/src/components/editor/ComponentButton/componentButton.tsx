import * as React from 'react'
import onClickOutside from "react-onclickoutside";

export const ComponentOrInstanceIndicator = (props: any) => {

    const [showComps, toggleComps] = React.useState(false);
    const toggle = () => toggleComps(!showComps)
    const ExpansionArrow = (props: any) => (
        <svg width={8} height={11} style={{ ...props.style }}>
            <line
                x1={0}
                y1={5}
                y2={5 + 3}
                x2={3}
                stroke="white"
                strokeLinecap="round"
            />
            <line
                x1={3 + 3}
                y1={5}
                y2={5 + 3}
                x2={3}
                stroke="white"
                strokeLinecap="round"
            />
        </svg>
    );


    return (
        <div
            id={"ComponentView"}
            onClick={toggle}
            style={{
                position: "relative",
                display: "flex",
                gap: 8,
                maxWidth: 80,
                padding: "5px 8px",
                borderRadius: 2,
                background: props.component
                    ? "#5852FE"
                    : props.instance
                        ? "#5852FE11"
                        : "inherit",
                boxShadow: `0px 0px 0px 1px ${props.component ? "#5852FE11" : props.instance ? "#5852FE" : "#888"
                    }`,
                color: props.component
                    ? "white"
                    : props.instance
                        ? "#5852FE"
                        : "inherit"
            }}
        >
            <div style={{ background: "pink", width: "5", alignItems: 'center' }}>ComponentIcon</div>
            <span
                style={{
                    whiteSpace: "nowrap",
                    overflow: "hidden",
                    textOverflow: "Ellipsis"
                }}
            >
                {props.label}
            </span>
            <ExpansionArrow style={{ flexShrink: 0, flexGrow: 0 }} />
            {showComps ? (
                <div
                    style={{
                        position: "absolute",
                        left: 0,
                        top: 0,
                        borderRadius: 5,
                        zIndex: 99999,
                        width: 170,
                        height: 95,
                        border: "1px solid grey",
                        background: "white",
                        boxShadow:
                            "inset 0px 0px 0px .5px lightgrey, 0px 2px 5px 0px lightgrey",
                        display: "flex",
                        flexDirection: "column",
                        color: "#333",
                        overflow: "ellipsis",
                        whiteSpace: "nowrap"
                    }}
                >
                    Test
                </div>
            ) : null}
        </div>
    )
}

/* const ClickOutsideConfig = {
    handleClickOutside: () => ComponentOrInstanceIndicator.handleClickOutside
  };


export const WrappedComponentOrInstanceIndicator = onClickOutside(
    ComponentOrInstanceIndicator,
    ClickOutsideConfig
  );
*/