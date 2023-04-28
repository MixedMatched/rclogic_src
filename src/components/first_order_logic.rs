use dioxus::prelude::*;

pub fn FirstOrderLogic(cx: Scope) -> Element {
    cx.render(rsx! {
        div {
            h1 { "First Order Logic" }
            p { "This is a website for practicing logic proofs." }
        }
    })
}
