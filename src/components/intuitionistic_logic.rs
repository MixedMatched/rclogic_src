use dioxus::prelude::*;

pub fn IntuitionisticLogic(cx: Scope) -> Element {
    cx.render(rsx! {
        div {
            h1 { "Intuitionistic Logic" }
            p { "This is a website for practicing logic proofs." }
        }
    })
}
