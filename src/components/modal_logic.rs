use dioxus::prelude::*;

pub fn ModalLogic(cx: Scope) -> Element {
    cx.render(rsx! {
        div {
            h1 { "Modal Logic" }
            p { "This is a website for practicing logic proofs." }
        }
    })
}
