use dioxus::prelude::*;

pub fn About(cx: Scope) -> Element {
    cx.render(rsx! {
        div {
            h1 { "About" }
            p { "This is a website for practicing logic proofs." }
        }
    })
}
