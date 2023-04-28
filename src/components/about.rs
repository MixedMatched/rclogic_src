use dioxus::prelude::*;

pub fn About(cx: Scope) -> Element {
    cx.render(rsx! {
        div {
            h1 { "About" }
            p { "This is a website for practicing logic proofs." }
            p { "Find the source code on " a { href: "https://github.com/MixedMatched/rclogic_src", "GitHub" } "." }
        }
    })
}
