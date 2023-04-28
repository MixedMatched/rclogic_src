#![allow(non_snake_case)]
use dioxus::prelude::*;

mod components {
    pub mod about;
    pub mod first_order_logic;
    pub mod intuitionistic_logic;
    pub mod modal_logic;
    pub mod propositional_logic;
}

#[derive(PartialEq)]
pub enum AppState {
    About,
    PropositionalLogic,
    FirstOrderLogic,
    ModalLogic,
    IntuitionisticLogic,
}

#[derive(PartialEq, Props)]
pub struct AppProps {
    state: AppState,
}

fn main() {
    dioxus_web::launch(App);
}

fn App(cx: Scope) -> Element {
    let props = use_state(cx, || AppProps {
        state: AppState::About,
    });

    cx.render(rsx! {
        style { include_str!("../src/simple.min.css") },
        header {
            nav {
                button {
                    onclick: move |_| {
                        props.set(AppProps {
                            state: AppState::About,
                        })
                    },
                    "About" 
                }
                button {
                    onclick: move |_| {
                        props.set(AppProps {
                            state: AppState::PropositionalLogic,
                        })
                    },
                    "Propositional Logic" 
                }
                button {
                    onclick: move |_| {
                        props.set(AppProps {
                            state: AppState::FirstOrderLogic,
                        })
                    },
                    "First Order Logic" 
                }
                button {
                    onclick: move |_| {
                        props.set(AppProps {
                            state: AppState::ModalLogic,
                        })
                    },
                    "Modal Logic" 
                }
                button {
                    onclick: move |_| {
                        props.set(AppProps {
                            state: AppState::IntuitionisticLogic,
                        })
                    },
                    "Intuitionistic Logic" 
                }
            }
        },
        match props.state {
            AppState::About => components::about::About(cx),
            AppState::PropositionalLogic => components::propositional_logic::PropositionalLogic(cx),
            AppState::FirstOrderLogic => components::first_order_logic::FirstOrderLogic(cx),
            AppState::ModalLogic => components::modal_logic::ModalLogic(cx),
            AppState::IntuitionisticLogic => components::intuitionistic_logic::IntuitionisticLogic(cx),
        },
    })
}
