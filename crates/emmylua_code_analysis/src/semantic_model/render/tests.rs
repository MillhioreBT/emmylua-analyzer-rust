use googletest::prelude::*;

use crate::VirtualWorkspace;

#[gtest]
fn explicit_table_arguments_survive_standard_library_definition() {
    let mut ws = VirtualWorkspace::new_with_init_std_lib();
    ws.def("---@class Train\n---@field id integer");

    for annotation in ["table<integer,Train>", "table<string,table<integer,Train>>"] {
        let ty = ws.ty(annotation);
        assert_that!(ws.humanize_type(ty.clone()), eq(annotation));
        assert_that!(ws.humanize_type_detailed(ty), eq(annotation));
    }
}

#[gtest]
fn rendering_preserves_arguments_beyond_declared_parameters() {
    let mut ws = VirtualWorkspace::new();
    ws.def("---@class Plain\n---@class Box<T>");

    for annotation in [
        "Plain<string>",
        "Box<string,integer>",
        "Missing<string,integer>",
    ] {
        let ty = ws.ty(annotation);
        assert_that!(ws.humanize_type(ty), eq(annotation));
    }
}

#[gtest]
fn rendering_still_fills_defaults_and_missing_parameters() {
    let mut ws = VirtualWorkspace::new();
    ws.def("---@class Pair<T, U = T[]>\n---@class Required<T, U>");

    for (annotation, expected) in [
        ("Pair<string>", "Pair<string,string[]>"),
        ("Pair<string,integer>", "Pair<string,integer>"),
        ("Required<string>", "Required<string,unknown>"),
    ] {
        let ty = ws.ty(annotation);
        assert_that!(ws.humanize_type(ty), eq(expected));
    }
}
