use crate::{FileId, LuaType, VirtualWorkspace};
use emmylua_parser::{LuaAstNode, LuaAstToken, LuaLocalName};
use googletest::prelude::*;

fn local_type(ws: &VirtualWorkspace, file: FileId, name: &str) -> LuaType {
    let model = ws.analysis.semantic_model(file);
    let chunk = model.chunk().expect("chunk");
    let local = chunk
        .descendants::<LuaLocalName>()
        .find(|local| {
            local
                .get_name_token()
                .is_some_and(|token| token.get_name_text() == name)
        })
        .expect("local declaration");
    let decl = model
        .decl_by_offset(local.get_name_token().expect("name token").get_position())
        .expect("declaration");
    model.type_of_decl(&decl).expect("local type")
}

#[gtest]
fn named_local_cast_does_not_replace_array_element_type() {
    for strict in [false, true] {
        let mut ws = VirtualWorkspace::new_with_init_std_lib();
        let mut config = ws.get_emmyrc();
        config.strict.array_index = strict;
        ws.update_emmyrc(config);
        let file = ws.def(
            r#"
            ---@param values string[]
            ---@param index integer?
            local function select_value(values, index)
                ---@cast index integer
                local direct = values[index]
                ---@cast index integer
                local checked = assert(values[index])
                return direct, checked
            end
            "#,
        );
        let expected = ws.ty(if strict { "string?" } else { "string" });
        expect_that!(local_type(&ws, file, "direct"), eq(&expected));
        expect_that!(local_type(&ws, file, "checked"), eq(&LuaType::String));
    }
}

#[gtest]
fn named_member_cast_narrows_only_the_array_key() {
    for strict in [false, true] {
        let mut ws = VirtualWorkspace::new_with_init_std_lib();
        let mut config = ws.get_emmyrc();
        config.strict.array_index = strict;
        ws.update_emmyrc(config);
        let file = ws.def(
            r#"
            ---@alias uint32 integer
            ---@class Entry
            ---@field name string
            local Entry = {}
            ---@class Position
            ---@field index uint32?
            local Position = {}

            ---@param values Entry[]
            ---@param position Position
            local function select_entry(values, position)
                ---@cast position.index uint32
                local direct = values[position.index]
                ---@cast position.index uint32
                local checked = assert(values[position.index])
                local later = values[position.index]
                return direct, checked, later
            end
            "#,
        );
        let expected = ws.ty(if strict { "Entry?" } else { "Entry" });
        expect_that!(local_type(&ws, file, "direct"), eq(&expected));
        expect_that!(local_type(&ws, file, "later"), eq(&expected));
        let checked = ws.ty("Entry");
        expect_that!(local_type(&ws, file, "checked"), eq(&checked));
    }
}

#[gtest]
fn named_cast_operators_do_not_change_unrelated_indexed_values() {
    let mut ws = VirtualWorkspace::new();
    let file = ws.def(
        r#"
        ---@param values string[]
        ---@param index integer?
        ---@param unrelated unknown
        local function select_values(values, index, unrelated)
            ---@cast unrelated integer
            local replaced = values[1]
            ---@cast unrelated +string
            local widened = values[1]
            ---@cast index -?
            local narrowed = values[index]
            return replaced, widened, narrowed
        end
        "#,
    );
    let expected = ws.ty("string?");
    for name in ["replaced", "widened", "narrowed"] {
        expect_that!(local_type(&ws, file, name), eq(&expected));
    }
}

#[gtest]
fn member_key_narrowing_reaches_nested_index_and_call_arguments() {
    let mut ws = VirtualWorkspace::new_with_init_std_lib();
    let file = ws.def(
        r#"
        ---@class NestedPosition
        ---@field index integer?
        local NestedPosition = {}

        ---@param values string[][]
        ---@param position NestedPosition
        local function select_value(values, position)
            ---@cast position.index -?
            local selected = assert(assert(values[position.index])[position.index])
            return selected
        end
        "#,
    );
    expect_that!(local_type(&ws, file, "selected"), eq(&LuaType::String));
}

#[gtest]
fn inline_expression_casts_still_apply() {
    let mut ws = VirtualWorkspace::new();
    let file = ws.def(
        r#"
        ---@param values string[]
        local function narrow_value(values)
            local narrowed = values[1] --[[@cast -?]]
            return narrowed
        end
        ---@param values string[]
        local function widen_value(values)
            local widened = values[1] --[[@cast +boolean]]
            return widened
        end
        ---@param values string[]
        local function replace_value(values)
            local replaced = values[1] --[[@as integer]]
            return replaced
        end
        "#,
    );
    expect_that!(local_type(&ws, file, "narrowed"), eq(&LuaType::String));
    let widened = ws.ty("string|nil|boolean");
    expect_that!(local_type(&ws, file, "widened"), eq(&widened));
    expect_that!(local_type(&ws, file, "replaced"), eq(&LuaType::Integer));
}
