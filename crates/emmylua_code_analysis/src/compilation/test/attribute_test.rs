#[cfg(test)]
mod test {
    use crate::{DiagnosticCode, VirtualWorkspace};

    #[test]
    fn test_constructor() {
        let mut ws = VirtualWorkspace::new();

        ws.def_files(vec![
            (
                "init.lua",
                r#"
                A = meta("A")
                "#,
            ),
            (
                "meta.lua",
                r#"
            ---@attribute constructor(name: string, root_class: string?, strip_self: boolean?, return_self: boolean?)

            ---@generic T
            ---@[constructor("__init")]
            ---@param name `T`
            ---@return T
            function meta(name)
            end
                "#,
            ),
        ]);
    }

    #[test]
    fn test_def_attribute() {
        let mut ws = VirtualWorkspace::new_with_init_std_lib();

        ws.check_code_for(
            DiagnosticCode::AssignTypeMismatch,
            r#"
        ---@[lsp_perf_optim("check_table_field")]
        local config = {}
        "#,
        );
    }
}
