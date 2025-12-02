#[cfg(test)]
mod test {
    use crate::VirtualWorkspace;

    #[test]
    fn test_custom_binary() {
        let mut ws = VirtualWorkspace::new();

        ws.def(
            r#"
        ---@class AA
        ---@operator pow(number): AA

        ---@type AA
        a = {}
        "#,
        );

        let ty = ws.expr_ty(
            r#"
        a ^ 1
        "#,
        );
        let expected = ws.ty("AA");
        assert_eq!(ty, expected);
    }

    #[test]
    fn test_issue_559() {
        let mut ws = VirtualWorkspace::new();

        ws.def(
            r#"
            ---@class Origin
            ---@operator add(Origin):Origin

            ---@alias AliasType Origin

            ---@type AliasType
            local x1
            ---@type AliasType
            local x2

            A = x1 + x2
        "#,
        );

        let ty = ws.expr_ty("A");
        let expected = ws.ty("Origin");
        assert_eq!(ty, expected);
    }

    #[test]
    fn test_issue_867() {
        let mut ws = VirtualWorkspace::new();

        ws.def(
            r#"
            local a --- @type { foo? : { bar: { baz: number } } }

            local b = a.foo.bar -- a.foo may be nil (correct)

            c = b.baz -- b may be nil (incorrect)
        "#,
        );

        let ty = ws.expr_ty("c");
        let expected = ws.ty("number");
        assert_eq!(ty, expected);
    }
}
