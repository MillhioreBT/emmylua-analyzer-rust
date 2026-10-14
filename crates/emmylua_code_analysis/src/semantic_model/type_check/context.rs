//! Check context: accesses the type environment only through `SemanticModel` (semantic).
//!
//! The old implementation held `&DbIndex` (type/member/signature/operator indexes);
//! 类型定义, 直接父类型和成员均通过语义模型查询.
//! Parts still missing from semantic (alias origin types, enum field unions, call operators) fall back to nominal checks.

use crate::LuaType;
use crate::LuaTypeDeclId;
use crate::semantic_db::def::{TypeDef, TypeDefKind};

use super::fail_reason::TypeCheckFailReason;
use crate::semantic_model::SemanticModel;
use crate::semantic_model::render::humanize_type;

/// Rules for comparing a value's type with the expected type.
/// Call argument counts are checked separately.
#[derive(Clone, Copy, PartialEq, Eq)]
pub enum TypeCheckMode {
    /// General compatibility checks, such as checking function arguments.
    /// If `Dog` extends `Animal`, a `Dog` can supply an `Animal` parameter.
    Compatibility,

    /// Check assignments to declared variables and table entries.
    /// For example, a `string` can be assigned to `string | nil`.
    /// Function types use the assignment-specific parameter rules.
    Assignment,

    /// Check result sequences during a compatibility check.
    /// For example, `integer...` can supply `number...`, but not the reverse.
    /// Use assignment rules for result values and `Compatibility` rules
    /// for function parameters.
    Result,
}

impl TypeCheckMode {
    pub(crate) fn uses_assignment_rules(self) -> bool {
        self != Self::Compatibility
    }
}

#[derive(Clone)]
pub struct TypeCheckContext<'db> {
    pub detail: bool,
    pub model: &'db SemanticModel<'db>,
    pub mode: TypeCheckMode,
    /// Strict subtype mode (legacy `type_check_subtype`: all components of a union target, object field level).
    pub strict_union: bool,
    pub strict_object: bool,
    /// Strict generic inheritance mode: retain parent class generic arguments for precise inheritance checks.
    pub strict_generic: bool,
}

impl<'db> TypeCheckContext<'db> {
    pub fn new(model: &'db SemanticModel<'db>, detail: bool) -> Self {
        Self {
            detail,
            model,
            mode: TypeCheckMode::Compatibility,
            strict_union: false,
            strict_object: false,
            strict_generic: false,
        }
    }

    /// Build the failure reason for the current context: when `detail` is enabled, return human-readable type mismatch info.
    pub fn mismatch(&self, source: &LuaType, target: &LuaType) -> TypeCheckFailReason {
        if self.detail {
            TypeCheckFailReason::TypeNotMatchWithReason(format!(
                "expected `{}`, found `{}`",
                humanize_type(self.model, target),
                humanize_type(self.model, source)
            ))
        } else {
            TypeCheckFailReason::TypeNotMatch
        }
    }

    // ── Type environment (semantic resolution) ──

    /// `LuaTypeDeclId` → semantic type definition.
    pub fn type_def_of(&self, id: &LuaTypeDeclId) -> Option<TypeDef> {
        self.model.type_def_of(id)
    }

    /// Direct parent types (A/B in `---@class C : A, B`).
    pub fn super_types_of(&self, id: &LuaTypeDeclId) -> Vec<LuaType> {
        self.model.base_types(id)
    }

    pub fn is_alias(&self, id: &LuaTypeDeclId) -> bool {
        self.type_def_of(id)
            .is_some_and(|def| def.kind == TypeDefKind::Alias)
    }

    /// Alias expansion: the target type (after projection, generic references keep `TplRef`).
    pub fn alias_target_of(&self, id: &LuaTypeDeclId) -> Option<LuaType> {
        let def = self.type_def_of(id)?;
        if def.kind != TypeDefKind::Alias {
            return None;
        }
        self.model.alias_target(&def)
    }

    pub fn is_enum(&self, id: &LuaTypeDeclId) -> bool {
        self.type_def_of(id)
            .is_some_and(|def| def.kind == TypeDefKind::Enum)
    }
}
