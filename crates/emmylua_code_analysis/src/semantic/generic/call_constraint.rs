use std::ops::Deref;

use emmylua_parser::{LuaAstNode, LuaAstToken, LuaCallExpr, LuaExpr, LuaIndexExpr};
use rowan::TextRange;

use crate::{
    DbIndex, DocTypeInferContext, GenericTplId, LuaDeclExtra, LuaMemberOwner, LuaSemanticDeclId,
    LuaSignature, LuaType, SemanticDeclLevel, SemanticModel, TypeOps, TypeSubstitutor,
    VariadicType, infer_doc_type,
};

// 描述收集到的泛型约束上下文
pub struct CallConstraintContext {
    pub params: Vec<(String, Option<LuaType>)>,
    pub arg_infos: Vec<(LuaType, TextRange)>,
    pub substitutor: TypeSubstitutor,
}

// 建立函数调用的泛型约束上下文
pub fn build_call_constraint_context(
    semantic_model: &SemanticModel,
    call_expr: &LuaCallExpr,
    signature: &LuaSignature,
) -> Option<CallConstraintContext> {
    let mut params = signature.get_type_params();
    let mut arg_infos = get_arg_infos(semantic_model, call_expr)?;
    let mut substitutor = TypeSubstitutor::new();

    // 读取显式传入的泛型实参
    if let Some(type_list) = call_expr.get_call_generic_type_list() {
        let doc_ctx =
            DocTypeInferContext::new(semantic_model.get_db(), semantic_model.get_file_id());
        for (idx, doc_type) in type_list.get_types().enumerate() {
            let ty = infer_doc_type(doc_ctx, &doc_type);
            substitutor.insert_type(GenericTplId::Func(idx as u32), ty);
        }
    }

    // 处理冒号调用与函数定义在 self 参数上的差异
    match (call_expr.is_colon_call(), signature.is_colon_define) {
        (true, true) | (false, false) => {}
        (false, true) => {
            params.insert(0, ("self".into(), Some(LuaType::SelfInfer)));
        }
        (true, false) => {
            // 为非冒号定义但使用冒号调用的情况插入调用者类型
            arg_infos.insert(
                0,
                (
                    infer_call_source_type(semantic_model, call_expr)?,
                    call_expr.get_colon_token()?.get_range(),
                ),
            );
        }
    }

    collect_generic_assignments(&mut substitutor, &params, &arg_infos);

    // 组装上下文信息供外部使用
    Some(CallConstraintContext {
        params,
        arg_infos,
        substitutor,
    })
}

// 将推导结果转换为更易比较的形式
pub fn normalize_constraint_type(db: &DbIndex, ty: LuaType) -> LuaType {
    match ty {
        LuaType::Tuple(tuple) if tuple.is_infer_resolve() => tuple.cast_down_array_base(db),
        _ => ty,
    }
}

// 收集各个参数对应的泛型推导
fn collect_generic_assignments(
    substitutor: &mut TypeSubstitutor,
    params: &[(String, Option<LuaType>)],
    arg_infos: &[(LuaType, TextRange)],
) {
    for (idx, (_, param_type)) in params.iter().enumerate() {
        let Some(param_type) = param_type else {
            continue;
        };
        let Some((arg_type, _)) = arg_infos.get(idx) else {
            continue;
        };
        record_generic_assignment(param_type, arg_type, substitutor);
    }
}

// 实际写入泛型替换表
fn record_generic_assignment(
    param_type: &LuaType,
    arg_type: &LuaType,
    substitutor: &mut TypeSubstitutor,
) {
    match param_type {
        LuaType::TplRef(tpl_ref) | LuaType::ConstTplRef(tpl_ref) => {
            substitutor.insert_type(tpl_ref.get_tpl_id(), arg_type.clone());
        }
        LuaType::StrTplRef(str_tpl_ref) => {
            substitutor.insert_type(str_tpl_ref.get_tpl_id(), arg_type.clone());
        }
        LuaType::Variadic(variadic) => {
            if let Some(inner) = variadic.get_type(0) {
                record_generic_assignment(inner, arg_type, substitutor);
            }
        }
        _ => {}
    }
}

// 解析冒号调用时调用者的具体类型
fn infer_call_source_type(
    semantic_model: &SemanticModel,
    call_expr: &LuaCallExpr,
) -> Option<LuaType> {
    match call_expr.get_prefix_expr()? {
        LuaExpr::IndexExpr(index_expr) => {
            let decl = semantic_model.find_decl(
                index_expr.syntax().clone().into(),
                SemanticDeclLevel::default(),
            )?;

            if let LuaSemanticDeclId::Member(member_id) = decl
                && let Some(LuaSemanticDeclId::Member(member_id)) =
                    semantic_model.get_member_origin_owner(member_id)
            {
                let root = semantic_model
                    .get_db()
                    .get_vfs()
                    .get_syntax_tree(&member_id.file_id)?
                    .get_red_root();
                let cur_node = member_id.get_syntax_id().to_node_from_root(&root)?;
                let index_expr = LuaIndexExpr::cast(cur_node)?;

                return index_expr.get_prefix_expr().map(|prefix_expr| {
                    semantic_model
                        .infer_expr(prefix_expr.clone())
                        .unwrap_or(LuaType::SelfInfer)
                });
            }

            return if let Some(prefix_expr) = index_expr.get_prefix_expr() {
                let expr_type = semantic_model
                    .infer_expr(prefix_expr.clone())
                    .unwrap_or(LuaType::SelfInfer);
                Some(expr_type)
            } else {
                None
            };
        }
        LuaExpr::NameExpr(name_expr) => {
            let decl = semantic_model.find_decl(
                name_expr.syntax().clone().into(),
                SemanticDeclLevel::default(),
            )?;
            if let LuaSemanticDeclId::Member(member_id) = decl {
                let root = semantic_model
                    .get_db()
                    .get_vfs()
                    .get_syntax_tree(&member_id.file_id)?
                    .get_red_root();
                let cur_node = member_id.get_syntax_id().to_node_from_root(&root)?;
                let index_expr = LuaIndexExpr::cast(cur_node)?;

                return index_expr.get_prefix_expr().map(|prefix_expr| {
                    semantic_model
                        .infer_expr(prefix_expr.clone())
                        .unwrap_or(LuaType::SelfInfer)
                });
            }

            return None;
        }
        _ => {}
    }

    None
}

// 推导每个实参类型并保留其语法范围
fn get_arg_infos(
    semantic_model: &SemanticModel,
    call_expr: &LuaCallExpr,
) -> Option<Vec<(LuaType, TextRange)>> {
    let arg_exprs = call_expr.get_args_list()?.get_args().collect::<Vec<_>>();
    let mut arg_infos = infer_expr_list_types(semantic_model, &arg_exprs);
    for (arg_type, arg_expr) in arg_infos.iter_mut() {
        let extend_type = try_instantiate_arg_type(semantic_model, arg_type, arg_expr, 0);
        if let Some(extend_type) = extend_type {
            *arg_type = extend_type;
        }
    }

    let arg_infos = arg_infos
        .into_iter()
        .map(|(arg_type, arg_expr)| (arg_type, arg_expr.get_range()))
        .collect();

    Some(arg_infos)
}

// 尝试展开泛型引用获取实际约束
fn try_instantiate_arg_type(
    semantic_model: &SemanticModel,
    arg_type: &LuaType,
    arg_expr: &LuaExpr,
    depth: usize,
) -> Option<LuaType> {
    match arg_type {
        LuaType::TplRef(tpl_ref) => {
            let node_or_token = arg_expr.syntax().clone().into();
            let semantic_decl =
                semantic_model.find_decl(node_or_token, SemanticDeclLevel::default())?;
            match tpl_ref.get_tpl_id() {
                GenericTplId::Func(tpl_id) => {
                    if let LuaSemanticDeclId::LuaDecl(decl_id) = semantic_decl {
                        let decl = semantic_model
                            .get_db()
                            .get_decl_index()
                            .get_decl(&decl_id)?;
                        match decl.extra {
                            LuaDeclExtra::Param { signature_id, .. } => {
                                let signature = semantic_model
                                    .get_db()
                                    .get_signature_index()
                                    .get(&signature_id)?;
                                if let Some(generic_param) =
                                    signature.generic_params.get(tpl_id as usize)
                                {
                                    return generic_param.type_constraint.clone();
                                }
                            }
                            _ => return None,
                        }
                    }
                    None
                }
                GenericTplId::Type(tpl_id) => {
                    if let LuaSemanticDeclId::LuaDecl(decl_id) = semantic_decl {
                        let decl = semantic_model
                            .get_db()
                            .get_decl_index()
                            .get_decl(&decl_id)?;
                        match decl.extra {
                            LuaDeclExtra::Param {
                                owner_member_id, ..
                            } => {
                                let owner_member_id = owner_member_id?;
                                let parent_owner = semantic_model
                                    .get_db()
                                    .get_member_index()
                                    .get_current_owner(&owner_member_id)?;
                                match parent_owner {
                                    LuaMemberOwner::Type(type_id) => {
                                        let generic_params = semantic_model
                                            .get_db()
                                            .get_type_index()
                                            .get_generic_params(type_id)?;
                                        return generic_params
                                            .get(tpl_id as usize)?
                                            .type_constraint
                                            .clone();
                                    }
                                    _ => return None,
                                }
                            }
                            _ => return None,
                        }
                    }
                    None
                }
            }
        }
        LuaType::Union(union_type) => {
            if depth > 1 {
                return None;
            }
            let mut result = LuaType::Unknown;
            for union_member_type in union_type.into_vec().iter() {
                let extend_type = try_instantiate_arg_type(
                    semantic_model,
                    union_member_type,
                    arg_expr,
                    depth + 1,
                )
                .unwrap_or(union_member_type.clone());
                result = TypeOps::Union.apply(semantic_model.get_db(), &result, &extend_type);
            }
            Some(result)
        }
        _ => None,
    }
}

// 将多个表达式推导为具体类型列表
fn infer_expr_list_types(
    semantic_model: &SemanticModel,
    exprs: &[LuaExpr],
) -> Vec<(LuaType, LuaExpr)> {
    let mut value_types = Vec::new();
    for expr in exprs.iter() {
        let expr_type = semantic_model
            .infer_expr(expr.clone())
            .unwrap_or(LuaType::Unknown);
        match expr_type {
            LuaType::Variadic(variadic) => match variadic.deref() {
                VariadicType::Base(base) => {
                    value_types.push((base.clone(), expr.clone()));
                }
                VariadicType::Multi(vecs) => {
                    for typ in vecs {
                        value_types.push((typ.clone(), expr.clone()));
                    }
                }
            },
            _ => value_types.push((expr_type.clone(), expr.clone())),
        }
    }
    value_types
}
