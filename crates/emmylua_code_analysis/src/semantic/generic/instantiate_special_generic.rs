use std::ops::Deref;

use crate::{
    DbIndex, LuaAliasCallKind, LuaAliasCallType, LuaMemberKey, LuaTupleStatus, LuaTupleType,
    LuaType, TypeOps, VariadicType, get_member_map,
    semantic::{
        generic::instantiate_type_generic::key_type_to_member_key,
        member::{find_members, infer_raw_member_type},
        type_check,
    },
};

use super::{TypeSubstitutor, instantiate_type_generic};

pub fn instantiate_alias_call(
    db: &DbIndex,
    alias_call: &LuaAliasCallType,
    substitutor: &TypeSubstitutor,
) -> LuaType {
    let operands = alias_call
        .get_operands()
        .iter()
        .map(|it| instantiate_type_generic(db, it, substitutor))
        .collect::<Vec<_>>();

    match alias_call.get_call_kind() {
        LuaAliasCallKind::Sub => {
            if operands.len() != 2 {
                return LuaType::Unknown;
            }
            // 如果类型为`Union`且只有一个类型, 则会解开`Union`包装
            TypeOps::Remove.apply(db, &operands[0], &operands[1])
        }
        LuaAliasCallKind::Add => {
            if operands.len() != 2 {
                return LuaType::Unknown;
            }

            TypeOps::Union.apply(db, &operands[0], &operands[1])
        }
        LuaAliasCallKind::KeyOf => {
            if operands.len() != 1 {
                return LuaType::Unknown;
            }
            let is_tuple = operands.len() == 1 && operands[0].is_tuple();
            // TODO: 根据`Typescript`的规则, 当为类型联合时, 应该返回共有字段, 但现在并不是

            let members = find_members(db, &operands[0]).unwrap_or_default();
            let member_key_types = members
                .iter()
                .filter_map(|m| match &m.key {
                    LuaMemberKey::Integer(i) => Some(LuaType::DocIntegerConst(*i)),
                    LuaMemberKey::Name(s) => Some(LuaType::DocStringConst(s.clone().into())),
                    _ => None,
                })
                .collect::<Vec<_>>();
            if is_tuple {
                LuaType::Tuple(
                    LuaTupleType::new(member_key_types, LuaTupleStatus::InferResolve).into(),
                )
            } else {
                LuaType::from_vec(member_key_types)
            }
        }
        // 条件类型不在此处理
        LuaAliasCallKind::Extends => {
            if operands.len() != 2 {
                return LuaType::Unknown;
            }

            let compact = type_check::check_type_compact(db, &operands[0], &operands[1]).is_ok();
            LuaType::BooleanConst(compact)
        }
        LuaAliasCallKind::Select => {
            if operands.len() != 2 {
                return LuaType::Unknown;
            }

            instantiate_select_call(&operands[0], &operands[1])
        }
        LuaAliasCallKind::Unpack => instantiate_unpack_call(db, &operands),
        LuaAliasCallKind::RawGet => {
            if operands.len() != 2 {
                return LuaType::Unknown;
            }

            instantiate_rawget_call(db, &operands[0], &operands[1])
        }
        LuaAliasCallKind::Index => {
            if operands.len() != 2 {
                return LuaType::Unknown;
            }

            instantiate_index_call(db, &operands[0], &operands[1])
        }
    }
}

#[derive(Debug)]
enum NumOrLen {
    Num(i64),
    Len,
    LenUnknown,
}

fn instantiate_select_call(source: &LuaType, index: &LuaType) -> LuaType {
    let num_or_len = match index {
        LuaType::DocIntegerConst(i) => {
            if *i == 0 {
                return LuaType::Unknown;
            }
            NumOrLen::Num(*i)
        }
        LuaType::IntegerConst(i) => {
            if *i == 0 {
                return LuaType::Unknown;
            }
            NumOrLen::Num(*i)
        }
        LuaType::DocStringConst(s) => {
            if s.as_str() == "#" {
                NumOrLen::Len
            } else {
                NumOrLen::LenUnknown
            }
        }
        LuaType::StringConst(s) => {
            if s.as_str() == "#" {
                NumOrLen::Len
            } else {
                NumOrLen::LenUnknown
            }
        }
        _ => return LuaType::Unknown,
    };

    match num_or_len {
        NumOrLen::Num(i) => match source {
            LuaType::Tuple(tuple) => {
                if let Some(first) = tuple.get_type(0) {
                    if first.is_variadic() {
                        return first.clone();
                    }
                }
                // 返回元组中从 i 开始的所有类型
                let mut types = Vec::new();
                for ty in tuple.get_types().iter().skip(i as usize - 1) {
                    types.push(ty.clone());
                }

                LuaType::Tuple(LuaTupleType::new(types, LuaTupleStatus::InferResolve).into())
            }
            _ => LuaType::Unknown,
        },
        NumOrLen::Len => {
            if let LuaType::Tuple(tuple) = source {
                return LuaType::IntegerConst(tuple.get_types().len() as i64);
            }
            LuaType::Integer
        }
        NumOrLen::LenUnknown => LuaType::Integer,
    }
}

fn instantiate_unpack_call(db: &DbIndex, operands: &[LuaType]) -> LuaType {
    if operands.is_empty() {
        return LuaType::Unknown;
    }

    let need_unpack_type = &operands[0];
    let mut start = -1;
    // todo use end
    #[allow(unused)]
    let mut end = -1;
    if operands.len() > 1 {
        if let LuaType::DocIntegerConst(i) = &operands[1] {
            start = *i - 1;
        } else if let LuaType::IntegerConst(i) = &operands[1] {
            start = *i - 1;
        }
    }

    #[allow(unused)]
    if operands.len() > 2 {
        if let LuaType::DocIntegerConst(i) = &operands[2] {
            end = *i;
        } else if let LuaType::IntegerConst(i) = &operands[2] {
            end = *i;
        }
    }

    match &need_unpack_type {
        LuaType::Tuple(tuple) => {
            let mut types = tuple.get_types().to_vec();
            if start > 0 {
                if start as usize > types.len() {
                    return LuaType::Unknown;
                }

                if start < types.len() as i64 {
                    types = types[start as usize..].to_vec();
                }
            }

            LuaType::Variadic(VariadicType::Multi(types).into())
        }
        LuaType::Array(array_type) => LuaType::Variadic(
            VariadicType::Base(TypeOps::Union.apply(db, array_type.get_base(), &LuaType::Nil))
                .into(),
        ),
        LuaType::TableGeneric(table) => {
            if table.len() != 2 {
                return LuaType::Unknown;
            }

            let value = table[1].clone();
            LuaType::Variadic(
                VariadicType::Base(TypeOps::Union.apply(db, &value, &LuaType::Nil)).into(),
            )
        }
        LuaType::Unknown | LuaType::Any => LuaType::Unknown,
        _ => {
            // may cost many
            let mut multi_types = vec![];
            let members = match get_member_map(db, need_unpack_type) {
                Some(members) => members,
                None => return LuaType::Unknown,
            };

            for i in 1..10 {
                let member_key = LuaMemberKey::Integer(i);
                if let Some(member_info) = members.get(&member_key) {
                    let mut member_type = LuaType::Unknown;
                    for sub_member_info in member_info {
                        member_type = TypeOps::Union.apply(db, &member_type, &sub_member_info.typ);
                    }
                    multi_types.push(member_type);
                } else {
                    break;
                }
            }

            LuaType::Variadic(VariadicType::Multi(multi_types).into())
        }
    }
}

fn instantiate_rawget_call(db: &DbIndex, owner: &LuaType, key: &LuaType) -> LuaType {
    let member_key = match key {
        LuaType::DocStringConst(s) => LuaMemberKey::Name(s.deref().clone()),
        LuaType::StringConst(s) => LuaMemberKey::Name(s.deref().clone()),
        LuaType::DocIntegerConst(i) => LuaMemberKey::Integer(*i),
        LuaType::IntegerConst(i) => LuaMemberKey::Integer(*i),
        _ => return LuaType::Unknown,
    };

    infer_raw_member_type(db, owner, &member_key).unwrap_or(LuaType::Unknown)
}

fn instantiate_index_call(db: &DbIndex, owner: &LuaType, key: &LuaType) -> LuaType {
    match owner {
        LuaType::Union(union) => {
            let results = union
                .into_vec()
                .iter()
                .map(|member| instantiate_index_call(db, member, key))
                .collect::<Vec<_>>();
            return LuaType::from_vec(results);
        }
        LuaType::Intersection(intersection) => {
            let results = intersection
                .get_types()
                .iter()
                .map(|member| instantiate_index_call(db, member, key))
                .collect::<Vec<_>>();
            return LuaType::from_vec(results);
        }
        LuaType::Variadic(variadic) => match variadic.deref() {
            VariadicType::Base(base) => {
                return instantiate_index_call(db, base, key);
            }
            VariadicType::Multi(types) => {
                let results = types
                    .iter()
                    .map(|member| instantiate_index_call(db, member, key))
                    .collect::<Vec<_>>();
                return LuaType::from_vec(results);
            }
        },
        _ => {}
    }

    match key {
        LuaType::Union(union) => {
            let results = union
                .into_vec()
                .iter()
                .map(|member| instantiate_index_call(db, owner, member))
                .collect::<Vec<_>>();
            return LuaType::from_vec(results);
        }
        LuaType::MultiLineUnion(multi) => {
            let results = multi
                .get_unions()
                .iter()
                .map(|(member, _)| instantiate_index_call(db, owner, member))
                .collect::<Vec<_>>();
            return LuaType::from_vec(results);
        }
        LuaType::Variadic(variadic) => match variadic.deref() {
            VariadicType::Base(base) => {
                return instantiate_index_call(db, owner, base);
            }
            VariadicType::Multi(types) => {
                let results = types
                    .iter()
                    .map(|member| instantiate_index_call(db, owner, member))
                    .collect::<Vec<_>>();
                return LuaType::from_vec(results);
            }
        },
        _ => {}
    }

    if let Some(member_key) = key_type_to_member_key(key) {
        infer_raw_member_type(db, owner, &member_key).unwrap_or(LuaType::Unknown)
    } else {
        LuaType::Unknown
    }
}
