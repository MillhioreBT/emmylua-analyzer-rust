use crate::FileId;
use std::collections::{HashMap, HashSet, VecDeque};

#[derive(Debug)]
pub struct FileDependencyRelation<'a> {
    dependencies: &'a HashMap<FileId, HashSet<FileId>>,
}

impl<'a> FileDependencyRelation<'a> {
    pub fn new(dependencies: &'a HashMap<FileId, HashSet<FileId>>) -> Self {
        Self { dependencies }
    }

    pub fn get_best_analysis_order(&self, file_ids: Vec<FileId>) -> Vec<FileId> {
        let n = file_ids.len();
        if n == 0 || self.dependencies.is_empty() {
            return file_ids;
        }

        let file_to_idx: HashMap<FileId, usize> = file_ids
            .iter()
            .enumerate()
            .map(|(i, &f)| (f, i))
            .collect();

        let mut in_degree = vec![0usize; n];
        let mut adjacency: Vec<Vec<usize>> = vec![Vec::new(); n];

        for (idx, &file_id) in file_ids.iter().enumerate() {
            if let Some(deps) = self.dependencies.get(&file_id) {
                for &dep in deps {
                    if let Some(&dep_idx) = file_to_idx.get(&dep) {
                        adjacency[dep_idx].push(idx);
                        in_degree[idx] += 1;
                    }
                }
            }
        }
        let mut result = Vec::with_capacity(n);
        let mut queue = VecDeque::with_capacity(n);

        let mut zero_in_degree: Vec<usize> = (0..n).filter(|&i| in_degree[i] == 0).collect();
        zero_in_degree.sort_by_key(|&i| file_ids[i]);

        for idx in zero_in_degree {
            queue.push_back(idx);
        }

        while let Some(idx) = queue.pop_front() {
            result.push(file_ids[idx]);
            for &neighbor in &adjacency[idx] {
                in_degree[neighbor] -= 1;
                if in_degree[neighbor] == 0 {
                    queue.push_back(neighbor);
                }
            }
        }

        if result.len() < n {
            for (idx, &deg) in in_degree.iter().enumerate() {
                if deg > 0 {
                    result.push(file_ids[idx]);
                }
            }
        }

        result
    }

    /// Get all direct and indirect dependencies for the file list
    pub fn collect_file_dependents(&self, file_ids: Vec<FileId>) -> Vec<FileId> {
        let mut reverse_map: HashMap<FileId, Vec<FileId>> = HashMap::new();
        for (&fid, deps) in self.dependencies.iter() {
            for &dep in deps {
                reverse_map.entry(dep).or_default().push(fid);
            }
        }
        let mut result = HashSet::new();
        let mut queue = VecDeque::new();
        for file_id in file_ids {
            queue.push_back(file_id);
        }
        while let Some(file_id) = queue.pop_front() {
            if let Some(dependents) = reverse_map.get(&file_id) {
                for &d in dependents {
                    if result.insert(d) {
                        queue.push_back(d);
                    }
                }
            }
        }
        result.into_iter().collect()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_best_analysis_order() {
        let mut map = HashMap::new();
        // 文件1依赖文件2
        map.insert(FileId::new(1), {
            let mut s = HashSet::new();
            s.insert(FileId::new(2));
            s
        });
        // 文件2没有依赖
        map.insert(FileId::new(2), HashSet::new());
        let rel = FileDependencyRelation::new(&map);
        let result = rel.get_best_analysis_order(vec![FileId::new(1), FileId::new(2)]);
        // 文件2没有依赖，应该在前；文件1依赖文件2，在后
        assert_eq!(result, vec![FileId::new(2), FileId::new(1)]);
    }

    #[test]
    fn test_best_analysis_order2() {
        let mut map = HashMap::new();
        // 文件1依赖文件2和文件3
        map.insert(1.into(), {
            let mut s = HashSet::new();
            s.insert(2.into());
            s.insert(3.into());
            s
        });
        // 文件2依赖文件3
        map.insert(2.into(), {
            let mut s = HashSet::new();
            s.insert(3.into());
            s
        });
        // 文件3没有依赖
        map.insert(3.into(), HashSet::new());
        let rel = FileDependencyRelation::new(&map);
        let result = rel.get_best_analysis_order(vec![1.into(), 2.into(), 3.into()]);
        // 文件3没有依赖，应该在最前面；然后是2，最后是1
        assert_eq!(result, vec![3.into(), 2.into(), 1.into()]);
    }

    #[test]
    fn test_no_deps_files_first() {
        let mut map = HashMap::new();
        // 文件1依赖文件2
        map.insert(FileId::new(1), {
            let mut s = HashSet::new();
            s.insert(FileId::new(2));
            s
        });
        // 文件2依赖文件1（循环依赖）
        map.insert(FileId::new(2), {
            let mut s = HashSet::new();
            s.insert(FileId::new(1));
            s
        });
        // 文件3没有依赖
        map.insert(FileId::new(3), HashSet::new());
        // 文件4没有依赖
        map.insert(FileId::new(4), HashSet::new());

        let rel = FileDependencyRelation::new(&map);
        let result = rel.get_best_analysis_order(vec![
            FileId::new(1),
            FileId::new(2),
            FileId::new(3),
            FileId::new(4),
        ]);

        // 文件3和4没有依赖，应该在前面
        assert_eq!(result[0], FileId::new(3));
        assert_eq!(result[1], FileId::new(4));
        // 文件1和2有循环依赖，在后面
        assert!(result.contains(&FileId::new(1)));
        assert!(result.contains(&FileId::new(2)));
    }

    #[test]
    fn test_collect_file_dependents() {
        let mut deps = HashMap::new();
        deps.insert(
            FileId::new(1),
            [FileId::new(2), FileId::new(3)].iter().cloned().collect(),
        );
        deps.insert(FileId::new(2), [FileId::new(3)].iter().cloned().collect());
        deps.insert(FileId::new(3), HashSet::new());
        deps.insert(FileId::new(4), [FileId::new(3)].iter().cloned().collect());

        let rel = FileDependencyRelation::new(&deps);
        let mut result = rel.collect_file_dependents(vec![FileId::new(3)]);
        result.sort();
        assert_eq!(result, vec![FileId::new(1), FileId::new(2), FileId::new(4)]);
    }
}
