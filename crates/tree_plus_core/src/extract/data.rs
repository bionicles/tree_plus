//! Structured data extractors: JSON family, JSONL, YAML, TOML, CSV, SQLite.
//! Output formatting matches the legacy Python f-strings (including Python
//! `str()` / `repr()` rendering of scalars where the legacy code relied on it).

use serde_json::Value as Json;

use super::{simple::splitlines, ExtractError, ExtractResult};

fn parse_err<E: std::fmt::Display>(e: E) -> ExtractError {
    ExtractError::Parse(e.to_string())
}

/// Python `str()` of a JSON scalar/value (used inside legacy f-strings).
fn py_str(v: &Json) -> String {
    match v {
        Json::Null => "None".to_string(),
        Json::Bool(b) => if *b { "True" } else { "False" }.to_string(),
        Json::Number(n) => n.to_string(),
        Json::String(s) => s.clone(),
        other => py_repr(other),
    }
}

/// Python `repr()` of a JSON value (lists/dicts inside legacy f-strings).
fn py_repr(v: &Json) -> String {
    match v {
        Json::Null => "None".to_string(),
        Json::Bool(b) => if *b { "True" } else { "False" }.to_string(),
        Json::Number(n) => n.to_string(),
        Json::String(s) => format!("'{}'", s.replace('\\', "\\\\").replace('\'', "\\'")),
        Json::Array(items) => {
            let inner: Vec<String> = items.iter().map(py_repr).collect();
            format!("[{}]", inner.join(", "))
        }
        Json::Object(map) => {
            let inner: Vec<String> = map
                .iter()
                .map(|(k, v)| format!("'{}': {}", k, py_repr(v)))
                .collect();
            format!("{{{}}}", inner.join(", "))
        }
    }
}

/// Legacy `parse_jsonl`: first line's keys with Python type names.
pub fn extract_jsonl(content: &str) -> ExtractResult {
    let first_line = content.split('\n').next().unwrap_or("");
    let data: Json = serde_json::from_str(first_line).map_err(parse_err)?;
    let Json::Object(map) = data else {
        return Err(ExtractError::Parse("jsonl root is not an object".into()));
    };
    Ok(map
        .iter()
        .map(|(key, value)| match value {
            Json::Array(_) => format!("{key}: list"),
            Json::Object(_) => format!("{key}: dict"),
            Json::Null => format!("{key}: None"),
            Json::Bool(_) => format!("{key}: bool"),
            Json::String(_) => format!("{key}: str"),
            Json::Number(n) => {
                if n.is_f64() {
                    format!("{key}: float")
                } else {
                    format!("{key}: int")
                }
            }
        })
        .collect())
}

/// Legacy `parse_json_schema`: $schema/type/title/description keys.
pub fn extract_json_schema(content: &str) -> ExtractResult {
    let parsed: Json = serde_json::from_str(content).map_err(parse_err)?;
    let mut keepers = Vec::new();
    for key in ["$schema", "type", "title", "description"] {
        if let Some(v) = parsed.get(key) {
            keepers.push(format!("{key}: {}", py_str(v)));
        }
    }
    Ok(keepers)
}

/// Legacy `parse_package_json`: name, version, scripts.
pub fn extract_package_json(content: &str) -> ExtractResult {
    let parsed: Json = serde_json::from_str(content).map_err(parse_err)?;
    let mut keepers = Vec::new();
    keepers.push(match parsed.get("name") {
        Some(name) => format!("name: '{}'", py_str(name)),
        None => "name: ?".to_string(),
    });
    keepers.push(match parsed.get("version") {
        Some(version) => format!("version: {}", py_str(version)),
        None => "version: ?".to_string(),
    });
    if let Some(Json::Object(scripts)) = parsed.get("scripts") {
        if !scripts.is_empty() {
            keepers.push("scripts:".to_string());
            for (key, value) in scripts {
                keepers.push(format!("    {key}: '{}'", py_str(value)));
            }
        }
    }
    Ok(keepers)
}

/// Legacy `parse_json_rpc`.
pub fn extract_json_rpc(content: &str) -> ExtractResult {
    let data: Json = serde_json::from_str(content).map_err(parse_err)?;
    let get = |k: &str| data.get(k).map(py_str).unwrap_or_else(|| "N/A".to_string());
    let mut components = vec![
        format!("jsonrpc: {}", get("jsonrpc")),
        format!("method: {}", get("method")),
        "params:".to_string(),
    ];
    if let Some(Json::Object(params)) = data.get("params") {
        for (param, value) in params {
            components.push(format!("    {param}: {}", py_str(value)));
        }
    }
    components.push(format!("id: {}", get("id")));
    Ok(components)
}

/// Legacy `parse_openrpc_json`.
pub fn extract_openrpc_json(content: &str) -> ExtractResult {
    let data: Json = serde_json::from_str(content).map_err(parse_err)?;
    let str_or =
        |v: Option<&Json>, default: &str| v.map(py_str).unwrap_or_else(|| default.to_string());
    let mut components = vec![
        format!("openrpc: {}", str_or(data.get("openrpc"), "N/A")),
        "info:".to_string(),
    ];
    let info = data.get("info");
    components.push(format!(
        "    title: {}",
        str_or(info.and_then(|i| i.get("title")), "N/A")
    ));
    components.push(format!(
        "    version: {}",
        str_or(info.and_then(|i| i.get("version")), "N/A")
    ));
    components.push("methods:".to_string());
    if let Some(Json::Array(methods)) = data.get("methods") {
        for method in methods {
            let name = str_or(method.get("name"), "None");
            let desc = str_or(method.get("description"), "No description");
            components.push(format!("    {name}: {desc}"));
            components.push("        params:".to_string());
            if let Some(Json::Array(params)) = method.get("params") {
                for param in params {
                    let ptype = str_or(param.get("schema").and_then(|s| s.get("type")), "N/A");
                    let pname = str_or(param.get("name"), "None");
                    components.push(format!("            - {pname}: {ptype}"));
                }
            }
            let result = str_or(method.get("result").and_then(|r| r.get("name")), "N/A");
            let result_desc = str_or(
                method.get("result").and_then(|r| r.get("description")),
                "No description",
            );
            components.push(format!("        result: {result} = {result_desc}"));
        }
    }
    Ok(components)
}

/// Legacy `parse_csv`: header columns, or a count when there are > 11.
pub fn extract_csv(content: &str) -> ExtractResult {
    let rows = splitlines(content);
    let first = rows
        .first()
        .ok_or_else(|| ExtractError::Parse("empty csv".into()))?;
    let columns: Vec<String> = first.split(',').map(|c| c.to_string()).collect();
    if columns.len() <= 11 {
        Ok(columns)
    } else {
        Ok(vec![format!("{} columns", columns.len())])
    }
}

// --- YAML ---------------------------------------------------------------

use serde::Deserialize;
use serde_yaml::Value as Yaml;

fn yaml_str(v: &Yaml) -> String {
    match v {
        Yaml::Null => "None".to_string(),
        Yaml::Bool(b) => if *b { "True" } else { "False" }.to_string(),
        Yaml::Number(n) => n.to_string(),
        Yaml::String(s) => s.clone(),
        other => format!("{other:?}"), // legacy would have raised; rare
    }
}

fn yaml_get<'a>(v: &'a Yaml, key: &str) -> Option<&'a Yaml> {
    v.as_mapping()
        .and_then(|m| m.get(Yaml::String(key.to_string())))
}

fn yaml_has(v: &Yaml, key: &str) -> bool {
    yaml_get(v, key).is_some()
}

/// Legacy `parse_yml` with category detection.
pub fn extract_yml(content: &str) -> ExtractResult {
    let mut docs: Vec<Yaml> = Vec::new();
    for doc in serde_yaml::Deserializer::from_str(content) {
        let value = Yaml::deserialize(doc).map_err(parse_err)?;
        docs.push(value);
    }
    // python yaml.safe_load_all yields None for empty docs; an empty file
    // yields no docs at all
    if docs.is_empty() {
        return Ok(Vec::new());
    }
    let first = &docs[0];
    if yaml_has(first, "apiVersion") && yaml_has(first, "kind") && yaml_has(first, "metadata") {
        return Ok(extract_k8s(&docs).unwrap_or_else(unsupported_yaml));
    }
    if first
        .as_sequence()
        .map(|seq| {
            seq.iter()
                .any(|item| item.as_mapping().is_some_and(|_| yaml_has(item, "name")))
        })
        .unwrap_or(false)
    {
        return Ok(extract_ansible(&docs).unwrap_or_else(unsupported_yaml));
    }
    if yaml_has(first, "name") && yaml_has(first, "jobs") {
        return Ok(extract_github(&docs).unwrap_or_else(unsupported_yaml));
    }
    if yaml_has(first, "openapi") || yaml_has(first, "swagger") {
        return Ok(extract_openapi(&docs).unwrap_or_else(unsupported_yaml));
    }
    Ok(vec!["Unsupported YAML Category".to_string()])
}

fn unsupported_yaml(_: ()) -> Vec<String> {
    vec!["Unsupported YAML Category".to_string()]
}

fn extract_k8s(docs: &[Yaml]) -> Result<Vec<String>, ()> {
    let mut result = Vec::new();
    for yml in docs {
        let api = yaml_get(yml, "apiVersion").ok_or(())?;
        let kind = yaml_get(yml, "kind").ok_or(())?;
        let name = yaml_get(yml, "metadata")
            .and_then(|m| yaml_get(m, "name"))
            .ok_or(())?;
        result.push(format!(
            "{}.{} -> {}",
            yaml_str(api),
            yaml_str(kind),
            yaml_str(name)
        ));
    }
    Ok(result)
}

fn extract_ansible(docs: &[Yaml]) -> Result<Vec<String>, ()> {
    let mut result = Vec::new();
    for yml in docs {
        let seq = yml.as_sequence().ok_or(())?;
        for item in seq {
            if item.as_mapping().is_some() {
                let name = yaml_get(item, "name").map(yaml_str).unwrap_or_default();
                result.push(name);
            }
        }
    }
    Ok(result)
}

fn extract_github(docs: &[Yaml]) -> Result<Vec<String>, ()> {
    let yml = &docs[0];
    let mut result = vec![yaml_str(yaml_get(yml, "name").ok_or(())?)];
    let jobs = yaml_get(yml, "jobs")
        .and_then(|j| j.as_mapping())
        .ok_or(())?;
    for (job_key, job) in jobs {
        result.push(format!("  job: {}", yaml_str(job_key)));
        if let Some(steps) = yaml_get(job, "steps").and_then(|s| s.as_sequence()) {
            for step in steps {
                if let Some(name) = yaml_get(step, "name") {
                    result.push(format!("    - {}", yaml_str(name)));
                }
            }
        }
    }
    Ok(result)
}

fn extract_openapi(docs: &[Yaml]) -> Result<Vec<String>, ()> {
    let yml = &docs[0];
    let mut components = Vec::new();
    let openapi = yaml_get(yml, "openapi")
        .or_else(|| yaml_get(yml, "swagger"))
        .map(yaml_str)
        .unwrap_or_else(|| "N/A".to_string());
    components.push(format!("openapi: {openapi}"));
    let info = yaml_get(yml, "info");
    let title = info
        .and_then(|i| yaml_get(i, "title"))
        .map(yaml_str)
        .unwrap_or_else(|| "N/A".to_string());
    components.push(format!("    title: {title}"));
    let description = info
        .and_then(|i| yaml_get(i, "description"))
        .map(yaml_str)
        .unwrap_or_default();
    let first_sentence = if description.is_empty() {
        "N/A".to_string()
    } else {
        // legacy: regex.split(r"\.\s|\.\n", description, maxsplit=1)[0] + "."
        let split_at = description
            .char_indices()
            .find(|&(i, c)| {
                c == '.'
                    && description[i + 1..]
                        .chars()
                        .next()
                        .is_some_and(|n| n.is_whitespace())
            })
            .map(|(i, _)| i);
        match split_at {
            Some(i) => format!("{}.", &description[..i]),
            None => format!("{description}."),
        }
    };
    components.push(format!("    description: {first_sentence}"));
    let version = info
        .and_then(|i| yaml_get(i, "version"))
        .map(yaml_str)
        .unwrap_or_else(|| "N/A".to_string());
    components.push(format!("    version: {version}"));
    if let Some(servers) = yaml_get(yml, "servers").and_then(|s| s.as_sequence()) {
        if !servers.is_empty() {
            components.push("servers:".to_string());
            for server in servers {
                let url = yaml_get(server, "url")
                    .map(yaml_str)
                    .unwrap_or_else(|| "N/A".to_string());
                components.push(format!("    - url: {url}"));
            }
        }
    }
    if let Some(paths) = yaml_get(yml, "paths").and_then(|p| p.as_mapping()) {
        if !paths.is_empty() {
            components.push("paths:".to_string());
            for (path, methods) in paths {
                components.push(format!("    '{}':", yaml_str(path)));
                if let Some(methods) = methods.as_mapping() {
                    for (method, details) in methods {
                        let op = yaml_get(details, "operationId")
                            .map(yaml_str)
                            .unwrap_or_else(|| "N/A".to_string());
                        let summary = yaml_get(details, "summary")
                            .map(yaml_str)
                            .unwrap_or_else(|| "N/A".to_string());
                        components.push(format!(
                            "        {} ({op}): {summary}",
                            yaml_str(method).to_uppercase()
                        ));
                    }
                }
            }
        }
    }
    Ok(components)
}

// --- TOML ---------------------------------------------------------------

use toml::Value as Toml;

fn toml_str(v: &Toml) -> String {
    match v {
        Toml::String(s) => s.clone(),
        Toml::Integer(i) => i.to_string(),
        Toml::Float(f) => f.to_string(),
        Toml::Boolean(b) => if *b { "True" } else { "False" }.to_string(),
        other => other.to_string(),
    }
}

/// Legacy `format_dependency`.
fn format_dependency(name: &str, details: &Toml) -> String {
    match details {
        Toml::String(s) => format!("  {name} {s}"),
        Toml::Table(t) => {
            let version = t.get("version").map(toml_str).unwrap_or_default();
            let features: Vec<String> = t
                .get("features")
                .and_then(|f| f.as_array())
                .map(|arr| arr.iter().map(toml_str).collect())
                .unwrap_or_default();
            if features.is_empty() {
                format!("  {name} {version}")
            } else {
                format!("  {name} {version} (features: {})", features.join(", "))
            }
        }
        _ => format!("  {name}"),
    }
}

/// Legacy `parse_cargo_toml`.
pub fn extract_cargo_toml(content: &str) -> ExtractResult {
    let data: Toml = content.parse().map_err(parse_err)?;
    let mut components = Vec::new();
    if let Some(package) = data.get("package") {
        for key in ["name", "version", "description", "license"] {
            let value = package
                .get(key)
                .map(toml_str)
                .unwrap_or_else(|| "N/A".to_string());
            components.push(format!("{key}: {value}"));
        }
    }
    if let Some(Toml::Table(deps)) = data.get("dependencies") {
        components.push("dependencies:".to_string());
        for (dep, details) in deps {
            components.push(format_dependency(dep, details));
        }
    }
    Ok(components)
}

/// Legacy `parse_pyproject_toml`.
pub fn extract_pyproject_toml(content: &str) -> ExtractResult {
    let data: Toml = content.parse().map_err(parse_err)?;
    let mut components = Vec::new();
    let project = data.get("project");
    if let Some(project) = project {
        for key in ["name", "version", "description"] {
            let value = project
                .get(key)
                .map(toml_str)
                .unwrap_or_else(|| "N/A".to_string());
            components.push(format!("{key}: {value}"));
        }
        if let Some(classifiers) = project.get("classifiers").and_then(|c| c.as_array()) {
            for classifier in classifiers {
                let c = toml_str(classifier);
                if c.contains("License ::") {
                    components.push(c);
                }
            }
        }
        if let Some(deps) = project.get("dependencies").and_then(|d| d.as_array()) {
            components.push("dependencies:".to_string());
            for dep in deps {
                components.push(format!("    {}", toml_str(dep)));
            }
        }
    }
    Ok(components)
}

// --- SQLite ---------------------------------------------------------------

/// Legacy `parse_db`: tables and columns from a SQLite database.
#[cfg(feature = "sqlite")]
pub fn extract_sqlite(path: &std::path::Path) -> ExtractResult {
    let conn =
        rusqlite::Connection::open_with_flags(path, rusqlite::OpenFlags::SQLITE_OPEN_READ_ONLY)
            .map_err(parse_err)?;
    let mut stmt = conn
        .prepare("SELECT name FROM sqlite_master WHERE type='table';")
        .map_err(parse_err)?;
    let tables: Vec<String> = stmt
        .query_map([], |row| row.get::<_, String>(0))
        .map_err(parse_err)?
        .filter_map(Result::ok)
        .collect();
    let mut components = Vec::new();
    for table in tables {
        components.push(format!("{table} table:"));
        let mut info = conn
            .prepare(&format!("PRAGMA table_info({table});"))
            .map_err(parse_err)?;
        let rows = info
            .query_map([], |row| {
                Ok((
                    row.get::<_, String>(1)?, // name
                    row.get::<_, String>(2)?, // type
                    row.get::<_, i64>(3)?,    // notnull
                    row.get::<_, i64>(5)?,    // pk
                ))
            })
            .map_err(parse_err)?;
        for row in rows.filter_map(Result::ok) {
            let (name, ctype, not_null, pk) = row;
            let mut desc = format!("   {name} {}", ctype.to_lowercase());
            if pk == 1 {
                desc.push_str(" primary key");
            }
            if not_null == 1 && pk != 1 {
                desc.push_str(" not null");
            }
            components.push(desc);
        }
    }
    Ok(components)
}

#[cfg(not(feature = "sqlite"))]
pub fn extract_sqlite(_path: &std::path::Path) -> ExtractResult {
    Ok(Vec::new())
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn jsonl_types() {
        let line = r#"{"a": 1, "b": "x", "c": [1], "d": {"k": 1}, "e": null, "f": 1.5, "g": true}"#;
        assert_eq!(
            extract_jsonl(line).unwrap(),
            vec!["a: int", "b: str", "c: list", "d: dict", "e: None", "f: float", "g: bool"]
        );
    }

    #[test]
    fn package_json() {
        let content = r#"{"name": "pkg", "version": "1.0.0", "scripts": {"test": "jest"}}"#;
        assert_eq!(
            extract_package_json(content).unwrap(),
            vec![
                "name: 'pkg'",
                "version: 1.0.0",
                "scripts:",
                "    test: 'jest'"
            ]
        );
    }

    #[test]
    fn csv_columns() {
        assert_eq!(extract_csv("a,b,c\n1,2,3\n").unwrap(), vec!["a", "b", "c"]);
        let wide = (0..12).map(|i| i.to_string()).collect::<Vec<_>>().join(",");
        assert_eq!(extract_csv(&wide).unwrap(), vec!["12 columns"]);
    }

    #[test]
    fn github_yaml() {
        let content =
            "name: CI\njobs:\n  build:\n    steps:\n      - name: checkout\n      - run: make\n";
        assert_eq!(
            extract_yml(content).unwrap(),
            vec!["CI", "  job: build", "    - checkout"]
        );
    }

    #[test]
    fn unsupported_yaml_category() {
        assert_eq!(
            extract_yml("just: a mapping\n").unwrap(),
            vec!["Unsupported YAML Category"]
        );
    }

    #[test]
    fn cargo_toml() {
        let content = "[package]\nname = \"x\"\nversion = \"0.1.0\"\n[dependencies]\nserde = { version = \"1\", features = [\"derive\"] }\nregex = \"1\"\n";
        assert_eq!(
            extract_cargo_toml(content).unwrap(),
            vec![
                "name: x",
                "version: 0.1.0",
                "description: N/A",
                "license: N/A",
                "dependencies:",
                "  serde 1 (features: derive)",
                "  regex 1",
            ]
        );
    }
}
