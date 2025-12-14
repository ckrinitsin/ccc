use regex::Regex;
use anyhow::Result;

fn remove_comments(content: String) -> Result<String> {
    let re = Regex::new(r"(//.*|/\*(.|\n)*\*/)")?;
    Ok(re.replace_all(&content, "").into_owned())
}

pub fn preprocess(content: String) -> Result<String> {
    remove_comments(content)
}
