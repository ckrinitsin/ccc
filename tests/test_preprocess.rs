use ccc::preprocess::preprocess;

#[test]
fn no_comments() {
    let input = "
    int main(void) {
        return 2;
    }
    ";
    let mut result = preprocess(input.to_string()).unwrap();
    result.retain(|c| !c.is_ascii_whitespace());

    let mut expected = "
    int main(void) {
        return 2;
    }
    ".to_string();
    expected.retain(|c| !c.is_ascii_whitespace());
    assert_eq!(result, expected);
}

#[test]
fn remove_two_single_line_comments() {
    let input = "
    // this is a comment
    int main(void) {
        return 2;
        // this is the second comment
    }
    ";
    let mut result = preprocess(input.to_string()).unwrap();
    result.retain(|c| !c.is_ascii_whitespace());

    let mut expected = "
    int main(void) {
        return 2;
    }
    ".to_string();
    expected.retain(|c| !c.is_ascii_whitespace());
    assert_eq!(result, expected);
}

#[test]
fn remove_inline_comment() {
    let input = "
    int main(void) { // this is a comment
        return 2;
    }
    ";
    let mut result = preprocess(input.to_string()).unwrap();
    result.retain(|c| !c.is_ascii_whitespace());

    let mut expected = "
    int main(void) {
        return 2;
    }
    ".to_string();
    expected.retain(|c| !c.is_ascii_whitespace());
    assert_eq!(result, expected);
}

#[test]
fn remove_multi_line_comments() {
    let input = "
    int main(void) {
    /* this is a comment that
     * goes through multiple
     * lines
     */
        return 2;
    }
    ";
    let mut result = preprocess(input.to_string()).unwrap();
    result.retain(|c| !c.is_ascii_whitespace());

    let mut expected = "
    int main(void) {
        return 2;
    }
    ".to_string();
    expected.retain(|c| !c.is_ascii_whitespace());
    assert_eq!(result, expected);
}
