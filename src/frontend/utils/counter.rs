use std::sync::atomic::{AtomicUsize, Ordering};

static IR_COUNTER_TMP: AtomicUsize = AtomicUsize::new(0);
static COUNTER_VAR: AtomicUsize = AtomicUsize::new(0);
static COUNTER_LABEL: AtomicUsize = AtomicUsize::new(0);

pub fn gen_tmp() -> String {
    let counter = IR_COUNTER_TMP.fetch_add(1, Ordering::SeqCst);
    "tmp.".to_string() + &counter.to_string()
}

pub fn gen_var_local(id: String) -> String {
    let counter = COUNTER_VAR.fetch_add(1, Ordering::SeqCst);
    "var.".to_string() + &id.to_string() + "." + &counter.to_string()
}

pub fn gen_label() -> String {
    let counter = COUNTER_LABEL.fetch_add(1, Ordering::SeqCst);
    "label.".to_string() + &counter.to_string()
}

pub fn gen_named_label(id: String) -> String {
    let counter = COUNTER_LABEL.fetch_add(1, Ordering::SeqCst);
    "label.".to_string() + &id.to_string() + "." + &counter.to_string()
}

pub fn reset_counter() {
    IR_COUNTER_TMP.store(0, Ordering::SeqCst);
    COUNTER_LABEL.store(0, Ordering::SeqCst);
    COUNTER_VAR.store(0, Ordering::SeqCst);
}
