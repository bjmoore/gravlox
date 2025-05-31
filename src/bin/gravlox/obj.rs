use std::cell::RefCell;
use std::rc::Rc;

pub type Obj<T> = Rc<RefCell<T>>;

pub fn make_obj<T>(inner: T) -> Obj<T> {
    Rc::new(RefCell::new(inner))
}
