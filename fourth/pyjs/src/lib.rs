use neon::{
    prelude::*,
    types::{buffer::TypedArray, *},
};
use pyo3::{prelude::*, types::*, AsPyPointer};
trait PyResultExt {
    type Value;

    fn to_js_err<'a, C: Context<'a>>(self, cx: &mut C) -> NeonResult<Self::Value>;
}

impl<T> PyResultExt for PyResult<T> {
    type Value = T;

    fn to_js_err<'a, C: Context<'a>>(self, cx: &mut C) -> NeonResult<Self::Value> {
        self.or_else(|e| cx.throw_error(format!("Got python error: {e}")))
    }
}

#[derive(Debug, Clone)]
struct PyWrapper<T>(Py<T>);

impl<T> Finalize for PyWrapper<T> {
    // fn finalize<'a, C: Context<'a>>(self, _: &mut C) {
    //     println!("garbage collecting");
    // }
}

impl<T> std::ops::Deref for PyWrapper<T> {
    type Target = Py<T>;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

type PyJs<T = PyAny> = JsBox<PyWrapper<T>>;

trait PyObjectExt {
    type Value;

    fn to_pyjs<'a, C: Context<'a>>(self, cx: &mut C) -> Handle<'a, JsBox<PyWrapper<Self::Value>>>;
}

impl<T> PyObjectExt for Py<T> {
    type Value = T;

    fn to_pyjs<'a, C: Context<'a>>(self, cx: &mut C) -> Handle<'a, JsBox<PyWrapper<Self::Value>>> {
        JsBox::new(cx, PyWrapper(self))
    }
}

// pyo3 lacks utilities for constructing PyLong
// trait IntoPyLong {
//     fn into_py_long<'a>(py: Python<'a>, val: Self) -> &'a PyLong;
// }

// macro_rules! numerical_pylong_impl {
//     ($t: ty, $dt: ty, $m: ident) => {
//         impl IntoPyLong for $t {
//             fn into_py_long<'a>(py: Python<'a>, val: Self) -> &'a PyLong {
//                 unsafe { py.from_owned_ptr(pyo3::ffi::$m(val as $dt)) }
//             }
//         }
//     };
// }

// numerical_pylong_impl!(i8, std::ffi::c_long, PyLong_FromLong);
// numerical_pylong_impl!(i16, std::ffi::c_long, PyLong_FromLong);
// numerical_pylong_impl!(i32, std::ffi::c_long, PyLong_FromLong);
// numerical_pylong_impl!(i64, std::ffi::c_longlong, PyLong_FromLongLong);
// numerical_pylong_impl!(u8, std::ffi::c_ulong, PyLong_FromUnsignedLong);
// numerical_pylong_impl!(u16, std::ffi::c_ulong, PyLong_FromUnsignedLong);
// numerical_pylong_impl!(u32, std::ffi::c_ulong, PyLong_FromUnsignedLong);
// numerical_pylong_impl!(u64, std::ffi::c_ulonglong, PyLong_FromUnsignedLongLong);

trait PyToJs {
    type Dest: Value;

    fn to_js_val<'a, 'b, C: Context<'b>>(
        &self,
        cx: &mut C,
        py: Python<'a>,
    ) -> JsResult<'b, Self::Dest>;
}

trait JsToPy {
    type Dest;

    fn to_py_val<'a, 'b, C: Context<'b>>(
        &self,
        cx: &mut C,
        py: Python<'a>,
    ) -> NeonResult<&'a Self::Dest>;
}

impl PyToJs for PyString {
    type Dest = JsString;

    fn to_js_val<'a, 'b, C: Context<'b>>(
        &self,
        cx: &mut C,
        _py: Python<'a>,
    ) -> JsResult<'b, Self::Dest> {
        let s = self.to_str().to_js_err(cx)?;
        Ok(JsString::new(cx, s))
    }
}

impl JsToPy for JsString {
    type Dest = PyString;

    fn to_py_val<'a, 'b, C: Context<'b>>(
        &self,
        cx: &mut C,
        py: Python<'a>,
    ) -> NeonResult<&'a Self::Dest> {
        Ok(PyString::new(py, &self.value(cx)))
    }
}

impl PyToJs for PyBool {
    type Dest = JsBoolean;

    fn to_js_val<'a, 'b, C: Context<'b>>(
        &self,
        cx: &mut C,
        _py: Python<'a>,
    ) -> JsResult<'b, Self::Dest> {
        Ok(JsBoolean::new(cx, self.is_true()))
    }
}

impl JsToPy for JsBoolean {
    type Dest = PyBool;

    fn to_py_val<'a, 'b, C: Context<'b>>(
        &self,
        cx: &mut C,
        py: Python<'a>,
    ) -> NeonResult<&'a Self::Dest> {
        Ok(PyBool::new(py, self.value(cx)))
    }
}

impl PyToJs for PyFloat {
    type Dest = JsNumber;

    fn to_js_val<'a, 'b, C: Context<'b>>(
        &self,
        cx: &mut C,
        _py: Python<'a>,
    ) -> JsResult<'b, Self::Dest> {
        Ok(JsNumber::new(cx, self.value()))
    }
}

impl JsToPy for JsNumber {
    type Dest = PyAny;

    fn to_py_val<'a, 'b, C: Context<'b>>(
        &self,
        cx: &mut C,
        py: Python<'a>,
    ) -> NeonResult<&'a Self::Dest> {
        let val = self.value(cx);
        Ok(if val as u64 as f64 == val {
            (val as u64).to_object(py).into_ref(py)
        } else {
            PyFloat::new(py, self.value(cx))
        })
    }
}

impl PyToJs for PyLong {
    type Dest = JsBigInt;

    fn to_js_val<'a, 'b, C: Context<'b>>(
        &self,
        cx: &mut C,
        py: Python<'a>,
    ) -> JsResult<'b, Self::Dest> {
        let is_negative = self.le(0u8.to_object(py)).to_js_err(cx)?;
        let num = if is_negative {
            unsafe { py.from_owned_ptr(pyo3::ffi::PyNumber_Negative(self.as_ptr())) }
        } else {
            self
        };
        let bit_count = unsafe { pyo3::ffi::_PyLong_NumBits(num.as_ptr()) } as usize;
        let byte_count = if bit_count % 8 == 0 {
            bit_count / 8
        } else {
            bit_count / 8 + 1
        };
        let mut bytes = vec![0u8; byte_count];
        let ret = unsafe {
            pyo3::ffi::_PyLong_AsByteArray(num.as_ptr() as _, bytes.as_mut_ptr(), byte_count, 1, 0)
        };
        if ret == -1 {
            if let Some(err) = PyErr::take(py) {
                Err(err).to_js_err(cx)?;
            } else {
                cx.throw_error("Unknown python error in _PyLong_AsByteArray occured.")?;
            }
        }
        let mut digits = Vec::with_capacity((byte_count + 3) / 4);
        for chunk in bytes.chunks(8) {
            let mut arr = [0; 8];
            arr[..chunk.len()].copy_from_slice(chunk);
            digits.push(u64::from_le_bytes(arr));
        }
        Ok(JsBigInt::from_digits_le(
            cx,
            if is_negative {
                bigint::Sign::Negative
            } else {
                bigint::Sign::Positive
            },
            &digits,
        ))
    }
}

impl JsToPy for JsBigInt {
    type Dest = PyLong;

    fn to_py_val<'a, 'b, C: Context<'b>>(
        &self,
        cx: &mut C,
        py: Python<'a>,
    ) -> NeonResult<&'a Self::Dest> {
        let (sign, digits) = self.to_digits_le(cx);
        let bytes: Vec<_> = digits.into_iter().flat_map(u64::to_le_bytes).collect();
        let ret = unsafe { pyo3::ffi::_PyLong_FromByteArray(bytes.as_ptr(), bytes.len(), 1, 0) };
        if ret == std::ptr::null_mut() {
            if let Some(err) = PyErr::take(py) {
                Err(err).to_js_err(cx)?;
            } else {
                cx.throw_error("Unknown python error in _PyLong_FromByteArray occurred.")?;
            }
        }
        let num = if sign == bigint::Sign::Negative {
            unsafe {
                let neg = pyo3::ffi::PyNumber_Negative(ret);
                pyo3::ffi::Py_DECREF(ret);
                py.from_owned_ptr(neg)
            }
        } else {
            unsafe { py.from_owned_ptr(ret) }
        };
        Ok(num)
    }
}

impl PyToJs for PyBytes {
    type Dest = JsUint8Array;

    fn to_js_val<'a, 'b, C: Context<'b>>(
        &self,
        cx: &mut C,
        _py: Python<'a>,
    ) -> JsResult<'b, Self::Dest> {
        JsUint8Array::from_slice(cx, self.as_bytes())
    }
}

impl JsToPy for JsUint8Array {
    type Dest = PyBytes;

    fn to_py_val<'a, 'b, C: Context<'b>>(
        &self,
        cx: &mut C,
        py: Python<'a>,
    ) -> NeonResult<&'a Self::Dest> {
        Ok(PyBytes::new(py, self.as_slice(cx)))
    }
}

impl PyToJs for PyList {
    type Dest = JsArray;

    fn to_js_val<'a, 'b, C: Context<'b>>(
        &self,
        cx: &mut C,
        py: Python<'a>,
    ) -> JsResult<'b, Self::Dest> {
        let len = self
            .len()
            .try_into()
            .or_else(|_| cx.throw_range_error("Python list is too long to convert to JS array"))?;
        let ret = JsArray::new(cx, len);
        for i in 0..len {
            let item = self.get_item(i as usize).to_js_err(cx)?.to_js_val(cx, py)?;
            ret.set(cx, i, item)?;
        }
        Ok(ret)
    }
}

impl PyToJs for PyTuple {
    type Dest = JsArray;

    fn to_js_val<'a, 'b, C: Context<'b>>(
        &self,
        cx: &mut C,
        py: Python<'a>,
    ) -> JsResult<'b, Self::Dest> {
        let len = self
            .len()
            .try_into()
            .or_else(|_| cx.throw_range_error("Python list is too long to convert to JS array"))?;
        let ret = JsArray::new(cx, len);
        for i in 0..len {
            let item = self.get_item(i as usize).to_js_err(cx)?.to_js_val(cx, py)?;
            ret.set(cx, i, item)?;
        }
        Ok(ret)
    }
}

impl PyToJs for PySet {
    type Dest = JsObject;

    fn to_js_val<'a, 'b, C: Context<'b>>(
        &self,
        cx: &mut C,
        py: Python<'a>,
    ) -> JsResult<'b, Self::Dest> {
        let set_class = cx.global().get::<JsFunction, _, _>(cx, "Set")?;
        let len = self
            .len()
            .try_into()
            .or_else(|_| cx.throw_range_error("Python set is too long to convert to JS array"))?;
        let ret = JsArray::new(cx, len);
        for (i, v) in self.iter().enumerate() {
            let val = v.to_js_val(cx, py)?;
            ret.set(cx, i as u32, val)?;
        }
        let args = [ret.as_value(cx)];
        set_class.construct(cx, &args)
    }
}

impl PyToJs for PyDict {
    type Dest = JsObject;

    fn to_js_val<'a, 'b, C: Context<'b>>(
        &self,
        cx: &mut C,
        py: Python<'a>,
    ) -> JsResult<'b, Self::Dest> {
        let map_class = cx.global().get::<JsFunction, _, _>(cx, "Map")?;
        let len = self
            .len()
            .try_into()
            .or_else(|_| cx.throw_range_error("Python dict is too long to convert to JS array"))?;
        let ret = JsArray::new(cx, len);
        for (i, (k, v)) in self.iter().enumerate() {
            let pair = JsArray::new(cx, 2);
            let key = k.to_js_val(cx, py)?;
            let val = v.to_js_val(cx, py)?;
            pair.set(cx, 0, key)?;
            pair.set(cx, 1, val)?;
            ret.set(cx, i as u32, pair)?;
        }
        let args = [ret.as_value(cx)];
        map_class.construct(cx, &args)
    }
}

impl JsToPy for JsArray {
    type Dest = PyList;

    fn to_py_val<'a, 'b, C: Context<'b>>(
        &self,
        cx: &mut C,
        py: Python<'a>,
    ) -> NeonResult<&'a Self::Dest> {
        Ok(PyList::new(
            py,
            self.to_vec(cx)?
                .into_iter()
                .map(|x| x.to_py_val(cx, py))
                .collect::<Result<Vec<_>, _>>()?,
        ))
    }
}

impl JsToPy for JsNull {
    type Dest = PyAny;

    fn to_py_val<'a, 'b, C: Context<'b>>(
        &self,
        _cx: &mut C,
        py: Python<'a>,
    ) -> NeonResult<&'a Self::Dest> {
        Ok(py.None().into_ref(py))
    }
}

impl PyToJs for PyAny {
    type Dest = JsValue;

    fn to_js_val<'a, 'b, C: Context<'b>>(
        &self,
        cx: &mut C,
        py: Python<'a>,
    ) -> JsResult<'b, Self::Dest> {
        macro_rules! add_impl {
            ($t: ty) => {
                if let Ok(x) = self.downcast::<$t>() {
                    return x.to_js_val(cx, py).map(|x| x.as_value(cx));
                }
            };
        }
        add_impl!(PyString);
        add_impl!(PyFloat);
        add_impl!(PyBytes);
        add_impl!(PyList);
        add_impl!(PyTuple);
        add_impl!(PyLong);
        add_impl!(PyBool);
        add_impl!(PySet);
        add_impl!(PyDict);
        if self.is_none() {
            return Ok(cx.null().as_value(cx));
        }
        Ok(self.to_object(self.py()).to_pyjs(cx).as_value(cx))
    }
}

impl JsToPy for PyJs {
    type Dest = PyAny;

    fn to_py_val<'a, 'b, C: Context<'b>>(
        &self,
        _cx: &mut C,
        py: Python<'a>,
    ) -> NeonResult<&'a Self::Dest> {
        Ok((self as &Py<PyAny>).clone().into_ref(py))
    }
}

impl<'h> JsToPy for Handle<'h, JsValue> {
    type Dest = PyAny;

    fn to_py_val<'a, 'b, C: Context<'b>>(
        &self,
        cx: &mut C,
        py: Python<'a>,
    ) -> NeonResult<&'a Self::Dest> {
        macro_rules! add_impl {
            ($t: ty) => {
                if let Ok(x) = self.downcast::<$t, _>(cx) {
                    return x.to_py_val(cx, py).map(|x| &x as _);
                }
            };
        }
        add_impl!(JsString);
        add_impl!(JsNumber);
        add_impl!(JsUint8Array);
        add_impl!(JsNull);
        add_impl!(JsArray);
        add_impl!(JsBigInt);
        add_impl!(JsBoolean);
        add_impl!(PyJs);
        let self_obj = self.downcast::<JsObject, _>(cx).ok();
        let cons = self_obj
            .and_then(|x| x.get_value(cx, "constructor").ok())
            .and_then(|cons| cons.downcast::<JsFunction, _>(cx).ok());
        if let (Some(obj), Some(val_cons)) = (self_obj, cons) {
            if let Ok(map_cons) = cx.global().get::<JsFunction, _, _>(cx, "Map") {
                if map_cons.strict_equals(cx, val_cons) {
                    let d = PyDict::new(py);
                    let iter = obj
                        .get::<JsFunction, _, _>(cx, "entries")?
                        .call(cx, obj, &[])?
                        .downcast_or_throw::<JsObject, _>(cx)?;
                    let next = iter.get::<JsFunction, _, _>(cx, "next")?;
                    loop {
                        let elem = next
                            .call(cx, iter, &[])?
                            .downcast_or_throw::<JsObject, _>(cx)?;
                        let is_done = elem.get::<JsBoolean, _, _>(cx, "done")?;
                        if is_done.value(cx) {
                            break;
                        } else {
                            let entry = elem.get::<JsArray, _, _>(cx, "value")?;
                            let k = entry.get::<JsValue, _, _>(cx, 0)?;
                            let v = entry.get::<JsValue, _, _>(cx, 1)?;
                            let key = k.to_py_val(cx, py)?;
                            let val = v.to_py_val(cx, py)?;
                            d.set_item(key, val).to_js_err(cx)?;
                        }
                    }
                    return Ok(d);
                }
            }
            if let Ok(set_cons) = cx.global().get::<JsFunction, _, _>(cx, "Set") {
                if set_cons.strict_equals(cx, val_cons) {
                    let mut items: Vec<&PyAny> = Vec::new();
                    let iter = obj
                        .get::<JsFunction, _, _>(cx, "values")?
                        .call(cx, obj, &[])?
                        .downcast_or_throw::<JsObject, _>(cx)?;
                    let next = iter.get::<JsFunction, _, _>(cx, "next")?;
                    loop {
                        let elem = next
                            .call(cx, iter, &[])?
                            .downcast_or_throw::<JsObject, _>(cx)?;
                        let is_done = elem.get::<JsBoolean, _, _>(cx, "done")?;
                        if is_done.value(cx) {
                            break;
                        } else {
                            let v = elem.get::<JsValue, _, _>(cx, "value")?;
                            let val = v.to_py_val(cx, py)?;
                            items.push(val);
                        }
                    }
                    return Ok(PySet::new(py, items).to_js_err(cx)?);
                }
            }
        }
        let type_name = cons
            .and_then(|cons| cons.get_value(cx, "name").ok())
            .and_then(|name| name.downcast::<JsString, _>(cx).ok())
            .map(|name| name.value(cx));
        return cx.throw_type_error(format!(
            "JS value of type {} could not be converted to Python value",
            type_name.as_deref().unwrap_or("<unknown>")
        ));
    }
}

// impl PyToJs for PyString {
//     type Dest = JsString;

//     fn into_js<'a, C: Context<'a>>(&self, cx: &mut C) -> PyResult<Handle<'a, Self::Dest>> {
//         Ok(JsString::new(cx, &self.to_str()?))
//     }
// }

fn py_eval(mut cx: FunctionContext) -> JsResult<PyJs> {
    let arg = cx.argument::<JsString>(0)?;
    Python::with_gil(|py| {
        let res = py.eval(&arg.value(&mut cx), None, None)?;
        Ok(res.to_object(py).to_pyjs(&mut cx))
    })
    .to_js_err(&mut cx)
}

fn py_exec(mut cx: FunctionContext) -> JsResult<JsUndefined> {
    let arg = cx.argument::<JsString>(0)?;
    Python::with_gil(|py| {
        py.run(&arg.value(&mut cx), None, None)?;
        Ok(JsUndefined::new(&mut cx))
    })
    .to_js_err(&mut cx)
}

fn py_import(mut cx: FunctionContext) -> JsResult<PyJs> {
    let arg = cx.argument::<JsString>(0)?;
    Python::with_gil(|py| {
        let res = py.import(PyString::new(py, &arg.value(&mut cx)))?;
        Ok(res.to_object(py).to_pyjs(&mut cx))
    })
    .to_js_err(&mut cx)
}

fn py_str(mut cx: FunctionContext) -> JsResult<JsString> {
    let arg = cx.argument::<PyJs>(0)?;
    Python::with_gil(|py| {
        arg.as_ref(py)
            .str()
            .to_js_err(&mut cx)?
            .to_js_val(&mut cx, py)
    })
}

fn py_repr(mut cx: FunctionContext) -> JsResult<JsString> {
    let arg = cx.argument::<PyJs>(0)?;
    Python::with_gil(|py| {
        arg.as_ref(py)
            .repr()
            .to_js_err(&mut cx)?
            .to_js_val(&mut cx, py)
    })
}

fn py_getattr(mut cx: FunctionContext) -> JsResult<PyJs> {
    let arg = cx.argument::<PyJs>(0)?;
    let attr = cx.argument::<JsString>(1)?;
    Python::with_gil(|py| {
        Ok(arg
            .as_ref(py)
            .getattr(attr.to_py_val(&mut cx, py)?)
            .to_js_err(&mut cx)?
            .to_object(py)
            .to_pyjs(&mut cx))
    })
}

fn py_hasattr(mut cx: FunctionContext) -> JsResult<JsBoolean> {
    let arg = cx.argument::<PyJs>(0)?;
    let attr = cx.argument::<JsString>(1)?;
    Python::with_gil(|py| {
        let val = arg
            .as_ref(py)
            .hasattr(attr.to_py_val(&mut cx, py)?)
            .to_js_err(&mut cx)?;
        Ok(JsBoolean::new(&mut cx, val))
    })
}

fn py_tuple(mut cx: FunctionContext) -> JsResult<PyJs> {
    let arg = cx.argument::<JsArray>(0)?;
    Python::with_gil(|py| {
        Ok(PyTuple::new(
            py,
            arg.to_vec(&mut cx)?
                .into_iter()
                .map(|x| x.to_py_val(&mut cx, py))
                .collect::<Result<Vec<_>, _>>()?,
        )
        .to_object(py)
        .to_pyjs(&mut cx))
    })
}

fn py_call(mut cx: FunctionContext) -> JsResult<PyJs> {
    let func = cx.argument::<PyJs>(0)?;
    let args = cx.argument::<PyJs>(1)?;
    let kwargs = cx.argument::<PyJs>(2)?;
    Python::with_gil(|py| {
        Ok(func
            .as_ref(py)
            .call(
                args.as_ref(py).downcast::<PyTuple>().or_else(|_| {
                    cx.throw_type_error("py_call expects a tuple as the first argument")
                })?,
                Some(kwargs.as_ref(py).downcast::<PyDict>().or_else(|_| {
                    cx.throw_type_error("py_call expects a dict as the second argument")
                })?),
            )
            .to_js_err(&mut cx)?
            .to_object(py)
            .to_pyjs(&mut cx))
    })
}

fn js_to_py(mut cx: FunctionContext) -> JsResult<PyJs> {
    let arg = cx.argument_opt(0).ok_or_else(|| {
        cx.throw_type_error::<_, ()>("js_to_py expects 1 argument, none given")
            .unwrap_err()
    })?;
    Python::with_gil(|py| {
        arg.to_py_val(&mut cx, py)
            .map(|x| x.to_object(py).to_pyjs(&mut cx))
    })
}

fn py_to_js(mut cx: FunctionContext) -> JsResult<JsValue> {
    let arg = cx.argument::<PyJs>(0)?;
    Python::with_gil(|py| arg.as_ref(py).to_js_val(&mut cx, py))
}

#[neon::main]
fn main(mut cx: ModuleContext) -> NeonResult<()> {
    cx.export_function("py_eval", py_eval)?;
    cx.export_function("py_exec", py_exec)?;
    cx.export_function("py_import", py_import)?;
    cx.export_function("py_str", py_str)?;
    cx.export_function("py_repr", py_repr)?;
    cx.export_function("py_getattr", py_getattr)?;
    cx.export_function("py_hasattr", py_hasattr)?;
    cx.export_function("py_tuple", py_tuple)?;
    cx.export_function("py_call", py_call)?;
    cx.export_function("js_to_py", js_to_py)?;
    cx.export_function("py_to_js", py_to_js)?;
    Ok(())
}
