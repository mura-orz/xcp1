///	@file
///	@brief	ISO/IEC14882 C++ implementation of standard headers
///	@author	Mura
///	@copyright	(C) 2024-, Mura

#if defined(_POSIX_C_SOURCE)
#include <unistd.h>
#endif

// ===========================================================================
#include "new"
namespace std {

nothrow_t const nothrow;

static new_handler new_handler_s;
new_handler		   get_new_handler() noexcept { return new_handler_s; }
new_handler		   set_new_handler(new_handler _new_p) noexcept {
	   auto p		 = new_handler_s;
	   new_handler_s = _new_p;
	   return p;
}

}	 // namespace std
// ===========================================================================
#include "exception"
namespace std {

static terminate_handler terminate_handler_s;
terminate_handler		 get_terminate() noexcept { return terminate_handler_s; }
terminate_handler		 set_terminate(terminate_handler _f) noexcept {
	   auto f			   = terminate_handler_s;
	   terminate_handler_s = _f;
	   return f;
}

[[noreturn]] void terminate() noexcept {
#if defined(_POSIX_C_SOURCE)
	::_ezit(-1);
#endif
}	 // TODO:

}	 // namespace std
// ===========================================================================
#include "typeinfo"
namespace std {

class type_info {
public:
	virtual ~type_info() {}
	bool		operator==(type_info const& _rhs) const noexcept;
	bool		before(type_info const& _rhs) const noexcept;
	size_t		hash_code() const noexcept;
	char const* name() const noexcept;
	type_info(type_info const&)			   = delete;
	type_info& operator=(type_info const&) = delete;
};

class bad_cast : public exception {
public:
	char const* what() const noexcept override { return "bad cast"; }
};

class bad_typeid : public exception {
public:
	char const* what() const noexcept override { return "bad typeid"; }
};

}	 // namespace std
// ===========================================================================
#include "initializer_list"
namespace std {

}	 // namespace std
// ===========================================================================
#include "type_traits"
namespace std {

}	 // namespace std
// ===========================================================================
#include "atomic"
namespace std {

}	 // namespace std
// ===========================================================================
#include "cstdlib"
namespace std {

struct _exit_exception_t {
	int _result_;
	_exit_exception_t() noexcept :
		_result_{} {}
	explicit _exit_exception_t(int _result) noexcept :
		_result_{_result} {}
	int _result() const noexcept { return _result_; }
};

struct _atexit_handler_t {
	bool _cpp;
	union {
		void*				_vp;
		_C_atexit_handler	_c_handler;
		_Cpp_atexit_handler _cpp_handler;
	} _u;

	_atexit_handler_t() noexcept :
		_cpp{}, _u{} {}
};
#define _XXX_atexit_handler_count (32)	  // 32: Minimum in the C++ spec.
static _atexit_handler_t _at_exit_handlers_s[_XXX_atexit_handler_count];
static _atexit_handler_t _at_quick_exit_handlers_s[_XXX_atexit_handler_count];

template<typename T>
static void _do_atexit_handlers_(T const& handlers) noexcept {
	for (int _n = (sizeof handlers / sizeof handlers[0]) - 1; 0 <= _n; --_n) {
		if (handlers[_n]._u._vp == nullptr) { continue; }
		if (handlers[_n]._cpp) {
			handlers[_n]._u._cpp_handler();
		} else {
			handlers[_n]._u._c_handler();
		}
	}
}

[[noreturn]] void abort() noexcept {
#if defined(_POSIX_C_SOURCE)
	::_ezit(-1);
#endif
}

int atexit(_C_atexit_handler* _func) noexcept {
	if (_func == nullptr) return -1;
	for (std::size_t _n = 0u, _size = sizeof _at_exit_handlers_s / sizeof _at_exit_handlers_s[0]; _n < _size; ++_n) {
		if (_at_exit_handlers_s[_n]._u._vp != nullptr) { continue; }
		_at_exit_handlers_s[_n]._cpp		  = false;
		_at_exit_handlers_s[_n]._u._c_handler = _func;
		return 0;
	}
	return -1;
}

int atexit(_Cpp_atexit_handler* _func) noexcept {
	if (_func == nullptr) return -1;
	for (std::size_t _n = 0u, _size = sizeof _at_exit_handlers_s / sizeof _at_exit_handlers_s[0]; _n < _size; ++_n) {
		if (_at_exit_handlers_s[_n]._u._vp != nullptr) { continue; }
		_at_exit_handlers_s[_n]._cpp			= true;
		_at_exit_handlers_s[_n]._u._cpp_handler = _func;
		return 0;
	}
	return -1;
}

int at_quick_exit(_C_atexit_handler* _func) noexcept {
	if (_func == nullptr) return -1;
	for (std::size_t _n = 0u, _size = sizeof _at_quick_exit_handlers_s / sizeof _at_quick_exit_handlers_s[0]; _n < _size; ++_n) {
		if (_at_quick_exit_handlers_s[_n]._u._vp != nullptr) { continue; }
		_at_quick_exit_handlers_s[_n]._cpp			= false;
		_at_quick_exit_handlers_s[_n]._u._c_handler = _func;
		return 0;
	}
	return -1;
}

int at_quick_exit(_Cpp_atexit_handler* _func) noexcept {
	if (_func == nullptr) return -1;
	for (std::size_t _n = 0u, _size = sizeof _at_quick_exit_handlers_s / sizeof _at_quick_exit_handlers_s[0]; _n < _size; ++_n) {
		if (_at_quick_exit_handlers_s[_n]._u._vp != nullptr) { continue; }
		_at_quick_exit_handlers_s[_n]._cpp			  = true;
		_at_quick_exit_handlers_s[_n]._u._cpp_handler = _func;
		return 0;
	}
	return -1;
}

[[noreturn]] void exit(int _status) {
	_do_atexit_handlers_(_at_exit_handlers_s);
	throw _exit_exception_t{_status};
}

[[noreturn]] void _Exit(int _status) noexcept {
	throw _exit_exception_t{_status};
}

[[noreturn]] void quick_exit(int _status) noexcept {
	_do_atexit_handlers_(_at_quick_exit_handlers_s);
	throw _exit_exception_t{_status};
}

}	 // namespace std
// ===========================================================================
#include "source_location"
namespace std {

}	 // namespace std
// ===========================================================================

// These headers have no implementation.
#include "bit"
#include "cfloat"
#include "climits"
#include "compare"
#include "concepts"
#include "coroutine"
#include "cstdarg"
#include "cstddef"
#include "cstdint"
#include "limits"
#include "version"

// ===========================================================================

extern int main(int, char*[]);

int _Crt(int _Ac, char* _Av[]) {
	try {
		int _result = main(_Ac, _Av);
		return _result;
	} catch (std::_exit_exception_t const& e) {
		return e._result();
	} catch (...) {
		return -1;
	}
}

// ===========================================================================
