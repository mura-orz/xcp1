///	@file
///	@brief	ISO/IEC14882 C++ implementation of standard headers
///	@author	Mura
///	@copyright	(C) 2024-, Mura

// ===========================================================================
#include "new"
namespace std {

nothrow_t const 	nothrow;

static new_handler new_handler_s;
new_handler get_new_handler() noexcept { return new_handler_s; }
new_handler set_new_handler(new_handler _new_p) noexcept { auto p = new_handler_s; new_handler_s = _new_p; return p; }

}	// namespace std
// ===========================================================================
#include "exception"
namespace std {

static terminate_handler terminate_handler_s;
terminate_handler get_terminate() noexcept { return terminate_handler_s; }
terminate_handler set_terminate(terminate_handler _f) noexcept { auto f = terminate_handler_s; terminate_handler_s = _f; return f; }

}	// namespace std
// ===========================================================================
#include "typeinfo"
namespace std {

class type_info {
public:
	virtual ~type_info() {}
	bool operator==(type_info const& _rhs) const noexcept;
	bool before(type_info const& _rhs) const noexcept;
	size_t hash_code() const noexcept;
	char const* name() const noexcept;
	type_info(type_info const&) = delete;
	type_info& operator =(type_info const&) = delete;
};

class bad_cast : public exception {
public:
	char const* what() const noexcept override { return "bad cast"; }
};

class bad_typeid : public exception {
public:
	char const* what() const noexcept override { return "bad typeid"; }
};

}	// namespace std
// ===========================================================================

#include "atomic"
#include "cfloat"
#include "climits"
#include "cstdarg"
#include "cstddef"
#include "cstdint"
#include "cstdlib"
#include "initializer_list"
#include "limits"
#include "type_traits"
#include "version"
#include "compare"
#include "concepts"
#include "coroutine"
#include "bit"
