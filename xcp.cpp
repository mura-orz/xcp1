///	@file
///	@brief	xcp - xxx c++ compiler.
///		It is a C++20 compiler written in C++20.
///		It s just for hobby and does not take care of performance.
///	@author		Mura
///	@copyright	(c) 2023-, Mura.

#include <source_location>
#include <string_view>
#include <unordered_map>
#include <unordered_set>
#include <algorithm>
#include <chrono>
#include <exception>
#include <filesystem>
#include <fstream>
#include <iomanip>
#include <iostream>
#include <list>
#include <locale>
#include <mutex>
#include <numeric>
#include <optional>
#include <ranges>
#include <regex>
#include <set>
#include <sstream>
#include <stack>
#include <stdexcept>
#include <string>
#include <tuple>
#include <variant>
#include <vector>

namespace xxx {
using namespace std::string_literals;
using namespace std::string_view_literals;

using svmatch = std::match_results<std::string_view::const_iterator>;

template<typename E = std::invalid_argument>
inline void check(bool result, std::string const& message = std::string{}, std::source_location const& sl = std::source_location::current()) {
	if (! result) throw E{sl.function_name() + std::to_string(sl.line()) + message};
}

inline std::string escape(std::string_view const& s, std::string_view::size_type limit = std::string_view::npos) {
	std::unordered_map<char, std::string_view> const escaped{
		{'\r', "$r"sv},
		{'\n', "$n"sv},
		{'\t', "$t"sv},
		{'\f', "$f"sv},
		{'\v', "$v"sv},
		{'\a', "$a"sv},
		{'\b', "$b"sv},
		{'\\', "$$"sv},
	};
	if (limit == std::string_view::npos) {
		return std::accumulate(std::ranges::begin(s), std::ranges::end(s), std::move(std::ostringstream()), [&escaped](auto&& o, auto const& a) { if (escaped.contains(a)) { o << escaped.at(a); } else { o << a; } return std::move(o); }).str();
	} else {
		auto const str = s | std::views::take(std::min(limit, s.length()));
		return std::accumulate(std::ranges::begin(str), std::ranges::end(str), std::move(std::ostringstream()), [&escaped](auto&& o, auto const& a) { if (escaped.contains(a)) { o << escaped.at(a); } else { o << a; } return std::move(o); }).str();
	}
}

template<typename C, typename T>
inline bool contains(C& container, T const& value) {
	return std::ranges::find(container, value) != std::ranges::end(container);
}

namespace log {

enum class level_t {
	Verbose,
	Trace,
	Info,
	Error,
	Silent
};

static std::mutex mutex_s;
static level_t	  level_s = level_t::Silent;

namespace impl {

char const*		 Lv[]{"[*]", "[T]", "[I]", "[E]", "[S]"};
std::regex const function_name_re{R"(^(?:[^ ]+ )*(?:`?[A-Za-z_{][-A-Za-z_0-9<>'}]*::)*(~?[A-Za-z_<][A-Za-z_0-9<>]*) ?\(.*$)"};

inline std::tm local_tm(std::chrono::system_clock::time_point const& now) {
	std::tm	   tm{};
	auto const tt{std::chrono::system_clock::to_time_t(now)};
	// ::localtime_s(&tm, &tt);     for xxx_win32
	// ::localtime_r(&tt, &tm);     for xxx_posix
	// NOTE: It is always called with lock of mutex currently.
	tm = *std::localtime(&tt);
	return tm;
}

inline std::string location(std::source_location const& sl) {
	std::ostringstream oss;
	oss << "[" << std::filesystem::path{sl.file_name()}.filename().string() << ":" << std::setfill('_') << std::setw(5) << sl.line() << ":" << std::setw(3) << sl.column() << "]";
	std::string_view fn{sl.function_name()};
	if (svmatch m; std::regex_match(fn.cbegin(), fn.cend(), m, function_name_re)) {
		oss << m.str(1);
	} else {
		oss << sl.function_name();
	}
	oss << " ";
	return oss.str();
}

inline std::string datetime(std::chrono::system_clock::time_point const& dt) {
	std::ostringstream oss;
	using namespace std::chrono_literals;
	auto const lt = local_tm(dt);
	auto const ms = std::chrono::duration_cast<std::chrono::microseconds>(dt.time_since_epoch()) % 1s;
	oss << std::put_time(&lt, "%FT%T.") << std::setfill('0') << std::setw(6) << ms.count() << std::put_time(&lt, "%z");
	return oss.str();
}

}	 // namespace impl

inline void log(level_t level, std::string_view const& message, std::source_location sl = std::source_location::current()) {
	if (level < level_s) return;
	std::lock_guard lock{mutex_s};
	std::clog << impl::datetime(std::chrono::system_clock::now()) << impl::Lv[static_cast<int>(level)] << impl::location(sl) << std::string{message} << std::endl;
}
inline void trace(std::string_view const& message, std::source_location sl = std::source_location::current()) { log(level_t::Trace, message, sl); }
inline void info(std::string_view const& message, std::source_location sl = std::source_location::current()) { log(level_t::Info, message, sl); }
inline void err(std::string_view const& message, std::source_location sl = std::source_location::current()) { log(level_t::Error, message, sl); }

class tracer_t {
public:
	explicit tracer_t(std::vector<std::string_view> const& args, bool silent = false, std::source_location sl = std::source_location::current()) :
		tracer_t(level_t::Trace, args, silent, sl) {}
	tracer_t(level_t level, std::vector<std::string_view> const& args, bool silent = false, std::source_location sl = std::source_location::current()) :
		level_{level}, sl_{sl}, result_{}, silent_{silent} {
		if (! silent_ && level_s <= level_) log(level_, ">>>>(" + std::reduce(std::ranges::begin(args), std::ranges::end(args), std::string{}, [](auto const& lhs, auto const& rhs) { return std::string{lhs} + (lhs.empty() ? "" : ",") + std::string{rhs}; }) + ")", sl_);
	}
	~tracer_t() {
		if (! silent_ && level_s <= level_) log(level_, "<<<<(" + result_ + ") ", sl_);
	}

	template<typename T>
	void trace(T const& v, bool force = false, std::source_location sl = std::source_location::current()) {
		if (! force && (silent_ || level_ < level_s)) return;

		if constexpr (std::is_integral_v<T>) {
			log(level_, "----" + std::to_string(sl.line()) + "|" + std::to_string(v) + "|", sl_);
		} else {
			std::ostringstream oss;
			oss << v;
			log(level_, "----" + std::to_string(sl.line()) + "|" + oss.str() + "|", sl_);
		}
	}

	template<typename T>
	void set_result(T const& v) {
		if constexpr (std::is_integral_v<T>) {
			result_ = std::to_string(v);
		} else {
			std::ostringstream oss;
			oss << v;
			result_ = oss.str();
		}
	}

private:
	level_t				 level_;
	std::source_location sl_;
	std::string			 result_;
	bool				 silent_;
};

template<>
inline void tracer_t::set_result(std::string const& v) { result_ = v; }
template<>
inline void tracer_t::set_result(std::string_view const& v) { result_ = std::string{v}; }
template<>
inline void tracer_t::set_result(std::vector<std::string_view> const& v) {
	result_ = std::reduce(std::ranges::begin(v), std::ranges::end(v), std::string{}, [](auto const& lhs, auto const& rhs) { return std::string{lhs} + (lhs.empty() ? "" : ",") + std::string{rhs}; });
}

template<>
inline void tracer_t::trace(std::string const& v, bool force, std::source_location sl) {
	if (! force && (silent_ || level_ < level_s)) return;
	log(level_, "----" + std::to_string(sl.line()) + "|" + v + "|", sl_);
}
template<>
inline void tracer_t::trace(std::string_view const& v, bool force, std::source_location sl) {
	if (! force && (silent_ || level_ < level_s)) return;
	log(level_, "----" + std::to_string(sl.line()) + "|" + std::string{v} + "|", sl_);
}
template<>
inline void tracer_t::trace(std::vector<std::string_view> const& v, bool force, std::source_location sl) {
	if (! force && (silent_ || level_ < level_s)) return;
	auto const vs = std::reduce(std::ranges::begin(v), std::ranges::end(v), std::string{}, [](auto const& lhs, auto const& rhs) { return std::string{lhs} + (lhs.empty() ? "" : ",") + std::string{rhs}; });
	log(level_, "----" + std::to_string(sl.line()) + "|" + vs + "|", sl_);
}

}	 // namespace log

std::string load_file(std::filesystem::path const& path) {
	log::tracer_t tr{{path.string()}};

	// -------------------------------
	// Opens the file.
	std::ifstream ifs;
	ifs.exceptions(std::ios::failbit | std::ios::badbit);
	ifs.open(path);
	ifs.exceptions(std::ios::failbit);
	// -------------------------------
	// Reads the file.
	std::stringstream ss;
	std::copy(std::istreambuf_iterator<char>(ifs), std::istreambuf_iterator<char>(), std::ostreambuf_iterator<char>(ss));
	auto const raw{ss.str()};
	// -------------------------------
	// Removes BOM if exists.
	auto const bom{"\xEF\xBB\xBF"sv};
	auto const s = raw.starts_with(bom) ? raw.substr(bom.size()) : raw;
	return s.ends_with("\n") ? s : s + "\n";	// The contents is always terminated by a new line.
}

namespace lex {
namespace def {

auto const and_s_="and"s;
auto const and_eq_s_="and_eq"s;
auto const or_s_="or"s;
auto const or_eq_s_="or_eq"s;
auto const xor_s_="xor"s;
auto const xor_eq_s_="xor_eq"s;
auto const not_s_="not"s;
auto const not_eq_s_="not_eq"s;
auto const compl_s_="compl"s;
auto const bitand_s_="bitand"s;
auto const bitor_s_="bitor"s;

auto const  alignas_s_=	"alignas"s;
auto const  alignof_s_=	"alignof"s;
auto const  asm_s_=	"asm"s;
auto const  auto_s_=	"auto"s;
auto const  bool_s_=	"bool"s;
auto const  break_s_=	"break"s;
auto const  case_s_=	"case"s;
auto const  catch_s_=	"catch"s;
auto const  char_s_=	"char"s;
auto const  char8_t_s_=	"char8_t"s;
auto const  char16_t_s_=	"char16_t"s;
auto const  char32_t_s_=	"char32_t"s;
auto const  class_s_=	"class"s;
auto const  concept_s_=	"concept"s;
auto const  const_s_=	"const"s;
auto const  constexpr_s_=	"constexpr"s;
auto const  consteval_s_=	"consteval"s;
auto const  constinit_s_=	"constinit"s;
auto const  const_cast_s_=	"const_cast"s;
auto const  co_await_s_=	"co_await"s;
auto const  co_return_s_=	"co_return"s;
auto const  co_yield_s_=	"co_yield"s;
auto const  continue_s_=	"continue"s;
auto const  decltype_s_=	"decltype"s;
auto const  default_s_=	"default"s;
auto const  do_s_=	"do"s;
auto const  delete_s_=	"delete"s;
auto const  double_s_=	"double"s;
auto const  dynamic_cast_s_=	"dynamic_cast"s;
auto const  else_s_=	"else"s;
auto const  enum_s_=	"enum"s;
auto const  explicit_s_=	"explicit"s;
auto const  export_s_=	"export"s;
auto const  extern_s_=	"extern"s;
auto const  false_s_=	"false"s;
auto const  float_s_=	"float"s;
auto const  for_s_=	"for"s;
auto const  friend_s_=	"friend"s;
auto const  goto_s_=	"goto"s;
auto const  if_s_=	"if"s;
auto const  inline_s_=	"inline"s;
auto const  int_s_=	"int"s;
auto const  long_s_=	"long"s;
auto const  mutable_s_=	"mutable"s;
auto const  namespace_s_=	"namespace"s;
auto const  new_s_=	"new"s;
auto const  noexcept_s_=	"noexcept"s;
auto const  nullptr_s_=	"nullptr"s;
auto const  operator_s_=	"operator"s;
auto const  private_s_=	"private"s;
auto const  protected_s_=	"protected"s;
auto const  public_s_=	"public"s;
auto const  reinterpret_cast_s_=	"reinterpret_cast"s;
auto const  requires_s_=	"requires"s;
auto const  return_s_=	"return"s;
auto const  register_s_=	"register"s;
auto const  short_s_=	"short"s;
auto const  signed_s_=	"signed"s;
auto const  sizeof_s_=	"sizeof"s;
auto const  static_s_=	"static"s;
auto const  static_assert_s_=	"static_assert"s;
auto const  static_cast_s_=	"static_cast"s;
auto const  struct_s_=	"struct"s;
auto const  switch_s_=	"switch"s;
auto const  template_s_=	"template"s;
auto const  this_s_=	"this"s;
auto const  thread_local_s_=	"thread_local"s;
auto const  throw_s_=	"throw"s;
auto const  typedef_s_=	"typedef"s;
auto const  typeid_s_=	"typeid"s;
auto const  typename_s_=	"typename"s;
auto const  try_s_=	"try"s;
auto const  true_s_=	"true"s;
auto const  union_s_=	"union"s;
auto const  unsigned_s_=	"unsigned"s;
auto const  using_s_=	"using"s;
auto const  virtual_s_=	"virtual"s;
auto const  void_s_=	"void"s;
auto const  volatile_s_=	"volatile"s;
auto const  wchar_t_s_=	"wchar_t"s;
auto const  while_s_=	"while"s;

auto const  override_s_=	"override"s;
auto const  final_s_=	"final"s;

// literal list
std::unordered_set<std::string_view> const alternative_expressions{and_s_, and_eq_s_, bitand_s_, bitor_s_, compl_s_, not_s_, not_eq_s_, or_s_, or_eq_s_, xor_s_, xor_eq_s_};
std::unordered_set<std::string_view> const keywords{
	alignas_s_, alignof_s_, asm_s_, auto_s_,

	bool_s_, break_s_,

	case_s_, catch_s_, char_s_, char16_t_s_, char32_t_s_, class_s_, const_s_, constexpr_s_, const_cast_s_, continue_s_,

	decltype_s_, default_s_, do_s_, delete_s_, double_s_, dynamic_cast_s_,

	else_s_, enum_s_, explicit_s_, export_s_, extern_s_,

	false_s_, float_s_, for_s_, friend_s_,

	goto_s_,

	if_s_, inline_s_, int_s_,

	long_s_,

	mutable_s_,

	namespace_s_, new_s_, noexcept_s_, nullptr_s_,

	operator_s_,

	private_s_, protected_s_, public_s_,

	return_s_, reinterpret_cast_s_, register_s_,

	short_s_, signed_s_, sizeof_s_, static_s_, static_assert_s_, static_cast_s_, struct_s_, switch_s_,

	template_s_, this_s_, thread_local_s_, throw_s_, typedef_s_, typeid_s_, typename_s_, try_s_, true_s_,

	union_s_, unsigned_s_, using_s_,

	virtual_s_, void_s_, volatile_s_,

	wchar_t_s_, while_s_,
};

// regex rules
std::regex const newline_re{R"(^(\r?\n))"};
std::regex const escaped_newline_re{R"(^(\\\r?\n))"};
std::regex const line_comment_re{R"(^(//[^\r\n]*(\r?\n)))"};
std::regex const block_comment_re{R"(^(/[*](?:[^*]|[*][^/]|^|$|[\r\n])*[*]/))"};	// workaround for multiline regex
std::regex const inline_whitespaces_re{R"(^([ \t\v\f]+))"};
std::regex const identifier_re{R"(^([a-zA-Z_][a-zA-Z_0-9]*))"};
std::regex const pp_number_re{R"(^([-+]?[.]?\d(?:\d|')*(?:['a-zA-Z_0-9]+|[eEpP][-+]|[.])?))"};
std::regex const string_literal_re{R"(^((?:u8?|[UL])?"(?:\\(?:[?'"abfnrtv\\]|u[0-9a-fA-f]{4}|U[0-9a-fA-f]{8})|[^"\r\n])*"[a-zA-Z_0-9]*))"};
std::regex const character_literal_re{R"(^((?:u8?|[UL])?'(?:\\(?:[?'"abfnrtv\\]|u[0-9a-fA-f]{4}|U[0-9a-fA-f]{8})|[^'\r\n])'))"};

std::regex const header_name_re{R"(^(<[^>\r\n]+>))"};
// The "..." (double-quoted literal) is also matched by string_literal_re, so there is not defined here.
// On the other hand, the <...> is also matched to template or other expressions: i.e., 1 < a && b > 1.
// In such cases, its result has to be replaced again.

std::regex const raw_string_literal_prefix_re{R"(^(?:u8?|[UL])?R"([^()\\\r\n\f\v ]*)\()"};

std::regex const preprocessing_op_re{
	"^("
	R"([.]{3}|(?:%:){1,2})"						 // ... %:%: %:
	R"(|\[|\]|(?:[.]|->)[*]?)"					 // [ ] . .* -> ->*
	R"(|&&?|-[-=]?|\+[+=]?|##?|:[:>]?|\|\|?)"	 // && || -- ++ ## :: & | - + # : += -= :>
	R"(|>>?=?|<(?::|<?=?))"						 // >>= <<= >> << >= <= > < <:
	R"(|[*/%^&|~!=]=?)"							 // *= /= %= ^= &= |= ~= != == * / % ^ & | ~ ! =
	R"(|[;?,])"									 // ; ? ,
	")"};										 // ( new delete and and_eq bitand bitor compl not not_eq or or_eq xor xor_eq )
std::regex const preprocessing_punc_re{
	"^("
	R"([{}()]|<%|%>)"	 // { } ( ) <% %>
	")"};

// character set
std::string_view const basic_source_character_set{"ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789_{}[]#()<>%:;.?*+-/^&|~!=,\\\"’ \t\v\f\r\n"};

std::unordered_set<std::string_view> const unary_op_set{"+", "-", "~", "*", "&", "#", "%:", "++", "--", "compl", "not"};
std::unordered_set<std::string_view> const binary_op_set{"##", "%:%:", "&&", "&", "||", "|", "-", "+", "+=", "-=", ">>=", "<<=", ">>", "<<", "<=", ">=", ">", "<", "*=", "/=", "%=", "^=", "&=", "|=", "~=", "!=", "==", "*", "/", "%", "^", "&", "=", "and", "and_eq", "bitand", "bitor", "not_eq", "or", "or_eq", "xor", "xor_eq"};
std::unordered_set<std::string_view> const trinary_op_set{"?", ":"};

}	 // namespace def

/// @brief      Lexical token type.
enum class token_type_t {
	Terminated,
	Failure,
	Identifier,
	Number,
	Raw_string,
	String,
	Character,
	Operator,
	Separator,
	Keyword,
	Block_comment,
	Line_comment,
	Whitespace,
	Newline,
	Header,
};

inline std::string to_string(token_type_t t) {
	using enum token_type_t;
	std::unordered_map<token_type_t, std::string> const names{
		{Terminated, "Terminated"},
		{Failure, "Failure"},
		{Identifier, "Identifier"},
		{Number, "Number"},
		{Raw_string, "Raw_string"},
		{String, "String"},
		{Character, "Character"},
		{Operator, "Operator"},
		{Separator, "Separator"},
		{Keyword, "Keyword"},
		{Block_comment, "Block_comment"},
		{Line_comment, "Line_comment"},
		{Whitespace, "Whitespace"},
		{Newline, "Newline"},
		{Header, "Header"},
	};
	return names.find(t)->second + "("s + std::to_string(static_cast<int>(t)) + ")"s;
}

inline std::ostream& operator<<(std::ostream& os, token_type_t t) {
	os << to_string(t);
	return os;
}

class pos_t {
public:
	auto line() const noexcept { return line_; }
	auto column() const noexcept { return column_; }
	auto file() const { return file_; }

	void				set_line(std::size_t value) noexcept { line_ = value; }
	void				set_column(std::size_t value) noexcept { column_ = value; }
	void				set_file(std::shared_ptr<std::filesystem::path const> value = std::shared_ptr<std::filesystem::path const>{}) { file_ = value; }
	[[nodiscard]] pos_t moved(std::size_t length) const { return {line_, column_ + length, file_}; }

	pos_t() :
		line_{}, column_{}, file_{} {}
	pos_t(std::size_t line, std::size_t column, std::shared_ptr<std::filesystem::path const> file = std::shared_ptr<std::filesystem::path const>{}) :
		line_{line}, column_{column}, file_{file} {}

private:
	std::size_t									 line_;
	std::size_t									 column_;
	std::shared_ptr<std::filesystem::path const> file_;
};

inline bool is_unary_op(std::string_view const& op) {
	log::tracer_t tr{{std::string{op}}};

	auto const result = def::unary_op_set.contains(op);
	tr.set_result(result);
	return result;
}
inline bool is_binary_op(std::string_view const& op) {
	log::tracer_t tr{{std::string{op}}};

	auto const result = def::binary_op_set.contains(op);
	tr.set_result(result);
	return result;
}
inline bool is_trinary_op(std::string_view const& op) {
	log::tracer_t tr{{std::string{op}}};

	auto const result = def::trinary_op_set.contains(op);
	tr.set_result(result);
	return result;
}

template<typename I>
inline I skip_ws(I pos, I const& end) {
	log::tracer_t tr{{}, true};
	using enum token_type_t;
	for (; pos != end; ++pos) {
		switch (pos->type()) {
		// -------------------------------
		// Terminates this line.
		case Failure: [[fallthrough]];
		case Terminated: [[fallthrough]];
		case Newline: return end;
		// -------------------------------
		// Skips whitespaces.
		case Block_comment: [[fallthrough]];
		case Line_comment: [[fallthrough]];
		case Whitespace: continue;
		// -------------------------------
		// Returns the next available token.
		default: return pos;
		}
	}
	return pos;
}

/// @brief      Lexical token.
class token_t {
public:
	using hideset_t = std::set<std::list<token_t>::const_iterator>;

	auto token() const noexcept { return token_; }
	auto type() const noexcept { return type_; }
	auto line() const noexcept { return pos_.line(); }
	auto column() const noexcept { return pos_.column(); }
	auto file() const noexcept { return pos_.file(); }

	auto&		hideset() noexcept { return hideset_; }
	auto const& hideset() const noexcept { return hideset_; }

	auto const& pos() const noexcept { return pos_; }

	void pos(std::size_t line, std::shared_ptr<std::filesystem::path const> path = std::shared_ptr<std::filesystem::path const>{}) {
		pos_.set_line(line);
		pos_.set_column(0u);
		pos_.set_file(path);
	}

	bool matched(token_type_t type, std::string_view token) const noexcept { return type_ == type && token_ == token; }
	bool matched(token_type_t type) const noexcept { return type_ == type; }

	token_t(token_type_t type, std::string_view token) :
		type_{type}, token_{token}, pos_{0, 0}, hideset_{} {}
	token_t(token_type_t type, std::string_view token, pos_t const& pos) :
		type_{type}, token_{token}, pos_{pos}, hideset_{} {
		check<std::logic_error>(! ! file());
	}
	token_t(token_type_t type, std::string_view token, std::set<token_t> const& hideset) :
		type_{type}, token_{token}, pos_{0, 0}, hideset_{hideset} {}
	token_t(token_type_t type, std::string_view token, std::set<token_t> const& hideset, pos_t const& pos) :
		type_{type}, token_{token}, pos_{pos}, hideset_{hideset} {
		check<std::logic_error>(! ! file());
	}

private:
	token_type_t	  type_;
	std::string_view  token_;
	pos_t			  pos_;
	std::set<token_t> hideset_;
};
inline bool operator==(token_t const& lhs, token_t const& rhs) { return lhs.type() == rhs.type() && lhs.token() == rhs.token(); }
inline bool operator<(token_t const& lhs, token_t const& rhs) { return lhs.type() < rhs.type() || (lhs.token() < rhs.token()); }

inline std::string to_string(pos_t const& pos) {
	if (! pos.file()) return "{no position}";

	std::ostringstream oss;
	oss << "[" << pos.file()->string() << "(l:" << std::to_string(pos.line()) << " c:" << std::to_string(pos.column()) << ")" << "]";
	return oss.str();
}

inline std::string to_string(token_t const& token) {
	std::ostringstream oss;
	oss << to_string(token.pos()) << token.type() << " (" << token.token().length() << ") =[" << xxx::escape(token.token()) << "]=";
	return oss.str();
}

/// @brief              Gets next token from source literal.
/// @param[in]  str             Source string literal, which have to be available while parsing results exist.
/// @return             The first is token type. The last string is parsed token, which is substring of the @p str.
inline std::tuple<token_type_t, std::string_view> next_token(std::string_view const& str, bool noheader = false) {
	log::tracer_t tr{{escape(str, 32)}, true};
	using enum token_type_t;
	if (str.empty()) return {Terminated, str};
	svmatch result;

	// It checks the first character once before using regex for performance
	// because parsing by complex regex is too slow.

	// Raw string is especial token because it is determined by d-char-sequence dynamically.
	// To parse the raw string, it separates two parts: prefix and suffix.
	// Regex rules of the suffix is made here according to its prefix.

	if (auto const ch = str.at(0); std::isdigit(ch)) {
		// -------------------------------
		// pp-number
		tr.trace(ch);
		if (std::regex_search(std::ranges::begin(str), std::ranges::end(str), result, def::pp_number_re)) return {Number, str.substr(0, result.length(1))};
	} else if (std::isalpha(ch) || ch == '_') {
		// -------------------------------
		// maybe identifier, keyword, or prefix.
		tr.trace(ch);
		switch (ch) {
		// -------------------------------
		// raw-string-literal
		case 'R':
			if (std::regex_search(std::ranges::begin(str), std::ranges::end(str), result, def::raw_string_literal_prefix_re)) {
				auto const suffix = ")" + result.str(1) + "\"";
				if (auto const pos = str.find(suffix, result.length(0)); pos != std::string_view::npos) return {Raw_string, str.substr(0, pos + suffix.length())};
				return {Failure, str.substr(0, 0)};
			}
			break;
		// -------------------------------
		// prefixes of strings or character literal
		case 'u': [[fallthrough]];
		case 'U': [[fallthrough]];
		case 'L':
			if (std::regex_search(std::ranges::begin(str), std::ranges::end(str), result, def::raw_string_literal_prefix_re)) {
				auto const suffix = ")" + result.str(1) + "\"";
				if (auto const pos = str.find(suffix, result.length(0)); pos != std::string_view::npos) return {Raw_string, str.substr(0, pos + suffix.length())};
				return {Failure, str.substr(0, 0)};
			}
			if (std::regex_search(std::ranges::begin(str), std::ranges::end(str), result, def::string_literal_re)) return {String, str.substr(0, result.length(1))};
			if (std::regex_search(std::ranges::begin(str), std::ranges::end(str), result, def::character_literal_re)) return {Character, str.substr(0, result.length(1))};
			break;
		}
		// -------------------------------
		// identifier or keyword (including alternative token)
		if (std::regex_search(std::ranges::begin(str), std::ranges::end(str), result, def::identifier_re)) {
			if (def::alternative_expressions.contains(result.str())) return {Operator, str.substr(0, result.length(1))};
			if (def::keywords.contains(result.str(1))) return {Keyword, str.substr(0, result.length(1))};
			return {Identifier, str.substr(0, result.length(1))};
		}
	} else if (std::isblank(ch) || std::iscntrl(ch)) {
		// -------------------------------
		// white-spaces regardless of escape
		tr.trace(ch);
		if (std::regex_search(std::ranges::begin(str), std::ranges::end(str), result, def::escaped_newline_re)) return {Whitespace, str.substr(0, result.length(1))};
		if (std::regex_search(std::ranges::begin(str), std::ranges::end(str), result, def::newline_re)) return {Newline, str.substr(0, result.length(1))};
		if (std::regex_search(std::ranges::begin(str), std::ranges::end(str), result, def::inline_whitespaces_re)) return {Whitespace, str.substr(0, result.length(1))};
	} else if (ch == '/') {
		// -------------------------------
		// comments or operator
		tr.trace(ch);
		if (std::regex_search(std::ranges::begin(str), std::ranges::end(str), result, def::block_comment_re)) return {Block_comment, str.substr(0, result.length(1))};
		if (std::regex_search(std::ranges::begin(str), std::ranges::end(str), result, def::line_comment_re)) return {Line_comment, str.substr(0, result.length(1))};
		if (std::regex_search(std::ranges::begin(str), std::ranges::end(str), result, def::preprocessing_op_re)) return {Operator, str.substr(0, result.length(1))};
		if (std::regex_search(std::ranges::begin(str), std::ranges::end(str), result, def::preprocessing_punc_re)) return {Separator, str.substr(0, result.length(1))};
	} else {
		// -------------------------------
		// string, character, (header), or pp-number
		// The header depends on context.
		tr.trace(ch);
		switch (ch) {
		case '"':
			if (std::regex_search(std::ranges::begin(str), std::ranges::end(str), result, def::string_literal_re)) return {String, str.substr(0, result.length(1))};
			break;
		case '<':
			if (! noheader && std::regex_search(std::ranges::begin(str), std::ranges::end(str), result, def::header_name_re)) return {Header, str.substr(0, result.length(1))};
			break;
		case '\'':
			if (std::regex_search(std::ranges::begin(str), std::ranges::end(str), result, def::character_literal_re)) return {Character, str.substr(0, result.length(1))};
			break;
		case '+': [[fallthrough]];
		case '-': [[fallthrough]];
		case '.':
			if (std::regex_search(std::ranges::begin(str), std::ranges::end(str), result, def::pp_number_re)) return {Number, str.substr(0, result.length(1))};
			break;
		}
		// -------------------------------
		// separator
		if (std::regex_search(std::ranges::begin(str), std::ranges::end(str), result, def::preprocessing_op_re)) return {Operator, str.substr(0, result.length(1))};
		if (std::regex_search(std::ranges::begin(str), std::ranges::end(str), result, def::preprocessing_punc_re)) return {Separator, str.substr(0, result.length(1))};
	}
	// -------------------------------
	// Other character, for example Japanese.
	// TODO: Such character shold handle as an universal-character.
	tr.trace(str.at(0));
	return {Failure, str.substr(0, 0)};
}

inline std::tuple<token_t, pos_t> next_token(std::string_view const& str, pos_t const& pos, bool noheader = false) {
	log::tracer_t tr{{to_string(pos), escape(str, 32)}, true};

	using enum token_type_t;
	switch (auto const [type, token]{next_token(str, noheader)}; type) {
	case Failure: tr.set_result(to_string(Failure)); throw std::runtime_error{"Invalid token:" + to_string(pos)};
	case Block_comment: [[fallthrough]];
	case Raw_string:
		tr.set_result(std::string{to_string(type)} + ":" + escape(token, 32));
		// TODO: Ths implementation does not take care of escaped new line here.
		if (auto const lf = std::ranges::count(token, '\n'); 0u < lf) {
			return {token_t{type, token, pos}, pos_t{pos.line() + lf, token.length() - token.find_last_of("\n"), pos.file()}};
		} else {
			return {token_t{type, token, pos}, pos.moved(token.length())};
		}
	case Line_comment: [[fallthrough]];
	case Newline:
		tr.set_result(std::string{to_string(type)} + ":" + escape(token, 32));
		return {token_t{type, token, pos}, pos_t{pos.line() + 1u, 1u, pos.file()}};
	default:
		tr.set_result(std::string{to_string(type)} + ":" + escape(token, 32));
		return {token_t{type, token, pos}, pos.moved(token.length())};
	}
}

std::list<token_t> scan(std::string_view const& str, std::filesystem::path const& name) {
	log::tracer_t tr{{name.string(), escape(str, 32)}, true};

	using enum token_type_t;
	pos_t pos{1u, 1u, std::make_shared<std::filesystem::path>(name)};
	// This implementation does not use recursive call here to avoid risk of stack overflow.
	std::list<token_t> tokens;
	for (auto s = str; ! s.empty(); s = s.substr(tokens.back().token().length())) {
		auto [token, next] = next_token(s, pos);
		tokens.push_back(token);
		switch (tokens.back().type()) {
		case Header:
			// Correct header is following include directive only.
			tr.trace("Header may appear.");
			if (auto const ritr = skip_ws(++tokens.crbegin(), tokens.crend()); ritr == tokens.crend() || ! ritr->matched(lex::token_type_t::Identifier, "include")) {
				tr.trace("It is not a header");
				// It is not the correct header.For example, it might be just an arithmetic comparison expression. e.g., (a < 1 && b > 1)
				// Drops it once and takes it again excluding header.
				tokens.erase(--tokens.rbegin().base());
				auto [nonheader, n] = next_token(s, pos, true);

				next = n;
				tokens.push_back(nonheader);
			}
			break;
		default: break;
		}
		pos = next;
	}
	return tokens;
}

}	 // namespace lex
namespace cxx::ast {

class node_t;

}	 // namespace cxx::ast
namespace pp {

using tokens_t		= std::list<lex::token_t>;
using tokens_itr_t	= tokens_t::iterator;
using tokens_citr_t = tokens_t::const_iterator;
using line_tokens_t = std::pair<tokens_citr_t, tokens_citr_t>;
using lines_t		= std::vector<line_tokens_t>;
using strs_t		= std::vector<std::string_view>;	// TODO:

namespace impl {

lines_t split_lines(tokens_t const& tokens) {
	log::tracer_t tr{{}};

	if (tokens.empty()) return lines_t{};
	auto const newline = [](auto const& a) { return a.type() == lex::token_type_t::Newline || a.type() == lex::token_type_t::Line_comment; };

	// This implementation does not use recursive call here to avoid risk of stack overflow.
	lines_t lines;
	lines.reserve(tokens.back().line() + 1u);
	// Source is always terminated by the LF.
	for (auto itr = std::ranges::begin(tokens), end = std::ranges::end(tokens), next = std::find_if(itr, end, newline); itr != end && next != end; itr = ++next, next = std::find_if(itr, end, newline)) {
		lines.emplace_back(std::make_pair(itr, next));
	}

	tr.set_result(lines.size());
	return lines;
}

inline tokens_citr_t next_nonws(tokens_citr_t pos, tokens_citr_t end) { return pos == end ? end : lex::skip_ws(++pos, end); }

}	 // namespace impl
namespace pm {

class path_manager_t {
public:
	auto const&		 path() const { return current_.top()->path; }
	std::string_view source() const { return current_.top()->source; }
	auto const&		 tokens() const { return current_.top()->tokens; }
	auto const&		 preprocessing_tokens() const { return current_.top()->lines; }
	auto			 nodes() const { return current_.top()->node; }

	void source(std::string_view str) { current_.top()->source = str; }
	void tokens(pp::tokens_t const& tokens) { current_.top()->tokens = tokens; }
	void tokens(pp::tokens_t&& tokens) { current_.top()->tokens = std::move(tokens); }
	void preprocessing_tokens(pp::lines_t const& pp_tokens) { current_.top()->lines = pp_tokens; }
	void preprocessing_tokens(pp::lines_t&& pp_tokens) { current_.top()->lines = std::move(pp_tokens); }
	void node(std::shared_ptr<cxx::ast::node_t> nodes) const { current_.top()->node = std::move(nodes); }

	auto& mutable_tokens() {
		if (current_.empty()) throw std::runtime_error(__func__);
		return current_.top()->tokens;
	}

	std::optional<std::filesystem::path> find(std::filesystem::path const& header, bool includes_current_path) const {
		auto dir = path();
		dir.remove_filename();
		if (includes_current_path && std::filesystem::exists(dir / header)) return dir / header;
		auto const paths = includes_ | std::views::transform([&header](auto const& a) { return a / header; });
		if (auto const itr = std::ranges::find_if(paths, [](auto const& a) { return std::filesystem::exists(a); }); itr != std::ranges::end(paths)) { return *itr; }
		return std::nullopt;
	}
	void push(std::filesystem::path const& hpath) {
		paths_.emplace_back(std::make_shared<file_t>(hpath, std::string{}, pp::tokens_t{}, pp::lines_t{}, nullptr));
		current_.push(paths_.back());
	}
	void pop() { current_.pop(); }

public:
	path_manager_t(std::vector<std::filesystem::path> const& includes, std::vector<std::filesystem::path> const& linkages, std::vector<std::string> const& libraries) :
		paths_{}, current_{}, includes_{includes}, linkages_{linkages}, libraries_{libraries} {
	}

private:
	void set_default_paths() {
		// -------------------------------
		// Default path to search including headers.
		includes_.emplace_back("/usr/include");
		includes_.emplace_back("/usr/local/include");

		// -------------------------------
		// Default path to search linking libraries.
		linkages_.emplace_back("/usr/lib");
		linkages_.emplace_back("/usr/local/lib");

		// -------------------------------
		// Standard libraries.
		libraries_.emplace_back("c++");		   // libc++
		libraries_.emplace_back("pthread");	   // libpthread
	}

private:
	struct file_t {
		std::filesystem::path			  path;
		std::string						  source;
		pp::tokens_t					  tokens;
		pp::lines_t						  lines;
		std::shared_ptr<cxx::ast::node_t> node;
	};

	std::vector<std::shared_ptr<file_t>> paths_;		///<    @brief  Stack of current paths.
	std::stack<std::shared_ptr<file_t>>	 current_;		///<    @brief  Stack of current paths.
	std::vector<std::filesystem::path>	 includes_;		///<    @brief  Paths to search headers.
	std::vector<std::filesystem::path>	 linkages_;		///<    @brief  Paths to search libraries.
	std::vector<std::string>			 libraries_;	///<    @brief  Name of libraries.
};

}	 // namespace pm
namespace cm {
class condition_manager_t {
public:
	bool available() const noexcept { return conditions_.empty() || conditions_.top(); }
	bool empty() const noexcept { return conditions_.empty(); }
	void push(bool condition) { conditions_.push(condition); }
	void flip() {
		check<std::runtime_error>(! conditions_.empty());
		conditions_.top() = ! conditions_.top();
	}
	void pop() {
		check<std::runtime_error>(! conditions_.empty());
		conditions_.pop();
	}

	condition_manager_t() :
		conditions_{} {}

private:
	std::stack<bool> conditions_;
};

}	 // namespace cm
namespace mm {
namespace def {

static auto const value_xcp_version = pp::tokens_t{{lex::token_type_t::Number, "00020000L"}};
static auto const value_empty		= pp::tokens_t{{lex::token_type_t::String, ""}};

static auto const value_0 = pp::tokens_t{{lex::token_type_t::Number, "0"}};
static auto const value_1 = pp::tokens_t{{lex::token_type_t::Number, "1"}};
static auto const value_4 = pp::tokens_t{{lex::token_type_t::Number, "4"}};

static auto const value_199901 = pp::tokens_t{{lex::token_type_t::Number, "199901L"}};
// static auto const value_200604 = pp::tokens_t{{lex::token_type_t::Number, "200604L"}};
// static auto const value_200610 = pp::tokens_t{{lex::token_type_t::Number, "200610L"}};
// static auto const value_200704 = pp::tokens_t{{lex::token_type_t::Number, "200704L"}};
// static auto const value_200707 = pp::tokens_t{{lex::token_type_t::Number, "200707L"}};
// static auto const value_200710 = pp::tokens_t{{lex::token_type_t::Number, "200710L"}};
// static auto const value_200806 = pp::tokens_t{{lex::token_type_t::Number, "200806L"}};
// static auto const value_200809 = pp::tokens_t{{lex::token_type_t::Number, "200809L"}};
// static auto const value_200907 = pp::tokens_t{{lex::token_type_t::Number, "200907L"}};
// static auto const value_201304 = pp::tokens_t{{lex::token_type_t::Number, "201304L"}};
// static auto const value_201309 = pp::tokens_t{{lex::token_type_t::Number, "201309L"}};
// static auto const value_201411 = pp::tokens_t{{lex::token_type_t::Number, "201411L"}};
// static auto const value_201510 = pp::tokens_t{{lex::token_type_t::Number, "201510L"}};
// static auto const value_201511 = pp::tokens_t{{lex::token_type_t::Number, "201511L"}};
// static auto const value_201611 = pp::tokens_t{{lex::token_type_t::Number, "201611L"}};
// static auto const value_201603 = pp::tokens_t{{lex::token_type_t::Number, "201603L"}};
// static auto const value_201606 = pp::tokens_t{{lex::token_type_t::Number, "201606L"}};
// static auto const value_201707 = pp::tokens_t{{lex::token_type_t::Number, "201707L"}};
// static auto const value_201711 = pp::tokens_t{{lex::token_type_t::Number, "201711L"}};
// static auto const value_201803 = pp::tokens_t{{lex::token_type_t::Number, "201803L"}};
// static auto const value_201806 = pp::tokens_t{{lex::token_type_t::Number, "201806L"}};
// static auto const value_201811 = pp::tokens_t{{lex::token_type_t::Number, "201811L"}};
// static auto const value_201902 = pp::tokens_t{{lex::token_type_t::Number, "201902L"}};
// static auto const value_201907 = pp::tokens_t{{lex::token_type_t::Number, "201907L"}};
// static auto const value_201911 = pp::tokens_t{{lex::token_type_t::Number, "201911L"}};
static auto const value_202002 = pp::tokens_t{{lex::token_type_t::Number, "202002L"}};

inline pp::line_tokens_t to_line_tokens(pp::tokens_t const& tokens) {
	return std::make_pair(tokens.begin(), tokens.end());
}

static auto const macro_xcp_version = to_line_tokens(value_xcp_version);
static auto const macro_empty		= to_line_tokens(value_empty);

static auto const macro_0 = to_line_tokens(value_0);
static auto const macro_1 = to_line_tokens(value_1);
static auto const macro_4 = to_line_tokens(value_4);

static auto const macro_199901 = to_line_tokens(value_199901);
// static auto const macro_200604 	= to_line_tokens(value_200604);
// static auto const macro_200610	= to_line_tokens(value_200610);
// static auto const macro_200704	= to_line_tokens(value_200704);
// static auto const macro_200707	= to_line_tokens(value_200707);
// static auto const macro_200710	= to_line_tokens(value_200710);
// static auto const macro_200806	= to_line_tokens(value_200806);
// static auto const macro_200809	= to_line_tokens(value_200809);
// static auto const macro_200907	= to_line_tokens(value_200907);
// static auto const macro_201304	= to_line_tokens(value_201304);
// static auto const macro_201309	= to_line_tokens(value_201309);
// static auto const macro_201411	= to_line_tokens(value_201411);
// static auto const macro_201510	= to_line_tokens(value_201510);
// static auto const macro_201511	= to_line_tokens(value_201511);
// static auto const macro_201603	= to_line_tokens(value_201603);
// static auto const macro_201606	= to_line_tokens(value_201606);
// static auto const macro_201611	= to_line_tokens(value_201611);
// static auto const macro_201707	= to_line_tokens(value_201707);
// static auto const macro_201711	= to_line_tokens(value_201711);
// static auto const macro_201803	= to_line_tokens(value_201803);
// static auto const macro_201806	= to_line_tokens(value_201806);
// static auto const macro_201811	= to_line_tokens(value_201811);
// static auto const macro_201902	= to_line_tokens(value_201902);
// static auto const macro_201907	= to_line_tokens(value_201907);
// static auto const macro_201911	= to_line_tokens(value_201911);
static auto const macro_202002 = to_line_tokens(value_202002);

}	 // namespace def

using parameters_t = std::list<lex::token_t>;
using arguments_t  = std::list<lex::token_t>;
using hideset_t	   = std::set<lex::token_t>;

inline tokens_t operator+(tokens_t const& lhs, tokens_t const& rhs) {
	tokens_t ts;
	std::ranges::copy(lhs, std::inserter(ts, std::ranges::end(ts)));
	std::ranges::copy(rhs, std::inserter(ts, std::ranges::end(ts)));
	return ts;
}
inline hideset_t operator+(hideset_t const& lhs, hideset_t const& rhs) {
	hideset_t hs;
	std::ranges::copy(lhs, std::inserter(hs, std::ranges::end(hs)));
	std::ranges::copy(rhs, std::inserter(hs, std::ranges::end(hs)));
	return hs;
}

class macro_manager_t {
public:
	using macro_parameters_t = std::vector<tokens_citr_t>;
	using values_t			 = line_tokens_t;
	using simple_macros_t	 = std::unordered_map<std::string_view, line_tokens_t>;
	using function_macro_t	 = std::pair<macro_parameters_t, values_t>;
	using function_macros_t	 = std::unordered_map<std::string_view, function_macro_t>;

public:
	auto const& value(std::string_view const& name) const noexcept { return simple_macros_.at(name); }							 // TODO:
	auto const& function_value(std::string_view const& name) const noexcept { return function_macros_.at(name).first; }			 // TODO:
	auto const& function_parameters(std::string_view const& name) const noexcept { return function_macros_.at(name).second; }	 // TODO:

	void define_simple_macro(std::string_view const& name, values_t const& value) {
		simple_macros_[name] = value;
		if (function_macros_.contains(name)) {
			function_macros_.erase(name);	 // Overrides it if exists.
		}
	}
	void define_faction_macro(std::string_view const& name, macro_parameters_t const& arguments, values_t const& value) {
		function_macros_[name] = std::make_pair(arguments, value);
		if (simple_macros_.contains(name)) {
			simple_macros_.erase(name);	   // Overrides it if exists.
		}
	}
	bool defined(std::string_view const& name) const noexcept { return simple_macros_.contains(name) || function_macros_.contains(name); }
	bool defined_simple_macro(std::string_view const& name) const noexcept { return simple_macros_.contains(name); }
	bool defined_function_macro(std::string_view const& name) const noexcept { return function_macros_.contains(name); }
	bool undefine_macro(std::string_view const& name) { return simple_macros_.erase(name) ? true : function_macros_.erase(name); }

public:
	macro_manager_t() :
		value_date_{}, value_time_{}, simple_macros_{}, function_macros_{} { set_predefined_macros(); }
	macro_manager_t(simple_macros_t const& simple_macros, function_macros_t const& function_macros) :
		value_date_{}, value_time_{}, simple_macros_{simple_macros}, function_macros_{function_macros} { set_predefined_macros(); }

private:
	void set_predefined_macros() {
		// -------------------------------
		// Implementation-defined
		simple_macros_["_XCP"] = def::macro_xcp_version;

		// -------------------------------
		// C++ Standards

		{	 // Dynamic run-time values
			auto const set_datetime = [this](std::string_view const& macro, std::tm const& tm, std::string const& format, std::string& str, pp::tokens_t& value) {
				std::ostringstream oss;
				oss << std::put_time(&tm, format.c_str());
				str	  = oss.str();
				value = {{lex::token_type_t::String, str}};

				simple_macros_[macro] = def::to_line_tokens(value);
			};
			auto const now = std::chrono::system_clock::to_time_t(std::chrono::system_clock::now());
			auto const tm  = *std::localtime(&now);	   // The returning tm is just copied as local variable instead of localtime_r because here is single-thread yet.

			set_datetime("__DATE__", tm, "%b %d %Y", str_date_, value_date_);	 // "Mmm dd yyyy"
			set_datetime("__TIME__", tm, "%T", str_time_, value_time_);			 // "hh:mm:ss"
		}

		// Static compile-time values
		simple_macros_["__cplusplus"]					   = def::macro_202002;
		simple_macros_["__FILE__"]						   = def::macro_empty;	  // placeholder
		simple_macros_["__LINE__"]						   = def::macro_empty;	  // placeholder
		simple_macros_["__STDC_HOSTED__"]				   = def::macro_1;
		simple_macros_["__STDCPP_DEFAULT_NEW_ALIGNMENT__"] = def::macro_4;

		// C++ features.
		// simple_macros_["__cpp_aggregate_bases"]						= def::macro_201603;
		// simple_macros_["__cpp_aggregate_nsdmi"]						= def::macro_201304;
		// simple_macros_["__cpp_aggregate_paren_init"]					= def::macro_201902;
		// simple_macros_["__cpp_alias_templates"]						= def::macro_200704;
		// simple_macros_["__cpp_aligned_new"]							= def::macro_201606;
		// simple_macros_["__cpp_attributes"]							= def::macro_200809;
		// simple_macros_["__cpp_binary_literals"]						= def::macro_201304;
		// simple_macros_["__cpp_capture_star_this"]					= def::macro_201603;
		// simple_macros_["__cpp_char8_t"]								= def::macro_201811;
		// simple_macros_["__cpp_concepts "]							= def::macro_201907;
		// simple_macros_["__cpp_conditional_explicit "]				= def::macro_201806;
		// simple_macros_["__cpp_constexpr "]							= def::macro_201907;
		// simple_macros_["__cpp_constexpr_dynamic_alloc "]				= def::macro_201907;
		// simple_macros_["__cpp_constexpr_in_decltype "]				= def::macro_201711;
		// simple_macros_["__cpp_consteval "]							= def::macro_201811;
		// simple_macros_["__cpp_constinit "]							= def::macro_201907;
		// simple_macros_["__cpp_decltype "]							= def::macro_200707;
		// simple_macros_["__cpp_decltype_auto "]						= def::macro_201304;
		// simple_macros_["__cpp_deduction_guides"]						= def::macro_201907;
		// simple_macros_["__cpp_delegating_constructors "]				= def::macro_200604;
		// simple_macros_["__cpp_designated_initializers "]				= def::macro_201707;
		// simple_macros_["__cpp_enumerator_attributes "]				= def::macro_201411;
		// simple_macros_["__cpp_fold_expressions "]					= def::macro_201603;
		// simple_macros_["__cpp_generic_lambdas "]						= def::macro_201707;
		// simple_macros_["__cpp_guaranteed_copy_elision "]				= def::macro_201606;
		// simple_macros_["__cpp_hex_float "]							= def::macro_201603;
		// simple_macros_["__cpp_if_constexpr "]						= def::macro_201606;
		// simple_macros_["__cpp_impl_coroutine "]						= def::macro_201902;
		// simple_macros_["__cpp_impl_destroying_delete "]				= def::macro_201806;
		// simple_macros_["__cpp_impl_three_way_comparison "]			= def::macro_201907;
		// simple_macros_["__cpp_inheriting_constructors "]				= def::macro_201511;
		// simple_macros_["__cpp_init_captures "]						= def::macro_201803;
		// simple_macros_["__cpp_initializer_lists "]					= def::macro_200806;
		// simple_macros_["__cpp_inline_variables "]					= def::macro_201606;
		// simple_macros_["__cpp_lambdas "]								= def::macro_200907;
		// simple_macros_["__cpp_modules "]								= def::macro_201907;
		// simple_macros_["__cpp_namespace_attributes "]				= def::macro_201411;
		// simple_macros_["__cpp_noexcept_function_type "]				= def::macro_201510;
		// simple_macros_["__cpp_nontype_template_args "]				= def::macro_201911;
		// simple_macros_["__cpp_nontype_template_parameter_auto "]		= def::macro_201606;
		// simple_macros_["__cpp_nsdmi "]								= def::macro_200809;
		// simple_macros_["__cpp_range_based_for "]						= def::macro_201603;
		// simple_macros_["__cpp_raw_strings "]							= def::macro_200710;
		// simple_macros_["__cpp_ref_qualifiers "]						= def::macro_200710;
		// simple_macros_["__cpp_return_type_deduction "]				= def::macro_201304;
		// simple_macros_["__cpp_rvalue_references "]					= def::macro_200610;
		// simple_macros_["__cpp_sized_deallocation "]					= def::macro_201309;
		// simple_macros_["__cpp_static_assert "]						= def::macro_201411;
		// simple_macros_["__cpp_structured_bindings "]					= def::macro_201606;
		// simple_macros_["__cpp_template_template_args "]				= def::macro_201611;
		// simple_macros_["__cpp_threadsafe_static_init "]				= def::macro_200806;
		// simple_macros_["__cpp_unicode_characters "]					= def::macro_200704;
		// simple_macros_["__cpp_unicode_literals "]					= def::macro_200710;
		// simple_macros_["__cpp_user_defined_literals "]				= def::macro_200809;
		// simple_macros_["__cpp_using_enum "]							= def::macro_201907;
		// simple_macros_["__cpp_variable_templates "]					= def::macro_201304;
		// simple_macros_["__cpp_variadic_templates "]					= def::macro_200704;
		// simple_macros_["__cpp_variadic_using "]						= def::macro_201611;

		// C-compatibilities
		simple_macros_["__STDC__"]						   = def::macro_1;
		simple_macros_["__STDC_VERSION__"]				   = def::macro_199901;
		simple_macros_["__STDC_MB_MIGHT_NEQ_WC__"]		   = def::macro_1;
		simple_macros_["__STDC_ISO_10646__"]			   = def::macro_1;
		simple_macros_["__STDCPP_STRICT_POINTER_SAFETY__"] = def::macro_1;
		simple_macros_["__STDCPP_THREADS__"]			   = def::macro_1;
	}

private:
	std::string	 str_date_;
	std::string	 str_time_;
	pp::tokens_t value_date_;
	pp::tokens_t value_time_;

	simple_macros_t	  simple_macros_;	   ///< @brief  Simple macros.
	function_macros_t function_macros_;	   ///< @brief  Function macros.

private:
	static auto follows(tokens_t const& ts) {
		return tokens_t(++std::ranges::begin(ts), std::ranges::end(ts));
	}
	static bool matched(tokens_t const& tokens, std::size_t offset, std::string const& token) {
		if (std::ranges::size(tokens) <= offset) {
			return false;
		}
		auto itr = std::ranges::begin(tokens);
		std::advance(itr, offset);
		return itr->token() == token;
	}
	static lex::token_t const& at(tokens_t const& tokens, std::size_t offset) {
		if (std::ranges::size(tokens) <= offset) {
			throw std::invalid_argument(__func__ + std::to_string(__LINE__));
		}
		auto itr = std::ranges::begin(tokens);
		std::advance(itr, offset);
		return *itr;
	}
	static std::optional<std::size_t> find_at(tokens_t const& tokens, lex::token_t const& token) {
		auto const itr	 = std::ranges::begin(tokens);
		auto const found = std::ranges::find(tokens, token);
		if (found == std::ranges::end(tokens)) {
			return std::nullopt;
		} else {
			return std::distance(itr, found);
		}
	}
	static tokens_t select(std::size_t i, tokens_t const& ts) {
		std::size_t n = 0;	  // SPEC: max count of macro parameters
		tokens_t	tokens;
		int			nest = 0;	 // SPEC: max of nest
		for (auto const& t: ts) {
			using enum lex::token_type_t;
			if (t.matched(String)) {	// TODO:
				tokens.insert(std::ranges::end(tokens), t);
			} else if (t.matched(Operator, "(")) {	  // TODO: {
				tokens.insert(std::ranges::end(tokens), t);
				++nest;
			} else if (t.matched(Operator, ")")) {	  // TODO: }
				if (--nest < 0) {
					if (n != i) throw std::invalid_argument(__func__ + std::to_string(__LINE__));
					return tokens;
				}
				tokens.insert(std::ranges::end(tokens), t);
			} else if (t.matched(Operator, ",")) {
				if (0 < nest) {
					tokens.insert(std::ranges::end(tokens), t);
				} else {
					if (n++ == i) {
						return tokens;
					} else {
						tokens.clear();
					}
				}
			} else {
			}
		}
		throw std::invalid_argument(__func__ + std::to_string(__LINE__));
	}
	static tokens_citr_t actuals(tokens_t const& ts) {
		int nest = 0;	 // SPEC: max of nest
		for (auto itr = std::ranges::begin(ts), end = std::ranges::end(ts); itr != end; ++itr) {
			using enum lex::token_type_t;
			if (itr->matched(Operator, "(")) {	  // TODO: {
				++nest;
			} else if (itr->matched(Operator, ")")) {	 // TODO: }
				if (--nest < 0) { return itr; }
			} else {
			}
		}
		throw std::invalid_argument(__func__ + std::to_string(__LINE__));
	}

	static lex::token_t stringize(tokens_t const& ts) {
		log::tracer_t tr{{std::to_string(ts.size())}};
		return lex::token_t{lex::token_type_t::String, std::accumulate(std::ranges::begin(ts), std::ranges::end(ts), std::move(std::ostringstream{}), [](auto&& o, auto const& a) { o << a.token(); return std::move(o); }).str()};
	}

	bool is_simple_macro(lex::token_t const& token) const noexcept { return false; }	  // TODO:
	bool is_function_macro(lex::token_t const& token) const noexcept { return false; }	  // TODO:

	tokens_t					  value(lex::token_t const& t) { return {}; }		// TODO: value of simple macro
	std::pair<tokens_t, tokens_t> function(lex::token_t const& t) { return {}; }	// TODO: value of simple macro

	auto glue(tokens_t const& ls, tokens_t const& rs) {
		log::tracer_t tr{{std::to_string(ls.size()), std::to_string(rs.size())}};
		// The ls is the last one and the rs is more.
		if (std::ranges::size(ls) == 1u && 1u < std::ranges::size(rs)) {
			hideset_t hs;
			{
				auto const& lhs = ls.front().hideset();
				auto const& rhs = rs.front().hideset();
				hs.insert(std::ranges::begin(lhs), std::ranges::end(lhs));
				hs.insert(std::ranges::begin(rhs), std::ranges::end(rhs));
			}
			auto const& l = ls.front();
			auto const& r = rs.front();

			lex::token_t lr{lex::token_type_t::String, std::string_view(l.token().data(), l.token().length() + r.token().length()), hs};	// TODO: reinterpred
			auto const	 rs_ = follows(ls);
			return tokens_t{lr} + rs_;
		} else {
			return tokens_t{ls.front()} + glue(follows(ls), rs);
		}
	}
	tokens_t hs_add(hideset_t const& hs, tokens_t const& ts) {
		log::tracer_t tr{{std::to_string(hs.size()), std::to_string(ts.size())}};
		if (! std::ranges::empty(ts)) {
			auto t	 = ts.front();
			auto hs_ = t.hideset();
			{ hs_.insert(std::ranges::begin(hs), std::ranges::end(hs)); }

			return tokens_t{t} + hs_add(hs, follows(ts));
		} else {
			return {};
		}
	}

	tokens_t subst(tokens_t const& is, parameters_t const& fp, arguments_t const& ap, hideset_t const& hs, tokens_t const& os) {
		log::tracer_t tr{{}};
		if (std::ranges::empty(is)) {
			return hs_add(hs, os);	  // There is no more token. So, the token sequence might have been terminated.
		}
		auto const is_ = follows(is);
		if (std::optional<std::size_t> i = std::nullopt; matched(is, 0, "#") && 1u < std::ranges::size(is) && (i = find_at(fp, at(is, 1)))) {
			// "#parameter" shall be stringized.

			return subst(is_, fp, ap, hs, os + tokens_t{stringize(select(*i, ap))});
		} else if (matched(is, 0, "##") && 1u < std::ranges::size(is) && (i = find_at(fp, at(is, 1)))) {
			// "##parameter" shall combine both the lhs and rhs tokens.
			if (auto const& ap_ = select(*i, ap); std::ranges::empty(ap_)) {
				// This argument is empty and is ignored.
				return subst(is_, fp, ap, hs, os);
			} else {
				// The parameter is replaced by its argument and combined.
				return subst(is_, fp, ap, hs, glue(os, ap_));
			}
		} else if (1u < std::ranges::size(is) && matched(is, 0, "##")) {
			// "##token" shall combine both the lhs and rhs tokens.
			return subst(is_, fp, ap, hs, glue(os, {at(is, 1)}));
		} else if (2u < std::ranges::size(is) && matched(is, 0, "##") && (i = find_at(fp, at(is, 1)))) {
			// "##parameter" shall combine both the lhs and rhs tokens.
			if (auto const& ap_ = select(*i, ap); std::ranges::empty(ap_)) {
				if (is_.front() == at(fp, *i)) {	// TODO: j
					return subst(is_, fp, ap, hs, os + ap_);
				} else {
					return subst(is_, fp, ap, hs, os);
				}
			} else {
				return subst(tokens_t{lex::token_t{lex::token_type_t::Operator, "##"}} + is_, fp, ap, hs, os + ap_);
			}
		} else if (1u < std::ranges::size(is) && (i = find_at(fp, at(is, 1)))) {
			return subst(is_, fp, ap, hs, os + expand(select(*i, ap)));
		} else {
			auto const& t = is.front();
			return subst(is_, fp, ap, hs, os + tokens_t{t});
		}
	}

public:
	tokens_t expand(tokens_t const& ts) {
		log::tracer_t tr{{std::to_string(ts.size())}};
		if (std::ranges::empty(ts)) {
			return {};	  // There is no more token. So, the token sequence might have been terminated.
		}
		auto const& t	= *std::ranges::begin(ts);
		auto const	ts_ = follows(ts);
		if (hideset_t const& hs = t.hideset(); hs.contains(t)) {
			// The token has been hidden. The token does not need more expansion.
			return tokens_t{t} + expand(ts_);
		} else if (is_simple_macro(t)) {
			// Expands simple macro.
			hideset_t hs;
			{
				auto const& h  = t.hideset();
				auto const& t_ = std::views::single(t);
				std::set_union(std::ranges::begin(h), std::ranges::end(h), std::ranges::begin(t_), std::ranges::end(t_), std::inserter(hs, std::ranges::end(hs)));
			}
			return expand(subst(value(t), {}, {}, hs, {}) + ts_);
		} else if (is_function_macro(t)) {
			// Expands functional macro.
			auto const rp = actuals(ts_);

			tokens_t ap;
			{
				auto li = std::ranges::begin(ts_);
				auto ri = rp;
				++li;	 // (
				--ri;	 // )
				ap.insert(std::ranges::end(ap), li, ri);
			}

			hideset_t h;
			{
				auto const& hs	= t.hideset();
				auto const& hs_ = rp->hideset();
				h.insert(std::ranges::begin(hs), std::ranges::end(hs));
				h.insert(std::ranges::begin(hs_), std::ranges::end(hs_));
			}
			return expand(subst(value(t), function(t).first, ap, hs + hideset_t{t}, {}) + ts_);
		} else {
			return tokens_t{ts.front()} + expand(ts_);
		}
	}
};

}	 // namespace mm
namespace sm {
class symbol_t {};

class symbol_manager_t {
};

}	 // namespace sm

lines_t preprocess(cm::condition_manager_t& conditions, mm::macro_manager_t& macros, pm::path_manager_t& paths, tokens_t const& tokens, std::filesystem::path const& source);

namespace impl {

template<typename T>
inline T lexical_cast(std::string_view const& str) {
	std::istringstream iss{std::string{str}};

	T v{};
	if (iss >> v) return v;
	throw std::invalid_argument(__func__);
}

using pp_value_t = std::variant<void const*, long long, long double>;

inline std::string to_string(pp_value_t const& value) {
	if (std::holds_alternative<void const*>(value)) {
		return std::to_string(reinterpret_cast<unsigned long long>(std::get<void const*>(value)));
	} else if (std::holds_alternative<long long>(value)) {
		return std::to_string(std::get<long long>(value));
	} else if (std::holds_alternative<long double>(value)) {
		return std::to_string(std::get<long double>(value));
	} else {
		throw std::invalid_argument(__func__);
	}
}

template<typename L, typename R>
inline pp_value_t calculate_(std::string_view const o, L const lhs, R const rhs) {
	if constexpr (! std::is_floating_point_v<L> && ! std::is_floating_point_v<R> && ! std::is_pointer_v<L> && ! std::is_pointer_v<R>) {
		// Several operators can apply to integral types only.
		if (o == "<<" || o == "<<=") return lhs << rhs;
		if (o == ">>" || o == ">>=") return lhs >> rhs;
		if (o == "|" || o == "|=") return lhs | rhs;
		if (o == "&" || o == "&=") return lhs & rhs;
		if (o == "^" || o == "^=") return lhs ^ rhs;
		if (o == "%" || o == "%=") return lhs % rhs;
	} else if constexpr (! std::is_pointer_v<L> && ! std::is_pointer_v<R>) {
		// Pointers cannot be right operand of several operators
		if (o == "*" || o == "*=") return lhs * rhs;
		if (o == "/" || o == "/=") return lhs / rhs;
	}

	// Deals pointer as a pointer to character because size of pointer to void is not specified.
	// A pointer can be subtracted by another pointer.
	// A pointer also can be added or subtracted by an integer.
	if constexpr (std::is_pointer_v<L> && std::is_pointer_v<R>) {
		if (o == "-" || o == "-=") return static_cast<char const*>(lhs) - static_cast<char const*>(rhs);
	} else if constexpr (std::is_pointer_v<L> && std::is_integral_v<R>) {
		if (o == "+" || o == "+=") return static_cast<char const*>(lhs) + rhs;
		if (o == "-" || o == "-=") return static_cast<char const*>(lhs) - rhs;
	} else if constexpr (! std::is_pointer_v<L> && ! std::is_pointer_v<R>) {
		if (o == "+" || o == "+=") return lhs + rhs;
		if (o == "-" || o == "-=") return lhs - rhs;
	}

	// Deals pointer as integer of address.
	// Furthermore, bool is one of integer, too.
	if constexpr (std::is_pointer_v<L>) {
		auto const l = reinterpret_cast<std::intptr_t>(lhs);
		if constexpr (std::is_pointer_v<R>) {
			auto const r = reinterpret_cast<std::intptr_t>(rhs);
			if (o == "==") return l == r;
			if (o == "!=") return l != r;
			if (o == "&&") return l && r;
			if (o == "||") return l || r;
			if (o == "=") return r;
		} else if constexpr (std::is_integral_v<R>) {
			if (o == "==") return l == rhs;
			if (o == "!=") return l != rhs;
			if (o == "&&") return l && rhs;
			if (o == "||") return l || rhs;
			if (o == "=") return rhs;
		}
	} else if constexpr (std::is_pointer_v<R>) {
		auto const r = reinterpret_cast<std::intptr_t>(rhs);
		if (o == "==") return lhs == r;
		if (o == "!=") return lhs != r;
		if (o == "&&") return lhs && r;
		if (o == "||") return lhs || r;
		if (o == "=") return r;
	} else {
		if (o == "==") return lhs == rhs;
		if (o == "!=") return lhs != rhs;
		if (o == "&&") return lhs && rhs;
		if (o == "||") return lhs || rhs;
		if (o == "=") return rhs;
	}
	throw std::invalid_argument(__func__ + std::to_string(__LINE__));
}

struct op_t {
	op_t(std::string_view const& o) :
		o_{o} {}

	pp_value_t operator()(void const* const& v) {
		if (o_ == "*") return 1;						// TODO:
		if (o_ == "#" || o_ == "%:") return "TODO:";	// TODO:
		throw std::invalid_argument(__func__);
	}
	pp_value_t operator()(long long v) {
		if (o_ == "+") return v;
		if (o_ == "-") return v * -1;
		if (o_ == "&") return 1;	// TODO:
		if (o_ == "~" || o_ == "compl") return ~v;
		if (o_ == "#" || o_ == "%:") return "TODO:";	// TODO:
		throw std::invalid_argument(__func__);
	}
	pp_value_t operator()(long double v) {
		if (o_ == "+") return v;
		if (o_ == "-") return v * -1;
		if (o_ == "&") return 1;						// TODO:
		if (o_ == "#" || o_ == "%:") return "TODO:";	// TODO:
		throw std::invalid_argument(__func__);
	}

private:
	std::string_view o_;
};

inline pp_value_t calculate(std::string_view const& o, pp_value_t const& lhs, pp_value_t const& rhs) {
	log::tracer_t tr{{std::string{o}}};

	auto const result = std::visit([o, &rhs](auto const& l) { return std::visit([o, &l](auto const& r) { return calculate_(o, l, r); }, rhs); }, lhs);
	tr.set_result(to_string(result));
	return result;
}
inline pp_value_t calculate(std::string_view const& o, pp_value_t const& value) {
	log::tracer_t tr{{std::string{o}}};

	auto const result = std::visit(op_t{o}, value);
	tr.set_result(to_string(result));
	return result;
}

inline int op_priority(std::string_view const& op) {
	log::tracer_t tr{{std::string{op}}};

	if (op == "==" || op == "!=" || op == "&&" || op == "||") return 1;
	if (op == "<<" || op == ">>" || op == "<<=" || op == ">>=") return 2;
	if (op == "&" || op == "|" || op == "^" || op == "&=" || op == "|=" || op == "^=") return 3;
	if (op == "+" || op == "-" || op == "+=" || op == "-=") return 4;
	if (op == "*" || op == "/" || op == "%" || op == "*=" || op == "/=" || op == "%=") return 5;
	if (op == "=") return 6;
	return 0;
}

inline void evaluate_operator(std::stack<std::string_view>& op, std::stack<pp_value_t>& value) {
	log::tracer_t tr{{}};
	if (op.empty() || value.empty()) throw std::invalid_argument(__func__);

	if (lex::is_binary_op(op.top())) {
		auto const lhs = value.top();
		value.pop();
		if (value.empty()) {
			if (! lex::is_unary_op(op.top())) throw std::invalid_argument(__func__);
			value.push(calculate(op.top(), lhs));
		} else {
			auto const rhs = value.top();
			value.pop();
			value.push(calculate(op.top(), lhs, rhs));
		}
	} else if (lex::is_unary_op(op.top())) {
		auto const v = value.top();
		value.pop();
		value.push(calculate(op.top(), v));
	} else if (lex::is_trinary_op(op.top())) {
		tr.trace("TODO:");	  // TODO:
	} else
		throw std::invalid_argument(__func__);
	op.pop();
}

pp_value_t evaluate(std::stack<std::string_view>& op, std::stack<pp_value_t>& value, mm::macro_manager_t& macros, line_tokens_t const& line) {
	log::tracer_t tr{{}};

	using enum lex::token_type_t;
	for (auto itr = line.first; itr != line.second; ++itr) {
		tr.trace(lex::to_string(itr->type()) + escape(itr->token()));
		switch (itr->type()) {
		// -------------------------------
		// String is just dealt as different address here.
		// Raw string is also just another pointer with different address.
		case Raw_string: [[fallthrough]];
		case String: value.push(itr->token().data()); break;
		// -------------------------------
		case Identifier:
			if (itr->token() == "defined") {
				// -------------------------------
				// defined(...)
				if (auto const lp = next_nonws(itr, line.second); lp->matched(lex::token_type_t::Separator, "(")) {
					if (auto const identifier = next_nonws(lp, line.second); identifier->matched(lex::token_type_t::Identifier)) {
						if (auto const rp = next_nonws(identifier, line.second); rp->matched(lex::token_type_t::Identifier, ")")) {
							auto const defined = macros.defined(std::string{identifier->token()});
							value.push(defined ? 1 : 0);
							break;
						}
					}
				}
				throw std::runtime_error("Invalid expression");
			} else if (macros.defined(std::string{itr->token()})) {
				// -------------------------------
				// Expands macro value
				value.push(evaluate(op, value, macros, macros.value(itr->token())));
				break;
			} else {
				throw std::runtime_error("Invalid expression");
			}
		// -------------------------------
		// Character is just dealt as small integral value.
		case Character: value.push(lexical_cast<long long>(itr->token().substr(1, itr->token().size() - 2))); break;
		// -------------------------------
		// Number is just dealt as value.
		case Number: value.push(itr->token().find('.') != std::string_view::npos ? lexical_cast<long double>(itr->token()) : lexical_cast<long long>(itr->token())); break;
		// -------------------------------
		// Only literals are dealt as value.
		case Keyword:
			if (itr->token() == "true") value.push(1);
			if (itr->token() == "false") value.push(0);
			if (itr->token() == "nullptr") value.push(nullptr);
			if (itr->token() == "sizeof") {
				value.push(nullptr);	// TODO:
			}
			if (itr->token() == "alignof") {
				value.push(nullptr);	// TODO:
			}
			break;
		// -------------------------------
		case Operator: {
			while (! op.empty() && op_priority(op.top()) >= op_priority(itr->token())) {
				evaluate_operator(op, value);
			}
			op.push(itr->token());
		} break;
		// -------------------------------
		case Separator:
			if (itr->token() == "(") {
				op.push(itr->token());
			} else if (itr->token() == ")") {
				while (! op.empty() && op.top() != "(") {
					evaluate_operator(op, value);
				}
			} else {
				;	 // TODO:
			}
			break;
		// -------------------------------
		case Header: break;
		default: break;
		}
	}

	for (; ! op.empty(); op.pop()) {
		auto const lhs = value.top();
		value.pop();
		auto const rhs = value.top();
		value.pop();
		value.push(calculate(op.top(), lhs, rhs));
	}
	tr.set_result(to_string(value.top()));
	return value.top();
}

std::tuple<bool, bool> parse_preprocessing_if_line(mm::macro_manager_t& macros, line_tokens_t const& line) {
	log::tracer_t tr{{lex::to_string(line.first->pos())}, true};

	auto const marker = lex::skip_ws(line.first, line.second);

	if (marker == line.second || ! marker->matched(lex::token_type_t::Operator, "#")) return {false, false};
	auto const directive = impl::next_nonws(marker, line.second);

	if (directive == line.second) return {false, false};
	if (directive->matched(lex::token_type_t::Keyword, "if")) {
		auto const conditions = impl::next_nonws(directive, line.second);
		if (conditions == line.second) return {false, false};
		std::stack<std::string_view> ops;
		std::stack<pp_value_t>		 values;

		auto const result = evaluate(ops, values, macros, std::make_pair(conditions, line.second));
		return {true, std::visit([](auto const& a) { return a != 0; }, result)};
	} else if (directive->matched(lex::token_type_t::Identifier, "ifdef")) {
		auto const macro = impl::next_nonws(directive, line.second);
		if (macro == line.second) return {false, false};
		auto const result = macros.defined(macro->token());
		return {next_nonws(macro, line.second) == line.second, result};
	} else if (directive->matched(lex::token_type_t::Identifier, "ifndef")) {
		auto const macro = impl::next_nonws(directive, line.second);
		if (macro == line.second) return {false, false};
		auto const result = macros.defined(macro->token());
		return {next_nonws(macro, line.second) == line.second, ! result};
	} else {
		return {false, false};
	}
}
std::tuple<bool, bool> parse_preprocessing_elif_line(mm::macro_manager_t& macros, line_tokens_t const& line) {
	log::tracer_t tr{{lex::to_string(line.first->pos())}, true};

	auto const marker = lex::skip_ws(line.first, line.second);
	if (marker == line.second || ! marker->matched(lex::token_type_t::Operator, "#")) return {false, false};
	auto const directive = impl::next_nonws(marker, line.second);
	if (directive == line.second || ! directive->matched(lex::token_type_t::Identifier, "elif")) return {false, false};
	auto const conditions = impl::next_nonws(directive, line.second);
	if (conditions == line.second) return {false, false};
	std::stack<std::string_view> ops;
	std::stack<pp_value_t>		 values;

	auto const result = evaluate(ops, values, macros, std::make_pair(conditions, line.second));
	return {true, std::visit([](auto const& a) { return a != 0; }, result) != 0};
}
bool parse_preprocessing_else_line(line_tokens_t const& line) {
	log::tracer_t tr{{lex::to_string(line.first->pos())}, true};

	auto const marker = lex::skip_ws(line.first, line.second);
	if (marker == line.second || ! marker->matched(lex::token_type_t::Operator, "#")) return false;
	auto const directive = impl::next_nonws(marker, line.second);
	if (directive == line.second || ! directive->matched(lex::token_type_t::Keyword, "else")) return false;
	return next_nonws(directive, line.second) == line.second;
}
bool parse_preprocessing_endif_line(line_tokens_t const& line) {
	log::tracer_t tr{{lex::to_string(line.first->pos())}, true};

	auto const marker = lex::skip_ws(line.first, line.second);
	if (marker == line.second || ! marker->matched(lex::token_type_t::Operator, "#")) return false;
	auto const directive = impl::next_nonws(marker, line.second);
	if (directive == line.second || ! directive->matched(lex::token_type_t::Identifier, "endif")) return false;
	return next_nonws(directive, line.second) == line.second;
}

mm::macro_manager_t::macro_parameters_t enclosed_parameters(tokens_citr_t itr, tokens_citr_t const& end) {
	if (itr->matched(lex::token_type_t::Separator, "(")) { ++itr; }
	mm::macro_manager_t::macro_parameters_t parameters;

	bool value{true};
	for (; itr != end; ++itr) {
		if (itr->matched(lex::token_type_t::Separator, ")")) {
			if (value && ! parameters.empty()) throw std::invalid_argument(__func__ + std::to_string(__LINE__));
			break;
		} else if (itr->matched(lex::token_type_t::Separator, ",")) {
			if (value) throw std::invalid_argument(__func__ + std::to_string(__LINE__));
			value = ! value;
		} else if (itr->matched(lex::token_type_t::Identifier)) {
			if (! value) throw std::invalid_argument(__func__ + std::to_string(__LINE__));
			value = ! value;
			parameters.push_back(itr);
		}
	}
	return parameters;
}
std::tuple<bool, bool> parse_preprocessing_define_line(mm::macro_manager_t& macros, line_tokens_t const& line) {
	log::tracer_t tr{{lex::to_string(line.first->pos())}};

	auto const marker = lex::skip_ws(line.first, line.second);
	if (marker == line.second || ! marker->matched(lex::token_type_t::Operator, "#")) return {false, false};
	auto const directive = impl::next_nonws(marker, line.second);
	if (directive == line.second || directive->matched(lex::token_type_t::Identifier, "define")) return {false, false};
	auto const macro = impl::next_nonws(directive, line.second);
	if (macro == line.second) return {false, false};
	auto lp = macro;
	lp++;	 // If there is left parenthesis without whitespace, it is function macro.
	if (lp == line.second) {
		// -------------------------------
		// Simple macro without value.
		macros.define_simple_macro(macro->token(), mm::def::macro_1);
		tr.set_result(escape(macro->token()));
	} else if (lp->matched(lex::token_type_t::Separator, "(")) {
		// -------------------------------
		// Function macro
		auto itr = next_nonws(lp, line.second);

		// parses parameters
		mm::macro_manager_t::macro_parameters_t parameters = enclosed_parameters(itr, line.second);

		// parses body
		itr = lex::skip_ws(itr, line.second);
		if (itr == line.second) return {true, false};
		auto const body = std::make_pair(itr, line.second);
		macros.define_faction_macro(macro->token(), parameters, body);
		tr.set_result(escape(macro->token()));
	} else {
		// -------------------------------
		// Simple macro
		macros.define_simple_macro(macro->token(), std::make_pair(lp, line.second));
		tr.set_result(escape(macro->token()));
	}
	return {true, true};
}
std::tuple<bool, bool> parse_preprocessing_undef_line(mm::macro_manager_t& macros, line_tokens_t const& line) {
	log::tracer_t tr{{lex::to_string(line.first->pos())}};

	auto const marker = lex::skip_ws(line.first, line.second);
	if (marker == line.second || ! marker->matched(lex::token_type_t::Operator, "#")) return {false, false};
	auto const directive = next_nonws(marker, line.second);
	if (directive == line.second || ! directive->matched(lex::token_type_t::Identifier, "undef")) return {false, false};
	auto const macro = next_nonws(directive, line.second);
	if (macro == line.second) return {false, false};
	auto const extra = next_nonws(macro, line.second);
	if (extra != line.second) return {false, false};

	if (! macros.defined(macro->token())) return {true, false};
	macros.undefine_macro(macro->token());
	tr.set_result(escape(macro->token()));
	return {true, true};
}
std::tuple<bool, std::filesystem::path, lines_t> parse_preprocessing_include_line(cm::condition_manager_t& conditions, mm::macro_manager_t& macros, pm::path_manager_t& paths, line_tokens_t const& line) {
	log::tracer_t tr{{lex::to_string(line.first->pos())}, true};

	auto const marker = lex::skip_ws(line.first, line.second);
	tr.trace(lex::to_string(*marker));
	if (marker == line.second || ! marker->matched(lex::token_type_t::Operator, "#")) return {false, {}, {}};
	auto const directive = next_nonws(marker, line.second);
	if (directive == line.second || ! directive->matched(lex::token_type_t::Identifier, "include")) return {false, {}, {}};
	auto const path_token = next_nonws(directive, line.second);
	if (path_token == line.second || (path_token->matched(lex::token_type_t::Header) && path_token->matched(lex::token_type_t::String))) return {false, {}, {}};
	auto const extra = next_nonws(path_token, line.second);
	if (extra != line.second) return {false, {}, {}};

	std::filesystem::path const path{std::string{path_token->token().substr(1, path_token->token().length() - 2)}};

	auto const fullpath = paths.find(path, path_token->matched(lex::token_type_t::String));
	if (! fullpath) return {false, path, {}};

	tr.set_result(fullpath->string());

	paths.push(*fullpath);
	try {
		paths.source(xxx::load_file(paths.path()));
		paths.tokens(xxx::lex::scan(paths.source(), *fullpath));
		paths.preprocessing_tokens(preprocess(conditions, macros, paths, paths.tokens(), *fullpath));
	} catch (std::exception const& e) {
		std::cerr << e.what() << std::endl;
		paths.pop();
		return {false, *fullpath, {}};
	}
	auto const& pp_tokens = paths.preprocessing_tokens();
	paths.pop();
	return {true, *fullpath, pp_tokens};
}
std::tuple<bool, std::string_view, unsigned long long> parse_preprocessing_line_line(mm::macro_manager_t&, line_tokens_t const& line) {
	log::tracer_t tr{{lex::to_string(line.first->pos())}, true};

	auto const marker = lex::skip_ws(line.first, line.second);
	if (marker == line.second || ! marker->matched(lex::token_type_t::Operator, "#")) return {false, "", 0ull};
	auto const directive = next_nonws(marker, line.second);
	if (directive == line.second || ! directive->matched(lex::token_type_t::Identifier, "line")) return {false, "", 0ull};
	auto const lineno = next_nonws(directive, line.second);
	if (lineno == line.second || ! lineno->matched(lex::token_type_t::Number)) return {false, "", 0ull};
	auto const no		= lexical_cast<unsigned long long>(lineno->token());
	auto const filename = next_nonws(lineno, line.second);
	return {true, filename == line.second ? ""sv : filename->token(), no};
}
bool parse_preprocessing_error_line(line_tokens_t const& line) {
	log::tracer_t tr{{lex::to_string(line.first->pos())}, true};

	auto const marker = lex::skip_ws(line.first, line.second);
	if (marker == line.second || ! marker->matched(lex::token_type_t::Operator, "#")) return false;
	auto const directive = next_nonws(marker, line.second);
	if (directive == line.second || ! directive->matched(lex::token_type_t::Identifier, "error")) return false;
	auto const message = next_nonws(directive, line.second);

	std::ostringstream oss;
	oss << "#error";
	if (message != line.second) {
		tokens_t messages(message, line.second);
		auto	 msg = messages | std::views::transform([](lex::token_t const& token) { return std::string{token.token()}; });
		oss << " - " << std::reduce(std::ranges::begin(msg), std::ranges::end(msg));
	}
	std::clog << oss.str() << std::endl;
	tr.set_result(oss.str());
	return true;
}
bool parse_preprocessing_pragma_line(line_tokens_t const& line) {
	log::tracer_t tr{{lex::to_string(line.first->pos())}, true};

	auto const marker = lex::skip_ws(line.first, line.second);
	if (marker == line.second || ! marker->matched(lex::token_type_t::Operator, "#")) return false;
	auto const directive = impl::next_nonws(marker, line.second);
	if (directive == line.second || ! directive->matched(lex::token_type_t::Identifier, "pragma")) return false;
	auto const message = lex::skip_ws(directive, line.second);

	std::ostringstream oss;
	oss << "unknown pragma is ignored ";	// TODO: _Pragma
	if (message != line.second) {
		tokens_t messages(message, line.second);
		auto	 msg = messages | std::views::transform([](lex::token_t const& token) { return std::string{token.token()}; });
		oss << " - " << std::reduce(std::ranges::begin(msg), std::ranges::end(msg));
	}
	std::clog << oss.str() << std::endl;
	tr.set_result(oss.str());
	return true;
}

std::tuple<bool, lines_t> parse_preprocessing_line(cm::condition_manager_t& conditions, mm::macro_manager_t& macros, pm::path_manager_t& paths, line_tokens_t const& line) {
	log::tracer_t tr{{lex::to_string(line.first->pos())}, true};
	if (auto const [matched, file, lineno] = parse_preprocessing_line_line(macros, line); matched) {
		// -------------------------------
		// #line number (filename)?
		long long current_line = line.first->line();
		// Gets non-const iterator of tokens to relocate these positions.
		auto& tokens = paths.mutable_tokens();
		auto  itr	 = std::ranges::begin(tokens);
		auto  end	 = std::ranges::end(tokens);
		std::advance(itr, std::distance(std::ranges::begin(paths.tokens()), line.first));

		auto const filename = (! file.empty()) ? std::make_shared<std::filesystem::path const>(file) : line.first->file();
		std::for_each(itr, end, [&filename, current_line, lineno](auto& a) {	if (a.file()) {a.pos(lineno + (a.line() - current_line), filename);	} });
		return {false, {line}};
	} else if (auto const [matched, file, lines] = parse_preprocessing_include_line(conditions, macros, paths, line); matched) {
		// -------------------------------
		// #include <...> or "..."
		return {true, lines};
	} else if (parse_preprocessing_error_line(line)) {
		// -------------------------------
		// #error ...
		// TODO: failed to compile
		return {true, {}};
	} else if (parse_preprocessing_pragma_line(line)) {
		// -------------------------------
		// #pragma ...
		// Ignores it because no pragma is supported yet.
		return {true, {}};
	} else if (auto const marker = lex::skip_ws(line.first, line.second); marker != line.second && marker->matched(lex::token_type_t::Operator, "#") && impl::next_nonws(marker, line.second) == line.second) {
		// -------------------------------
		// #
		// Ignores it because of empty directive.
		// It is parsed here directly.
		return {true, {}};
	} else {
		return {true, {line}};
	}
}

///     @brief  Proceeds conditions.
////            #if ... (#elif ...)* (#else ...)? #endif
std::tuple<lines_t, lines_t::const_iterator> preprocess_conditions(cm::condition_manager_t& conditions, mm::macro_manager_t& macros, pm::path_manager_t& paths, lines_t::const_iterator const& begin, lines_t::const_iterator const& end, std::filesystem::path const& source) {
	log::tracer_t tr{{}, true};

	lines_t result;

	bool elseif{true};
	for (auto line = begin; line != end; ++line) {
		auto const token = lex::skip_ws(line->first, line->second);
		if (token == line->second) continue;
		if (auto const [matched, condition] = impl::parse_preprocessing_if_line(macros, {token, line->second}); matched) {
			// -------------------------------
			// #if ...
			tr.trace(lex::to_string(token->pos()) + "#if " + std::to_string(condition), true);
			conditions.push(condition);
			auto const [r, itr] = preprocess_conditions(conditions, macros, paths, ++line, end, source);
			if (! std::ranges::empty(r)) { result.insert(std::ranges::end(result), std::ranges::begin(r), std::ranges::end(r)); }
			if (itr == end) break;
			line = itr;
		} else if (auto const [matched, condition] = impl::parse_preprocessing_elif_line(macros, {token, line->second}); matched) {
			// -------------------------------
			// #elif ...
			tr.trace(lex::to_string(token->pos()) + "#elif");
			if (conditions.empty()) throw std::runtime_error("Invalid #elif");
			if (! elseif) throw std::runtime_error("Invalid #elif - after #else");
			conditions.pop();
			conditions.push(condition);
		} else if (impl::parse_preprocessing_else_line({token, line->second})) {
			// -------------------------------
			// #else ...
			tr.trace(lex::to_string(token->pos()) + "#else");
			if (conditions.empty()) throw std::runtime_error("Invalid #else");
			if (! elseif) throw std::runtime_error("Invalid #else - after #else");
			elseif = false;
			conditions.flip();
		} else if (impl::parse_preprocessing_endif_line({token, line->second})) {
			// -------------------------------
			// #endif
			tr.trace(lex::to_string(token->pos()) + "#endif");
			if (conditions.empty()) throw std::runtime_error("Invalid #endif");
			conditions.pop();
			return {result, line};
		} else if (conditions.available()) {
			// -------------------------------
			// ...
			tr.trace(lex::to_string(token->pos()) + "#");
			if (auto [required, lines] = parse_preprocessing_line(conditions, macros, paths, *line); required) {
				std::ranges::for_each(lines, [&macros, &paths](auto& line) {
					// TODO: currently all the lines are regenerated here regardless of whether macro expanded or not....
					pp::tokens_t tokens(line.first, line.second);
					auto const	 ex = macros.expand(tokens);
					auto&		 mt = paths.mutable_tokens();
					mt.erase(line.first, line.second);
					auto pos = std::ranges::begin(mt);
					int	 tmp = std::distance(std::ranges::begin(paths.tokens()), line.first) - 1;
					std::clog << tmp << "---" << std::endl;
					std::advance(pos, tmp);
					auto i	   = mt.insert(pos, std::ranges::begin(ex), std::ranges::end(ex));
					line.first = i;
				});
				std::ranges::copy(lines, std::inserter(result, std::ranges::end(result)));
			}
		}	 // else skips whole
	}
	return {result, end};
}

}	 // namespace impl

lines_t preprocess(cm::condition_manager_t& conditions, mm::macro_manager_t& macros, pm::path_manager_t& paths, tokens_t const& tokens, std::filesystem::path const& source) {
	log::tracer_t tr{{}};

	check(! source.string().empty());
	if (tokens.empty()) return {};	  // Empty file is valid.

	// -------------------------------
	// Proceeds line by line.
	auto const lines		 = impl::split_lines(tokens);
	auto const [result, itr] = impl::preprocess_conditions(conditions, macros, paths, std::ranges::begin(lines), std::ranges::end(lines), source);
	if (itr != std::ranges::end(lines)) throw std::runtime_error("unexpected line");
	return result;
}

}	 // namespace pp
namespace cxx {
namespace ast {

class node_t {
public:
	auto token() const noexcept { return token_; }
	auto children() const noexcept { return children_; }

	void push(std::shared_ptr<node_t> node) { children_.push_back(node); }
	void push(pp::tokens_citr_t begin, pp::tokens_citr_t end) {
		std::transform(begin, end, std::back_inserter(children_), [](auto const& a) { return std::make_shared<node_t>(a); });
	}

public:
	///     @note   implicit
	node_t(lex::token_t const& token) noexcept :
		token_{std::make_shared<lex::token_t>(token)}, children_{} {}
	///     @note   implicit
	node_t(lex::token_t&& token) noexcept :
		token_{std::make_shared<lex::token_t>(std::move(token))}, children_{} {}

	explicit node_t(std::vector<std::shared_ptr<node_t>> const& children) noexcept :
		token_{}, children_{children} {}
	explicit node_t(std::vector<std::shared_ptr<node_t>>&& children) noexcept :
		token_{}, children_{std::move(children)} {}

private:
	std::shared_ptr<lex::token_t>		 token_;
	std::vector<std::shared_ptr<node_t>> children_;
};

}	 // namespace ast
namespace stx {

struct parser_t {
	using nodes_t  = std::vector<std::shared_ptr<ast::node_t>>;
	using result_t = std::tuple<nodes_t, std::optional<pp::tokens_t>>;

	virtual result_t parse(nodes_t const& nodes, pp::tokens_t const& source) const = 0;

	virtual ~parser_t() = default;
};
using parser_p = std::shared_ptr<parser_t const>;

namespace impl {

struct or_t : parser_t {
	virtual result_t parse(nodes_t const& nodes, pp::tokens_t const& source) const override {
		for (auto const& parser: parsers_) {
			if (auto const [ns, rest] = parser->parse(nodes, source); rest) { return {ns, rest}; }
		}
		return {nodes_t{}, std::nullopt};
	}
	explicit or_t(std::vector<parser_p> const& parsers) :
		parsers_{parsers} {}

private:
	std::vector<parser_p> parsers_;
};
struct seq_t : parser_t {
	virtual result_t parse(nodes_t const& nodes, pp::tokens_t const& source) const override {
		nodes_t		 r = nodes;
		pp::tokens_t t = source;
		for (auto const& parser: parsers_) {
			if (auto const [ns, rest] = parser->parse(r, t); rest) {
				r.insert(std::ranges::end(r), std::ranges::begin(ns), std::ranges::end(ns));
				t = *rest;
			} else {
				return {};
			}
		}
		return {r, t};
	}
	explicit seq_t(std::vector<parser_p> const& parsers) :
		parsers_{parsers} {}

private:
	std::vector<parser_p> parsers_;
};
struct opt_t : parser_t {
	virtual result_t parse(nodes_t const& nodes, pp::tokens_t const& source) const override {
		if (auto const [ns, rest] = parser_->parse(nodes, source); rest) { return {ns, rest}; }
		return {nodes_t{}, source};
	}
	explicit opt_t(parser_p parser) :
		parser_{parser} {}

private:
	parser_p parser_;
};
struct zom_t : parser_t {
	virtual result_t parse(nodes_t const& nodes, pp::tokens_t const& source) const {
		nodes_t		 r = nodes;
		pp::tokens_t t = source;
		for (;;) {
			if (auto const [ns, rest] = parser_->parse(r, t); rest) {
				r.insert(std::ranges::end(r), std::ranges::begin(ns), std::ranges::end(ns));
				t = *rest;
			} else {
				break;
			}
		}
		return {r, t};
	}
	explicit zom_t(parser_p parser) :
		parser_{parser} {}

private:
	parser_p parser_;
};
struct oom_t : parser_t {
	virtual result_t parse(nodes_t const& nodes, pp::tokens_t const& source) const override {
		if (auto const [ns, rest] = zom_t(parser_).parse(nodes, source); rest && ! ns.empty()) { return {ns, rest}; }
		return {nodes_t{}, std::nullopt};
	}
	explicit oom_t(parser_p parser) :
		parser_{parser} {}

private:
	parser_p parser_;
};

struct tok_t : parser_t {
	virtual result_t parse(nodes_t const& nodes, pp::tokens_t const& source) const override {
		if (source.empty()) return {nodes, {}};
		if (auto const& token = source.front(); token.matched(lex::token_type_t::Identifier)) {
			nodes_t ns = nodes;
			ns.emplace_back(std::make_shared<ast::node_t>(token));
			return {ns, pp::tokens_t{++source.begin(), source.end()}};
		}
		return {nodes_t{}, std::nullopt};
	}
	explicit tok_t(lex::token_type_t type) :
		type_{type} {
	}

private:
	lex::token_type_t type_;
};

struct str_t : parser_t {
	virtual result_t parse(nodes_t const& nodes, pp::tokens_t const& source) const override {
		if (source.empty()) return {nodes, {}};
		if (auto const& token = source.front(); token.matched(lex::token_type_t::Identifier, str_)) {
			nodes_t ns = nodes;
			ns.emplace_back(std::make_shared<ast::node_t>(token));
			return {ns, pp::tokens_t{++source.begin(), source.end()}};
		}
		return {nodes_t{}, std::nullopt};
	}
	explicit str_t(std::string const& str, lex::token_type_t type) :
		str_{str}, type_{type} {
		if (str.empty()) { throw std::invalid_argument(__func__); }
	}

private:
	std::string		  str_;
	lex::token_type_t type_;
};

struct set_t : parser_t {
	virtual result_t parse(nodes_t const& nodes, pp::tokens_t const& source) const override {
		if (source.empty()) return {nodes, {}};
		for (auto const& str: set_) {
			if (auto const& token = source.front(); token.matched(type_, str)) {
				nodes_t ns = nodes;
				ns.emplace_back(std::make_shared<ast::node_t>(token));
				return {ns, pp::tokens_t{++source.begin(), source.end()}};
			}
		}
		return {nodes_t{}, std::nullopt};
	}
	explicit set_t(std::vector<std::string> const& set, lex::token_type_t type) :
		set_{set}, type_{type} {
		if (set.empty()) { throw std::invalid_argument(__func__); }
	}

private:
	std::vector<std::string> set_;
	lex::token_type_t		 type_;
};

}	 // namespace impl

inline parser_p or_(std::vector<parser_p> const& parsers) { return std::make_shared<impl::or_t>(parsers); }
inline parser_p seq_(std::vector<parser_p> const& parsers) { return std::make_shared<impl::seq_t>(parsers); }
inline parser_p opt_(parser_p parser) { return std::make_shared<impl::opt_t>(parser); }
inline parser_p zom_(parser_p parser) { return std::make_shared<impl::oom_t>(parser); }
inline parser_p oom_(parser_p parser) { return std::make_shared<impl::zom_t>(parser); }
inline parser_p tok_(lex::token_type_t type) { return std::make_shared<impl::tok_t>(type); }
inline parser_p tok_(lex::token_type_t type, std::string const& str) { return std::make_shared<impl::str_t>(str, type); }
inline parser_p id_(std::string const& str) { return std::make_shared<impl::str_t>(str, lex::token_type_t::Identifier); }
inline parser_p op_(std::string const& str) { return std::make_shared<impl::str_t>(str, lex::token_type_t::Operator); }
inline parser_p kw_(std::string const& str) { return std::make_shared<impl::str_t>(str, lex::token_type_t::Keyword); }
inline parser_p punc_(std::string const& str) { return std::make_shared<impl::str_t>(str, lex::token_type_t::Separator); }
inline parser_p kw_set_(std::vector<std::string> const& set) { return std::make_shared<impl::set_t>(set, lex::token_type_t::Operator); }
inline parser_p op_set_(std::vector<std::string> const& set) { return std::make_shared<impl::set_t>(set, lex::token_type_t::Keyword); }

#define xxx_parser_declare(name)                                                                 \
	struct name##parser_t : parser_t {                                                           \
		virtual result_t parse(nodes_t const& nodes, pp::tokens_t const& source) const override; \
	};                                                                                           \
	auto const name = std::make_shared<name##parser_t>()

#define xxx_parser_define(name, body)                                                                \
	struct name##parser_t : parser_t {                                                               \
		virtual result_t parse(nodes_t const& nodes, pp::tokens_t const& source) const override body \
	};                                                                                               \
	auto const name = std::make_shared<name##parser_t>()

#define xxx_parser_impl(name, body) \
	inline parser_t::result_t name##parser_t::parse(nodes_t const& nodes, pp::tokens_t const& source) const body

namespace lit {
auto const scope_	   = op_("::");
auto const lp_		   = punc_("(");
auto const rp_		   = punc_(")");
auto const lbc_		   = punc_("{");
auto const rbc_		   = punc_("}");
auto const lbk_		   = op_("[");
auto const rbk_		   = op_("]");
auto const tilde_	   = op_("~");
auto const lt_		   = op_("<");
auto const gt_		   = op_(">");
auto const amp_		   = op_("&");
auto const amp2_	   = op_("&&");
auto const vl_		   = op_("|");
auto const vl2_		   = op_("||");
auto const eq_		   = op_("=");
auto const ex_		   = op_("!");
auto const q_		   = op_("?");
auto const semi_	   = op_(";");
auto const col_		   = op_(":");
auto const comma_	   = op_(",");
auto const hut_		   = op_("^");
auto const sq_		   = op_("'");
auto const dot_		   = op_(".");
auto const ellipsis_   = op_("...");
auto const arrow_	   = op_("->");
auto const arrow_star_ = op_("->*");
auto const star_	   = op_("*");
auto const spaceship_  = op_("<=>");
auto const dq_		   = op_("\"");

auto const zero_ = tok_(lex::token_type_t::Number, "0");

auto const alignas_			 = kw_(lex::def::alignas_s_);
auto const alignof_			 = kw_(lex::def::alignof_s_);
auto const asm_				 = kw_(lex::def::asm_s_);
auto const auto_			 = kw_(lex::def::auto_s_);
auto const bool_			 = kw_(lex::def::bool_s_);
auto const break_			 = kw_(lex::def::break_s_);
auto const case_			 = kw_(lex::def::case_s_);
auto const catch_			 = kw_(lex::def::catch_s_);
auto const char_			 = kw_(lex::def::char_s_);
auto const char8_t_			 = kw_(lex::def::char8_t_s_);
auto const char16_t_		 = kw_(lex::def::char16_t_s_);
auto const char32_t_		 = kw_(lex::def::char32_t_s_);
auto const class_			 = kw_(lex::def::class_s_);
auto const concept_			 = kw_(lex::def::concept_s_);
auto const const_			 = kw_(lex::def::const_s_);
auto const consteval_		 = kw_(lex::def::consteval_s_);
auto const constexpr_		 = kw_(lex::def::constexpr_s_);
auto const constinit_		 = kw_(lex::def::constinit_s_);
auto const const_cast_		 = kw_(lex::def::const_cast_s_);
auto const continue_		 = kw_(lex::def::continue_s_);
auto const co_await_		 = kw_(lex::def::co_await_s_);
auto const co_return_		 = kw_(lex::def::co_return_s_);
auto const co_yield_		 = kw_(lex::def::co_yield_s_);
auto const decltype_		 = kw_(lex::def::decltype_s_);
auto const default_			 = kw_(lex::def::default_s_);
auto const delete_			 = kw_(lex::def::delete_s_);
auto const do_				 = kw_(lex::def::do_s_);
auto const double_			 = kw_(lex::def::double_s_);
auto const dynamic_cast_	 = kw_(lex::def::dynamic_cast_s_);
auto const else_			 = kw_(lex::def::else_s_);
auto const enum_			 = kw_(lex::def::enum_s_);
auto const explicit_		 = kw_(lex::def::explicit_s_);
auto const export_			 = kw_(lex::def::export_s_);
auto const extern_			 = kw_(lex::def::extern_s_);
auto const false_			 = kw_(lex::def::false_s_);
auto const float_			 = kw_(lex::def::float_s_);
auto const for_				 = kw_(lex::def::for_s_);
auto const friend_			 = kw_(lex::def::friend_s_);
auto const goto_			 = kw_(lex::def::goto_s_);
auto const if_				 = kw_(lex::def::if_s_);
auto const inline_			 = kw_(lex::def::inline_s_);
auto const int_				 = kw_(lex::def::int_s_);
auto const long_			 = kw_(lex::def::long_s_);
auto const mutable_			 = kw_(lex::def::mutable_s_);
auto const namespace_		 = kw_(lex::def::namespace_s_);
auto const new_				 = kw_(lex::def::new_s_);
auto const noexcept_		 = kw_(lex::def::noexcept_s_);
auto const nullptr_			 = kw_(lex::def::nullptr_s_);
auto const operator_		 = kw_(lex::def::operator_s_);
auto const private_			 = kw_(lex::def::private_s_);
auto const protected_		 = kw_(lex::def::protected_s_);
auto const public_			 = kw_(lex::def::public_s_);
auto const register_		 = kw_(lex::def::register_s_);
auto const reinterpret_cast_ = kw_(lex::def::reinterpret_cast_s_);
auto const requires_		 = kw_(lex::def::requires_s_);
auto const return_			 = kw_(lex::def::return_s_);
auto const short_			 = kw_(lex::def::short_s_);
auto const signed_			 = kw_(lex::def::signed_s_);
auto const sizeof_			 = kw_(lex::def::sizeof_s_);
auto const static_			 = kw_(lex::def::static_s_);
auto const static_assert_	 = kw_(lex::def::static_assert_s_);
auto const static_cast_		 = kw_(lex::def::static_cast_s_);
auto const struct_			 = kw_(lex::def::struct_s_);
auto const switch_			 = kw_(lex::def::switch_s_);
auto const template_		 = kw_(lex::def::template_s_);
auto const this_			 = kw_(lex::def::this_s_);
auto const thread_local_	 = kw_(lex::def::thread_local_s_);
auto const throw_			 = kw_(lex::def::throw_s_);
auto const true_			 = kw_(lex::def::true_s_);
auto const try_				 = kw_(lex::def::try_s_);
auto const typedef_			 = kw_(lex::def::typedef_s_);
auto const typeid_			 = kw_(lex::def::typeid_s_);
auto const typename_		 = kw_(lex::def::typename_s_);
auto const union_			 = kw_(lex::def::union_s_);
auto const unsigned_		 = kw_(lex::def::unsigned_s_);
auto const using_			 = kw_(lex::def::using_s_);
auto const virtual_			 = kw_(lex::def::virtual_s_);
auto const void_			 = kw_(lex::def::void_s_);
auto const volatile_		 = kw_(lex::def::volatile_s_);
auto const wchar_t_			 = kw_(lex::def::wchar_t_s_);
auto const while_			 = kw_(lex::def::while_s_);

auto const final_	 = id_("final");
auto const override_ = id_("override");

auto const import_ = id_("import");
auto const module_ = id_("module");

}	 // namespace lit

xxx_parser_declare(simple_template_id_);
xxx_parser_declare(identifier_);

//  A.2 Keywords [gram.key]

xxx_parser_define(typedef_name_, { return or_({simple_template_id_, identifier_})->parse(nodes, source); });
xxx_parser_define(namespace_alias_, { return identifier_->parse(nodes, source); });
xxx_parser_define(namespace_name_, { return or_({namespace_alias_, identifier_})->parse(nodes, source); });
xxx_parser_define(class_name_, { return or_({simple_template_id_, identifier_})->parse(nodes, source); });
xxx_parser_define(enum_name_, { return identifier_->parse(nodes, source); });
xxx_parser_define(template_name_, { return identifier_->parse(nodes, source); });

//	A.3 Lexical conventions

xxx_parser_declare(token_);
xxx_parser_declare(header_name_);
xxx_parser_declare(keyword_);
xxx_parser_define(import_keyword_, { return lit::import_->parse(nodes, source); });
xxx_parser_define(module_keyword_, { return lit::module_->parse(nodes, source); });
xxx_parser_define(export_keyword_, { return lit::export_->parse(nodes, source); });
xxx_parser_declare(literal_);
xxx_parser_declare(integer_literal_);
xxx_parser_declare(binary_literal_);
xxx_parser_declare(octal_literal_);
xxx_parser_declare(decimal_literal_);
xxx_parser_declare(hexadecimal_literal_);
xxx_parser_declare(character_literal_);
xxx_parser_declare(floating_point_literal_);
xxx_parser_declare(string_literal_);
xxx_parser_define(boolean_literal_, { return kw_set_({lex::def::true_s_, lex::def::false_s_})->parse(nodes, source); });
xxx_parser_define(pointer_literal_, { return lit::nullptr_->parse(nodes, source); });
xxx_parser_declare(user_defined_literal_);
xxx_parser_declare(user_defined_integer_literal_);
xxx_parser_declare(user_defined_floating_point_literal_);
xxx_parser_declare(user_defined_string_literal_);
xxx_parser_declare(user_defined_character_literal_);

// TODO:
xxx_parser_impl(token_, { return kw_("TODO:")->parse(nodes, source); });
xxx_parser_impl(header_name_, { return kw_("TODO:")->parse(nodes, source); });
xxx_parser_impl(identifier_, { return kw_("TODO:")->parse(nodes, source); });
xxx_parser_define(identifier_list_, { return seq_({identifier_, zom_(seq_({lit::comma_, identifier_}))})->parse(nodes, source); });
xxx_parser_impl(keyword_, { return kw_("TODO:")->parse(nodes, source); });
xxx_parser_impl(literal_, { return kw_("TODO:")->parse(nodes, source); });
xxx_parser_impl(integer_literal_, { return kw_("TODO:")->parse(nodes, source); });
xxx_parser_impl(binary_literal_, { return kw_("TODO:")->parse(nodes, source); });
xxx_parser_impl(octal_literal_, { return kw_("TODO:")->parse(nodes, source); });
xxx_parser_impl(decimal_literal_, { return kw_("TODO:")->parse(nodes, source); });
xxx_parser_impl(hexadecimal_literal_, { return kw_("TODO:")->parse(nodes, source); });
xxx_parser_impl(character_literal_, { return kw_("TODO:")->parse(nodes, source); });
xxx_parser_impl(floating_point_literal_, { return kw_("TODO:")->parse(nodes, source); });
xxx_parser_impl(string_literal_, { return kw_("TODO:")->parse(nodes, source); });
xxx_parser_impl(user_defined_literal_, { return kw_("TODO:")->parse(nodes, source); });
xxx_parser_impl(user_defined_integer_literal_, { return kw_("TODO:")->parse(nodes, source); });
xxx_parser_impl(user_defined_floating_point_literal_, { return kw_("TODO:")->parse(nodes, source); });
xxx_parser_impl(user_defined_string_literal_, { return kw_("TODO:")->parse(nodes, source); });
xxx_parser_impl(user_defined_character_literal_, { return kw_("TODO:")->parse(nodes, source); });

//	A.4 Basics [gram.basic]

xxx_parser_declare(translation_unit_);

//	A.5 Expressions

xxx_parser_declare(primary_expression_);
xxx_parser_declare(id_expression_);
xxx_parser_declare(unqualified_id_);
xxx_parser_declare(qualified_id_);
xxx_parser_declare(nested_name_specifier_);
xxx_parser_declare(lambda_expression_);
xxx_parser_declare(lambda_introducer_);
xxx_parser_declare(lambda_declarator_);
xxx_parser_declare(lambda_specifier_);
xxx_parser_declare(lambda_capture_);
xxx_parser_declare(capture_default_);
xxx_parser_declare(capture_list_);
xxx_parser_declare(capture_);
xxx_parser_declare(simple_capture_);
xxx_parser_declare(init_capture_);
xxx_parser_declare(fold_expression_);
xxx_parser_declare(fold_operator_);
xxx_parser_declare(requires_expression_);
xxx_parser_declare(requirement_parameter_list_);
xxx_parser_declare(requirement_body_);
xxx_parser_declare(requirement_);
xxx_parser_declare(simple_requirement_);
xxx_parser_declare(type_requirement_);
xxx_parser_declare(compound_requirement_);
xxx_parser_declare(return_type_requirement_);
xxx_parser_declare(nested_requirement_);
xxx_parser_declare(postfix_expression_);
xxx_parser_declare(expression_list_);
xxx_parser_declare(unary_expression_);
xxx_parser_declare(unary_operator_);
xxx_parser_declare(await_expression_);
xxx_parser_declare(noexcept_expression_);
xxx_parser_declare(new_expression_);
xxx_parser_declare(new_placement_);
xxx_parser_declare(new_type_id_);
xxx_parser_declare(new_declarator_);
xxx_parser_declare(noptr_new_declarator_);
xxx_parser_declare(new_initializer_);
xxx_parser_declare(delete_expression_);
xxx_parser_declare(cast_expression_);
xxx_parser_declare(pm_expression_);
xxx_parser_declare(multiplicative_expression_);
xxx_parser_declare(additive_expression_);
xxx_parser_declare(shift_expression_);
xxx_parser_declare(compare_expression_);
xxx_parser_declare(relational_expression_);
xxx_parser_declare(equality_expression_);
xxx_parser_declare(and_expression_);
xxx_parser_declare(exclusive_or_expression_);
xxx_parser_declare(inclusive_or_expression_);
xxx_parser_declare(logical_and_expression_);
xxx_parser_declare(logical_or_expression_);
xxx_parser_declare(conditional_expression_);
xxx_parser_declare(yield_expression_);
xxx_parser_declare(throw_expression_);
xxx_parser_declare(assignment_expression_);
xxx_parser_declare(assignment_operator_);
xxx_parser_declare(expression_);
xxx_parser_declare(constant_expression_);

//	A.6 Statements

xxx_parser_declare(statement_);
xxx_parser_declare(init_statement_);
xxx_parser_declare(condition_);
xxx_parser_declare(label_);
xxx_parser_declare(labeled_statement_);
xxx_parser_declare(expression_statement_);
xxx_parser_declare(compound_statement_);
xxx_parser_declare(selection_statement_);
xxx_parser_declare(iteration_statement_);
xxx_parser_declare(for_range_declaration_);
xxx_parser_declare(for_range_initializer_);
xxx_parser_declare(jump_statement_);
xxx_parser_declare(coroutine_return_statement_);
xxx_parser_declare(declaration_statement_);

//	A.7 Declarations

xxx_parser_declare(declaration_);
xxx_parser_declare(name_declaration_);
xxx_parser_declare(special_declaration_);
xxx_parser_declare(block_declaration_);
xxx_parser_declare(nodeclspec_function_declaration_);
xxx_parser_declare(alias_declaration_);
xxx_parser_declare(simple_declaration_);
xxx_parser_declare(static_assert_declaration_);
xxx_parser_declare(empty_declaration_);
xxx_parser_declare(attribute_declaration_);
xxx_parser_declare(decl_specifier_);
xxx_parser_declare(decl_specifier_seq_);
xxx_parser_declare(storage_class_specifier_);
xxx_parser_declare(function_specifier_);
xxx_parser_declare(explicit_specifier_);
xxx_parser_declare(type_specifier_);
xxx_parser_declare(defining_type_specifier_);
xxx_parser_declare(simple_type_specifier_);
xxx_parser_declare(type_name_);
xxx_parser_declare(elaborated_type_specifier_);
xxx_parser_declare(decltype_specifier_);
xxx_parser_declare(placeholder_type_specifier_);
xxx_parser_declare(init_declarator_list_);
xxx_parser_declare(init_declarator_);
xxx_parser_declare(declarator_);
xxx_parser_declare(ptr_declarator_);
xxx_parser_declare(noptr_declarator_);
xxx_parser_declare(parameters_and_qualifiers_);
xxx_parser_declare(trailing_return_type_);
xxx_parser_declare(ptr_operator_);
xxx_parser_declare(cv_qualifier_);
xxx_parser_declare(ref_qualifier_);
xxx_parser_declare(declarator_id_);
xxx_parser_declare(type_id_);
xxx_parser_declare(defining_type_id_);
xxx_parser_declare(abstract_declarator_);
xxx_parser_declare(ptr_abstract_declarator_);
xxx_parser_declare(noptr_abstract_declarator_);
xxx_parser_declare(abstract_pack_declarator_);
xxx_parser_declare(noptr_abstract_pack_declarator_);
xxx_parser_declare(parameter_declaration_clause_);
xxx_parser_declare(parameter_declaration_list_);
xxx_parser_declare(parameter_declaration_);
xxx_parser_declare(initializer_);
xxx_parser_declare(brace_or_equal_initializer_);
xxx_parser_declare(initializer_clause_);
xxx_parser_declare(braced_init_list_);
xxx_parser_declare(initializer_list_);
xxx_parser_declare(designated_initializer_list_);
xxx_parser_declare(designated_initializer_clause_);
xxx_parser_declare(designator_);
xxx_parser_declare(expr_or_braced_init_list_);
xxx_parser_declare(function_definition_);
xxx_parser_declare(function_body_);
xxx_parser_declare(enum_specifier_);
xxx_parser_declare(enum_head_);
xxx_parser_declare(enum_head_name_);
xxx_parser_declare(opaque_enum_declaration_);
xxx_parser_declare(enum_key_);
xxx_parser_declare(enum_base_);
xxx_parser_declare(enumerator_list_);
xxx_parser_declare(enumerator_definition_);
xxx_parser_declare(enumerator_);
xxx_parser_declare(using_enum_declaration_);
xxx_parser_declare(using_enum_declarator_);
xxx_parser_declare(namespace_definition_);
xxx_parser_declare(named_namespace_definition_);
xxx_parser_declare(unnamed_namespace_definition_);
xxx_parser_declare(nested_namespace_definition_);
xxx_parser_declare(enclosing_namespace_specifier_);
xxx_parser_declare(namespace_body_);
xxx_parser_declare(namespace_alias_definition_);
xxx_parser_declare(qualified_namespace_specifier_);
xxx_parser_declare(using_directive_);
xxx_parser_declare(using_declaration_);
xxx_parser_declare(using_declarator_list_);
xxx_parser_declare(using_declarator_);
xxx_parser_declare(asm_declaration_);
xxx_parser_declare(linkage_specification_);
xxx_parser_declare(attribute_specifier_);
xxx_parser_declare(alignment_specifier_);
xxx_parser_declare(attribute_using_prefix_);
xxx_parser_declare(attribute_list_);
xxx_parser_declare(attribute_);
xxx_parser_declare(attribute_token_);
xxx_parser_declare(attribute_scoped_token_);
xxx_parser_declare(attribute_namespace_);
xxx_parser_declare(attribute_argument_clause_);
xxx_parser_declare(balanced_token_);

//	A.8 Modules

xxx_parser_declare(module_declaration_);
xxx_parser_declare(module_name_);
xxx_parser_declare(module_partition_);
xxx_parser_declare(module_name_qualifier_);
xxx_parser_declare(export_declaration_);
xxx_parser_declare(module_import_declaration_);
xxx_parser_declare(global_module_fragment_);
xxx_parser_declare(private_module_fragment_);

//	A.9 Classes

xxx_parser_declare(class_specifier_);
xxx_parser_declare(class_head_);
xxx_parser_declare(class_head_name_);
xxx_parser_declare(class_virt_specifier_);
xxx_parser_declare(class_key_);
xxx_parser_declare(member_specification_);
xxx_parser_declare(member_declaration_);
xxx_parser_declare(member_declarator_list_);
xxx_parser_declare(member_declarator_);
xxx_parser_declare(virt_specifier_);
xxx_parser_declare(pure_specifier_);
xxx_parser_declare(conversion_function_id_);
xxx_parser_declare(conversion_type_id_);
xxx_parser_declare(conversion_declarator_);
xxx_parser_declare(base_clause_);
xxx_parser_declare(base_specifier_list_);
xxx_parser_declare(base_specifier_);
xxx_parser_declare(class_or_decltype_);
xxx_parser_declare(access_specifier_);
xxx_parser_declare(ctor_initializer_);
xxx_parser_declare(mem_initializer_list_);
xxx_parser_declare(mem_initializer_);
xxx_parser_declare(mem_initializer_id_);

//	A.10 Overloading

xxx_parser_declare(operator_function_id_);
xxx_parser_declare(operator_);
xxx_parser_declare(literal_operator_id_);

//	A.11 Templates

xxx_parser_declare(template_declaration_);
xxx_parser_declare(template_head_);
xxx_parser_declare(template_parameter_list_);
xxx_parser_declare(requires_clause_);
xxx_parser_declare(constraint_logical_or_expression_);
xxx_parser_declare(constraint_logical_and_expression_);
xxx_parser_declare(template_parameter_);
xxx_parser_declare(type_parameter_);
xxx_parser_declare(type_parameter_key_);
xxx_parser_declare(type_constraint_);
xxx_parser_declare(template_id_);
xxx_parser_declare(template_argument_list_);
xxx_parser_declare(template_argument_);
xxx_parser_declare(constraint_expression_);
xxx_parser_declare(deduction_guide_);
xxx_parser_declare(concept_definition_);
xxx_parser_declare(concept_name_);
xxx_parser_declare(typename_specifier_);
xxx_parser_declare(explicit_instantiation_);
xxx_parser_declare(explicit_specialization_);

//	A.12 Exception handling

xxx_parser_declare(try_block_);
xxx_parser_declare(function_try_block_);
xxx_parser_declare(handler_);
xxx_parser_declare(exception_declaration_);
xxx_parser_declare(noexcept_specifier_);

//	A.13 Preprocessing directives

//	--------------------------------------------

//	A.4 Basics [gram.basic]

xxx_parser_impl(translation_unit_, { return or_({zom_(declaration_), seq_({opt_(global_module_fragment_), module_declaration_, zom_(declaration_), opt_(private_module_fragment_)})})->parse(nodes, source); });

//	A.5 Expressions

xxx_parser_impl(primary_expression_, { return or_({literal_, lit::this_, seq_({lit::lp_, expression_, lit::rp_}), id_expression_, lambda_expression_, fold_expression_, requires_expression_})->parse(nodes, source); });
xxx_parser_impl(id_expression_, { return or_({qualified_id_, unqualified_id_})->parse(nodes, source); });
xxx_parser_impl(unqualified_id_, { return or_({identifier_, operator_function_id_, conversion_function_id_, literal_operator_id_, seq_({lit::tilde_, type_name_}), seq_({lit::tilde_, decltype_specifier_}), template_id_})->parse(nodes, source); });
xxx_parser_impl(qualified_id_, { return seq_({nested_name_specifier_, opt_(lit::template_), unqualified_id_})->parse(nodes, source); });
xxx_parser_impl(nested_name_specifier_, { return or_({lit::scope_, seq_({type_name_, lit::scope_}), seq_({namespace_name_, lit::scope_}), seq_({decltype_specifier_, lit::scope_}), seq_({nested_name_specifier_, or_({seq_({opt_(lit::template_), simple_template_id_}), identifier_}), lit::scope_})})->parse(nodes, source); });
xxx_parser_impl(lambda_expression_, {
	return or_({seq_({lambda_introducer_, zom_(attribute_specifier_), lambda_declarator_, compound_statement_}),
				seq_({
					lambda_introducer_,
					lit::lt_,
					template_parameter_list_,
					lit::gt_,
					opt_(requires_clause_),
					zom_(attribute_specifier_),
				}),
				seq_({lambda_declarator_, compound_statement_})})
		->parse(nodes, source);
});
xxx_parser_impl(lambda_introducer_, { return seq_({lit::lbk_, opt_(lambda_capture_), lit::rbk_})->parse(nodes, source); });
xxx_parser_impl(lambda_declarator_, {
	return or_({seq_({oom_(lambda_specifier_), opt_(noexcept_specifier_), zom_(attribute_specifier_), opt_(trailing_return_type_)}),
				seq_({noexcept_specifier_, zom_(attribute_specifier_), opt_(trailing_return_type_)}),
				opt_(trailing_return_type_),
				seq_({lit::lp_, parameter_declaration_clause_, lit::rp_, zom_(lambda_specifier_), opt_(noexcept_specifier_), zom_(attribute_specifier_), opt_(trailing_return_type_), opt_(requires_clause_)})})
		->parse(nodes, source);
});
xxx_parser_impl(lambda_specifier_, { return or_({lit::consteval_, lit::constexpr_, lit::mutable_, lit::static_})->parse(nodes, source); });
xxx_parser_impl(lambda_capture_, { return or_({seq_({capture_default_, opt_(seq_({lit::comma_, capture_list_}))}), capture_list_})->parse(nodes, source); });
xxx_parser_impl(capture_default_, { return op_set_({"&", "="})->parse(nodes, source); });
xxx_parser_impl(capture_list_, { return seq_({capture_, zom_(seq_({lit::comma_, capture_}))})->parse(nodes, source); });
xxx_parser_impl(capture_, { return or_({simple_capture_, init_capture_})->parse(nodes, source); });
xxx_parser_impl(simple_capture_, { return or_({seq_({identifier_, lit::ellipsis_}), seq_({lit::amp_, identifier_, lit::ellipsis_}), lit::this_, seq_({lit::star_, lit::this_})})->parse(nodes, source); });
xxx_parser_impl(init_capture_, { return seq_({opt_(lit::amp_), opt_(lit::ellipsis_), identifier_, initializer_})->parse(nodes, source); });
xxx_parser_impl(fold_expression_, { return seq_({lit::lp_, or_({seq_({lit::ellipsis_, fold_operator_, cast_expression_}), seq_({constant_expression_, fold_operator_, lit::ellipsis_, opt_(seq_({fold_operator_, cast_expression_}))})}), lit::rp_})->parse(nodes, source); });
xxx_parser_impl(fold_operator_, {
	return op_set_({"->*", "->",
				 "==", "!=", "<=", ">=", "&&", "||", ",", ".*",
				 "*=", "/=", "%=", "+=", "-=", "^", "&=", "!", "<<=", ">>=",
				 "*", "/", "%", "+", "-", "^=", "&", "|=", "<<", ">>",
				 "<", ">"})
		->parse(nodes, source);
});
xxx_parser_impl(requires_expression_, { return seq_({lit::requires_, opt_(requirement_parameter_list_), requirement_body_})->parse(nodes, source); });
xxx_parser_impl(requirement_parameter_list_, { return seq_({lit::lp_, parameter_declaration_clause_, lit::rp_})->parse(nodes, source); });
xxx_parser_impl(requirement_body_, { return seq_({lit::lbc_, oom_(requirement_), lit::rbc_})->parse(nodes, source); });
xxx_parser_impl(requirement_, { return or_({simple_requirement_, type_requirement_, compound_requirement_, nested_requirement_})->parse(nodes, source); });
xxx_parser_impl(simple_requirement_, { return seq_({expression_, lit::semi_})->parse(nodes, source); });
xxx_parser_impl(type_requirement_, { return seq_({lit::typename_, opt_(nested_name_specifier_), type_name_, lit::semi_})->parse(nodes, source); });
xxx_parser_impl(compound_requirement_, { return seq_({lit::lbc_, expression_, lit::rbc_, opt_(lit::noexcept_), opt_(return_type_requirement_), lit::semi_})->parse(nodes, source); });
xxx_parser_impl(return_type_requirement_, { return seq_({lit::arrow_, type_constraint_})->parse(nodes, source); });
xxx_parser_impl(nested_requirement_, { return seq_({lit::requires_, constraint_expression_, lit::semi_})->parse(nodes, source); });
xxx_parser_impl(postfix_expression_, {
	return seq_({or_({primary_expression_, seq_({or_({typename_specifier_, simple_type_specifier_}), or_({seq_({lit::lp_, opt_(expression_list_), lit::rp_}), braced_init_list_})}), seq_({kw_set_({lex::def::dynamic_cast_s_, lex::def::static_cast_s_, lex::def::reinterpret_cast_s_, lex::def::const_cast_s_}), lit::lt_, type_id_, lit::gt_, lit::lp_, expression_, lit::rp_}), seq_({lit::typeid_, lit::lp_, or_({type_id_, expression_}), lit::rp_})}),
				 zom_(or_({seq_({lit::lbk_, opt_(expression_list_), lit::rbk_}), seq_({lit::lp_, opt_(expression_list_), lit::rp_}), seq_({op_set_({"->", "."}), opt_(lit::template_), id_expression_}), op_set_({"++", "--"})}))})
		->parse(nodes, source);
});
xxx_parser_impl(expression_list_, { return initializer_list_->parse(nodes, source); });
xxx_parser_impl(unary_expression_, {
	return or_({seq_({op_set_({"++", "--"}), cast_expression_}),
				seq_({unary_operator_, cast_expression_}),
				await_expression_,
				noexcept_expression_,
				new_expression_,
				delete_expression_,
				seq_({lit::alignof_, lit::lp_, type_id_, lit::rp_}),
				seq_({lit::sizeof_, or_({seq_({lit::ellipsis_, lit::lp_, identifier_, lit::rp_}), seq_({lit::lp_, type_id_, lit::rp_}), unary_expression_})}),
				postfix_expression_})
		->parse(nodes, source);
});
xxx_parser_impl(unary_operator_, { return op_set_({"&", "*", "+", "-", "~", "!"})->parse(nodes, source); });
xxx_parser_impl(await_expression_, { return seq_({lit::co_await_, cast_expression_})->parse(nodes, source); });
xxx_parser_impl(noexcept_expression_, { return seq_({lit::noexcept_, lit::lp_, expression_, lit::rp_})->parse(nodes, source); });
xxx_parser_impl(new_expression_, {
	return seq_({opt_(lit::scope_), lit::new_, opt_(new_placement_),
				 or_({seq_({lit::lp_, type_id_, lit::rp_}), new_type_id_}),
				 opt_(new_initializer_)})
		->parse(nodes, source);
});
xxx_parser_impl(new_placement_, { return seq_({lit::lp_, expression_list_, lit::rp_})->parse(nodes, source); });
xxx_parser_impl(new_type_id_, { return seq_({oom_(type_specifier_), opt_(attribute_specifier_), opt_(new_declarator_)})->parse(nodes, source); });
xxx_parser_impl(new_declarator_, { return seq_({zom_(ptr_operator_), opt_(noptr_new_declarator_)})->parse(nodes, source); });
xxx_parser_impl(noptr_new_declarator_, { return seq_({zom_(seq_({lit::lbk_, opt_(expression_), lit::rbk_})), zom_(attribute_specifier_), lit::lbk_, constant_expression_, lit::rbk_, zom_(attribute_specifier_)})->parse(nodes, source); });
xxx_parser_impl(new_initializer_, { return or_({seq_({lit::lp_, opt_(expression_list_), lit::rp_}), braced_init_list_})->parse(nodes, source); });
xxx_parser_impl(delete_expression_, { return seq_({opt_(lit::scope_), lit::delete_, opt_(seq_({lit::lbk_, lit::rbk_})), cast_expression_})->parse(nodes, source); });
xxx_parser_impl(cast_expression_, { return seq_({zom_(seq_({lit::lp_, type_id_, lit::rp_})), unary_expression_})->parse(nodes, source); });
xxx_parser_impl(pm_expression_, { return seq_({cast_expression_, zom_(seq_({op_set_({"->*", ".*"}), cast_expression_}))})->parse(nodes, source); });
xxx_parser_impl(multiplicative_expression_, { return seq_({pm_expression_, zom_(seq_({op_set_({"*", "/", "%"}), pm_expression_}))})->parse(nodes, source); });
xxx_parser_impl(additive_expression_, { return seq_({multiplicative_expression_, zom_(seq_({op_set_({"+", "-"}), multiplicative_expression_}))})->parse(nodes, source); });
xxx_parser_impl(shift_expression_, { return seq_({additive_expression_, zom_(seq_({op_set_({">>", "<<"}), additive_expression_}))})->parse(nodes, source); });
xxx_parser_impl(compare_expression_, { return seq_({shift_expression_, zom_(seq_({lit::spaceship_, shift_expression_}))})->parse(nodes, source); });
xxx_parser_impl(relational_expression_, { return seq_({compare_expression_, zom_(seq_({op_set_({"<=", ">=", "<", ">"}), compare_expression_}))})->parse(nodes, source); });
xxx_parser_impl(equality_expression_, { return seq_({relational_expression_, zom_(seq_({op_set_({"!=", "=="}), relational_expression_}))})->parse(nodes, source); });
xxx_parser_impl(and_expression_, { return seq_({equality_expression_, zom_(seq_({lit::amp_, equality_expression_}))})->parse(nodes, source); });
xxx_parser_impl(exclusive_or_expression_, { return seq_({and_expression_, zom_(seq_({lit::hut_, and_expression_}))})->parse(nodes, source); });
xxx_parser_impl(inclusive_or_expression_, { return seq_({exclusive_or_expression_, zom_(seq_({lit::vl_, exclusive_or_expression_}))})->parse(nodes, source); });
xxx_parser_impl(logical_and_expression_, { return seq_({inclusive_or_expression_, zom_(seq_({lit::amp2_, inclusive_or_expression_}))})->parse(nodes, source); });
xxx_parser_impl(logical_or_expression_, { return seq_({logical_and_expression_, zom_(seq_({lit::vl2_, logical_and_expression_}))})->parse(nodes, source); });
xxx_parser_impl(conditional_expression_, { return seq_({logical_or_expression_, opt_(seq_({lit::q_, expression_, lit::col_, assignment_expression_}))})->parse(nodes, source); });
xxx_parser_impl(yield_expression_, { return seq_({lit::co_yield_, or_({braced_init_list_, assignment_expression_})})->parse(nodes, source); });
xxx_parser_impl(throw_expression_, { return seq_({lit::throw_, opt_(assignment_expression_)})->parse(nodes, source); });
xxx_parser_impl(assignment_expression_, { return or_({conditional_expression_, yield_expression_, throw_expression_, seq_({logical_or_expression_, assignment_operator_, initializer_clause_})})->parse(nodes, source); });
xxx_parser_impl(assignment_operator_, { return op_set_({"=", "*=", "/=", "%=", "+=", "-=", "<<=", ">>=", "&=", "~=", "|="})->parse(nodes, source); });
xxx_parser_impl(expression_, { return seq_({assignment_expression_, zom_(seq_({lit::comma_, assignment_expression_}))})->parse(nodes, source); });
xxx_parser_impl(constant_expression_, { return conditional_expression_->parse(nodes, source); });

//	A.6 Statements

xxx_parser_impl(statement_, {
	return or_({
				   labeled_statement_,
				   seq_({zom_(attribute_specifier_), or_({expression_statement_, compound_statement_, selection_statement_, iteration_statement_, jump_statement_})}),
				   seq_({zom_(attribute_specifier_), try_block_}),
				   declaration_statement_,
			   })
		->parse(nodes, source);
});
xxx_parser_impl(init_statement_, { return or_({expression_statement_, simple_declaration_, alias_declaration_})->parse(nodes, source); });
xxx_parser_impl(condition_, { return or_({seq_({zom_(attribute_specifier_), oom_(decl_specifier_), declarator_, brace_or_equal_initializer_}), expression_})->parse(nodes, source); });
xxx_parser_impl(label_, { return seq_({zom_(attribute_specifier_), or_({lit::default_, seq_({lit::case_, constant_expression_}), identifier_}), lit::col_})->parse(nodes, source); });
xxx_parser_impl(labeled_statement_, { return seq_({label_, statement_})->parse(nodes, source); });
xxx_parser_impl(expression_statement_, { return seq_({opt_(expression_), lit::semi_})->parse(nodes, source); });
xxx_parser_impl(compound_statement_, { return seq_({lit::lbc_, zom_(statement_), zom_(label_), lit::rbc_})->parse(nodes, source); });
xxx_parser_impl(selection_statement_, { return seq_({lit::lbc_, or_({seq_({lit::if_, opt_(lit::constexpr_), lit::lp_, opt_(init_statement_), condition_, lit::rp_, statement_, opt_(seq_({lit::else_, statement_}))}), seq_({lit::if_, opt_(lit::ex_), lit::consteval_, compound_statement_, opt_(seq_({lit::else_, statement_}))}), seq_({lit::switch_, lit::lp_, opt_(init_statement_), condition_, lit::rp_, statement_})}), lit::rbc_})->parse(nodes, source); });
xxx_parser_impl(iteration_statement_, {
	return or_({
				   seq_({lit::while_, lit::lp_, condition_, lit::rp_, statement_}),
				   seq_({lit::do_, statement_, lit::while_, lit::lp_, expression_, lit::rp_, lit::semi_}),
				   seq_({lit::for_, lit::lp_, or_({seq_({init_statement_, opt_(condition_), lit::semi_, opt_(expression_)}), seq_({opt_(init_statement_), for_range_declaration_, lit::col_, for_range_initializer_})}), lit::rp_, statement_}),
			   })
		->parse(nodes, source);
});
xxx_parser_impl(for_range_declaration_, { return seq_({zom_(attribute_specifier_), decl_specifier_seq_, or_({seq_({opt_(ref_qualifier_), lit::lbk_, identifier_list_, lit::rbk_}), declarator_})})->parse(nodes, source); });
xxx_parser_impl(for_range_initializer_, { return expr_or_braced_init_list_->parse(nodes, source); });
xxx_parser_impl(jump_statement_, {
	return or_({seq_({lit::break_, lit::semi_}),
				seq_({lit::continue_, lit::semi_}),
				seq_({lit::goto_, identifier_, lit::semi_}),
				seq_({lit::return_, opt_(expr_or_braced_init_list_), lit::semi_}),
				coroutine_return_statement_})
		->parse(nodes, source);
});
xxx_parser_impl(coroutine_return_statement_, { return seq_({lit::co_return_, opt_(expr_or_braced_init_list_), lit::semi_})->parse(nodes, source); });
xxx_parser_impl(declaration_statement_, { return block_declaration_->parse(nodes, source); });

//	A.7 Declarations

xxx_parser_impl(declaration_, { return or_({special_declaration_, name_declaration_})->parse(nodes, source); });
xxx_parser_impl(name_declaration_, {
	return or_({
				   block_declaration_,
				   nodeclspec_function_declaration_,
				   function_definition_,
				   template_declaration_,
				   deduction_guide_,
				   linkage_specification_,
				   namespace_definition_,
				   empty_declaration_,
				   attribute_declaration_,
				   module_import_declaration_,

			   })
		->parse(nodes, source);
});
xxx_parser_impl(special_declaration_, { return or_({export_declaration_, explicit_instantiation_, explicit_specialization_})->parse(nodes, source); });
xxx_parser_impl(block_declaration_, {
	return or_({asm_declaration_,
				namespace_alias_definition_,
				using_declaration_,
				using_enum_declaration_,
				using_directive_,
				static_assert_declaration_,
				alias_declaration_,
				opaque_enum_declaration_,
				simple_declaration_})
		->parse(nodes, source);
});
xxx_parser_impl(nodeclspec_function_declaration_, { return seq_({zom_(attribute_specifier_), declarator_, lit::semi_})->parse(nodes, source); });
xxx_parser_impl(alias_declaration_, { return seq_({lit::using_, identifier_, zom_(attribute_specifier_), lit::eq_, defining_type_id_, lit::semi_})->parse(nodes, source); });
xxx_parser_impl(simple_declaration_, {
	return or_({seq_({oom_(decl_specifier_), opt_(init_declarator_list_), lit::semi_}),
				seq_({oom_(attribute_specifier_), decl_specifier_seq_, init_declarator_list_, lit::semi_}),
				seq_({zom_(attribute_specifier_), decl_specifier_seq_, opt_(ref_qualifier_), lit::lbk_, identifier_list_, lit::rbk_, initializer_, lit::semi_})})
		->parse(nodes, source);
});
xxx_parser_impl(static_assert_declaration_, { return seq_({lit::static_assert_, lit::lp_, constant_expression_, opt_(seq_({lit::comma_, string_literal_})), lit::rp_, lit::semi_})->parse(nodes, source); });
xxx_parser_impl(empty_declaration_, { return seq_({lit::semi_})->parse(nodes, source); });
xxx_parser_impl(attribute_declaration_, { return seq_({oom_(attribute_specifier_), lit::semi_})->parse(nodes, source); });
xxx_parser_impl(decl_specifier_, { return or_({lit::friend_, lit::typedef_, lit::constexpr_, lit::consteval_, lit::constinit_, lit::inline_, storage_class_specifier_, defining_type_specifier_, function_specifier_})->parse(nodes, source); });
xxx_parser_impl(decl_specifier_seq_, { return seq_({oom_(decl_specifier_), zom_(attribute_specifier_)})->parse(nodes, source); });
xxx_parser_impl(storage_class_specifier_, { return kw_set_({lex::def::static_s_, lex::def::thread_local_s_, lex::def::extern_s_, lex::def::mutable_s_})->parse(nodes, source); });
xxx_parser_impl(function_specifier_, { return or_({lit::virtual_, explicit_specifier_})->parse(nodes, source); });
xxx_parser_impl(explicit_specifier_, { return seq_({lit::explicit_, opt_(seq_({lit::lp_, constant_expression_, lit::rp_}))})->parse(nodes, source); });
xxx_parser_impl(type_specifier_, { return or_({cv_qualifier_, typename_specifier_, simple_type_specifier_, elaborated_type_specifier_})->parse(nodes, source); });
xxx_parser_impl(defining_type_specifier_, { return or_({type_specifier_, class_specifier_, enum_specifier_})->parse(nodes, source); });
xxx_parser_impl(simple_type_specifier_, {
	return or_({kw_set_({lex::def::char_s_, lex::def::char8_t_s_, lex::def::char16_t_s_, lex::def::char32_t_s_, lex::def::wchar_t_s_, lex::def::bool_s_, lex::def::short_s_, lex::def::int_s_, lex::def::long_s_, lex::def::signed_s_, lex::def::unsigned_s_, lex::def::float_s_, lex::def::double_s_, lex::def::void_s_}),
				seq_({opt_(nested_name_specifier_), type_name_}),
				seq_({nested_name_specifier_, lit::template_, simple_template_id_}),
				decltype_specifier_,
				placeholder_type_specifier_,
				seq_({opt_(nested_name_specifier_), template_name_})})
		->parse(nodes, source);
});
xxx_parser_impl(type_name_, { return or_({class_name_, enum_name_, typedef_name_})->parse(nodes, source); });
xxx_parser_impl(elaborated_type_specifier_, {
	return or_({seq_({class_key_, or_({zom_(attribute_specifier_), opt_(nested_name_specifier_), identifier_}), seq_({nested_name_specifier_, opt_(lit::template_), simple_template_id_}), simple_template_id_}),
				seq_({lit::enum_, opt_(nested_name_specifier_), identifier_})})
		->parse(nodes, source);
});
xxx_parser_impl(decltype_specifier_, { return seq_({lit::decltype_, lit::lp_, expression_, lit::rp_})->parse(nodes, source); });
xxx_parser_impl(placeholder_type_specifier_, { return seq_({opt_(type_constraint_), or_({lit::auto_, seq_({lit::decltype_, lit::lp_, lit::auto_, lit::rp_})})})->parse(nodes, source); });
xxx_parser_impl(init_declarator_list_, { return seq_({init_declarator_, zom_(seq_({lit::comma_, init_declarator_}))})->parse(nodes, source); });
xxx_parser_impl(init_declarator_, { return seq_({declarator_, or_({requires_clause_, opt_(initializer_)})})->parse(nodes, source); });
xxx_parser_impl(declarator_, { return or_({ptr_declarator_, seq_({noptr_declarator_, parameters_and_qualifiers_, trailing_return_type_})})->parse(nodes, source); });
xxx_parser_impl(ptr_declarator_, { return seq_({zom_(ptr_operator_), noptr_declarator_})->parse(nodes, source); });
xxx_parser_impl(noptr_declarator_, {
	return seq_({or_({seq_({lit::lp_, ptr_declarator_, lit::rp_}), seq_({declarator_id_, zom_(attribute_specifier_)})}),
				 zom_(or_({seq_({lit::lbk_, opt_(constant_expression_), lit::rbk_, zom_(attribute_specifier_)}), parameters_and_qualifiers_}))})
		->parse(nodes, source);
});
xxx_parser_impl(parameters_and_qualifiers_, {
	return or_({
				   seq_({lit::lp_, parameter_declaration_clause_, lit::rp_, zom_(cv_qualifier_)}),
				   seq_({opt_(ref_qualifier_), opt_(noexcept_specifier_), zom_(attribute_specifier_)}),
			   })
		->parse(nodes, source);
});
xxx_parser_impl(trailing_return_type_, { return seq_({lit::arrow_, type_id_})->parse(nodes, source); });
xxx_parser_impl(ptr_operator_, {
	return or_({
				   seq_({opt_(nested_name_specifier_), lit::star_, zom_(attribute_specifier_), zom_(cv_qualifier_)}),
				   seq_({op_set_({"&&", "&"}), zom_(attribute_specifier_)}),
			   })
		->parse(nodes, source);
});
xxx_parser_impl(cv_qualifier_, { return kw_set_({lex::def::const_s_, lex::def::volatile_s_})->parse(nodes, source); });
xxx_parser_impl(ref_qualifier_, { return op_set_({"&&", "&"})->parse(nodes, source); });
xxx_parser_impl(declarator_id_, { return seq_({opt_(lit::ellipsis_), id_expression_})->parse(nodes, source); });
xxx_parser_impl(type_id_, { return seq_({oom_(type_specifier_), opt_(abstract_declarator_)})->parse(nodes, source); });
xxx_parser_impl(defining_type_id_, { return seq_({oom_(defining_type_specifier_), opt_(abstract_declarator_)})->parse(nodes, source); });
xxx_parser_impl(abstract_declarator_, { return or_({ptr_abstract_declarator_, abstract_pack_declarator_, seq_({opt_(noptr_abstract_declarator_), parameters_and_qualifiers_, trailing_return_type_})})->parse(nodes, source); });
xxx_parser_impl(ptr_abstract_declarator_, { return or_({seq_({ptr_operator_, opt_(ptr_abstract_declarator_)}), noptr_abstract_declarator_})->parse(nodes, source); });
xxx_parser_impl(noptr_abstract_declarator_, { return seq_({opt_(seq_({lit::lp_, ptr_abstract_declarator_, lit::rp_})), or_({seq_({lit::lp_, opt_(constant_expression_), lit::rp_, zom_(attribute_specifier_)}), parameters_and_qualifiers_})})->parse(nodes, source); });
xxx_parser_impl(abstract_pack_declarator_, { return seq_({oom_(ptr_operator_), noptr_abstract_pack_declarator_})->parse(nodes, source); });
xxx_parser_impl(noptr_abstract_pack_declarator_, { return seq_({lit::ellipsis_, zom_(or_({seq_({lit::lbk_, opt_(constant_expression_), lit::rbk_, oom_(attribute_specifier_)}), parameters_and_qualifiers_}))})->parse(nodes, source); });
xxx_parser_impl(parameter_declaration_clause_, { return or_({seq_({parameter_declaration_list_, lit::comma_, lit::ellipsis_}), seq_({opt_(parameter_declaration_list_), opt_(lit::ellipsis_)})})->parse(nodes, source); });
xxx_parser_impl(parameter_declaration_list_, { return seq_({parameter_declaration_, zom_(seq_({lit::comma_, parameter_declaration_}))})->parse(nodes, source); });
xxx_parser_impl(parameter_declaration_, { return seq_({zom_(attribute_specifier_), or_({seq_({opt_(lit::this_), oom_(decl_specifier_), or_({opt_(abstract_declarator_), declarator_})}), seq_({oom_(decl_specifier_), or_({opt_(abstract_declarator_), declarator_}), lit::eq_, initializer_clause_})})})->parse(nodes, source); });
xxx_parser_impl(initializer_, { return or_({seq_({lit::lp_, expression_list_, lit::rp_}), brace_or_equal_initializer_})->parse(nodes, source); });
xxx_parser_impl(brace_or_equal_initializer_, { return or_({seq_({lit::eq_, initializer_clause_}), braced_init_list_})->parse(nodes, source); });
xxx_parser_impl(initializer_clause_, { return or_({assignment_expression_, braced_init_list_})->parse(nodes, source); });
xxx_parser_impl(braced_init_list_, { return seq_({lit::lp_, opt_(seq_({or_({designated_initializer_list_, initializer_list_}), opt_(lit::comma_)})), lit::rp_})->parse(nodes, source); });
xxx_parser_impl(initializer_list_, { return seq_({initializer_clause_, opt_(lit::ellipsis_), zom_(seq_({lit::comma_, initializer_clause_, opt_(lit::ellipsis_)}))})->parse(nodes, source); });
xxx_parser_impl(designated_initializer_list_, { return seq_({designated_initializer_clause_, zom_(seq_({lit::comma_, designated_initializer_clause_}))})->parse(nodes, source); });
xxx_parser_impl(designated_initializer_clause_, { return seq_({designator_, brace_or_equal_initializer_})->parse(nodes, source); });
xxx_parser_impl(designator_, { return seq_({lit::dot_, identifier_})->parse(nodes, source); });
xxx_parser_impl(expr_or_braced_init_list_, { return or_({expression_, braced_init_list_})->parse(nodes, source); });
xxx_parser_impl(function_definition_, { return seq_({zom_(attribute_specifier_), zom_(decl_specifier_), declarator_, or_({requires_clause_, zom_(virt_specifier_)}), function_body_})->parse(nodes, source); });
xxx_parser_impl(function_body_, { return or_({seq_({lit::eq_, kw_set_({lex::def::default_s_, lex::def::delete_s_}), lit::semi_}), function_try_block_, seq_({opt_(ctor_initializer_), compound_statement_})})->parse(nodes, source); });
xxx_parser_impl(enum_specifier_, { return seq_({enum_head_, lit::lbc_, or_({seq_({enumerator_list_, lit::comma_}), opt_(enumerator_list_)}), lit::rbc_})->parse(nodes, source); });
xxx_parser_impl(enum_head_, { return seq_({enum_key_, zom_(attribute_specifier_), opt_(enum_head_name_), opt_(enum_base_)})->parse(nodes, source); });
xxx_parser_impl(enum_head_name_, { return seq_({opt_(nested_name_specifier_), identifier_})->parse(nodes, source); });
xxx_parser_impl(opaque_enum_declaration_, { return seq_({enum_key_, zom_(attribute_specifier_), enum_head_name_, opt_(enum_base_), lit::semi_})->parse(nodes, source); });
xxx_parser_impl(enum_key_, { return seq_({lit::enum_, opt_(or_({lit::class_, lit::struct_}))})->parse(nodes, source); });
xxx_parser_impl(enum_base_, { return seq_({lit::col_, oom_(type_specifier_)})->parse(nodes, source); });
xxx_parser_impl(enumerator_list_, { return seq_({enumerator_definition_, oom_(seq_({lit::comma_, enumerator_definition_}))})->parse(nodes, source); });
xxx_parser_impl(enumerator_definition_, { return seq_({enumerator_, opt_(seq_({lit::eq_, constant_expression_}))})->parse(nodes, source); });
xxx_parser_impl(enumerator_, { return seq_({identifier_, zom_(attribute_specifier_)})->parse(nodes, source); });
xxx_parser_impl(using_enum_declaration_, { return seq_({lit::using_, lit::enum_, using_enum_declarator_, lit::semi_})->parse(nodes, source); });
xxx_parser_impl(using_enum_declarator_, { return seq_({opt_(nested_name_specifier_), or_({simple_template_id_, identifier_})})->parse(nodes, source); });
xxx_parser_impl(namespace_definition_, { return or_({named_namespace_definition_, nested_namespace_definition_, unnamed_namespace_definition_})->parse(nodes, source); });
xxx_parser_impl(named_namespace_definition_, { return seq_({opt_(lit::inline_), lit::namespace_, oom_(attribute_specifier_), identifier_, lit::lbc_, namespace_body_, lit::rbc_})->parse(nodes, source); });
xxx_parser_impl(unnamed_namespace_definition_, { return seq_({opt_(lit::inline_), lit::namespace_, oom_(attribute_specifier_), lit::lbc_, namespace_body_, lit::rbc_})->parse(nodes, source); });
xxx_parser_impl(nested_namespace_definition_, { return seq_({lit::namespace_, enclosing_namespace_specifier_, lit::scope_, opt_(lit::inline_), identifier_, lit::lbc_, namespace_body_, lit::rbc_})->parse(nodes, source); });
xxx_parser_impl(enclosing_namespace_specifier_, { return or_({seq_({enclosing_namespace_specifier_, lit::scope_, opt_(lit::inline_), identifier_}), identifier_})->parse(nodes, source); });
xxx_parser_impl(namespace_body_, { return oom_(declaration_)->parse(nodes, source); });
xxx_parser_impl(namespace_alias_definition_, { return seq_({lit::namespace_, identifier_, lit::eq_, qualified_namespace_specifier_, lit::semi_})->parse(nodes, source); });
xxx_parser_impl(qualified_namespace_specifier_, { return seq_({opt_(nested_name_specifier_), namespace_name_})->parse(nodes, source); });
xxx_parser_impl(using_directive_, { return seq_({zom_(attribute_specifier_), lit::using_, lit::namespace_, opt_(nested_name_specifier_), namespace_name_, lit::semi_})->parse(nodes, source); });
xxx_parser_impl(using_declaration_, { return seq_({lit::using_, using_declarator_list_, lit::semi_})->parse(nodes, source); });
xxx_parser_impl(using_declarator_list_, { return seq_({using_declarator_, opt_(lit::ellipsis_), zom_(seq_({lit::comma_, using_declarator_, opt_(lit::ellipsis_)}))})->parse(nodes, source); });
xxx_parser_impl(using_declarator_, { return seq_({opt_(lit::typename_), nested_name_specifier_, unqualified_id_})->parse(nodes, source); });
xxx_parser_impl(asm_declaration_, { return seq_({zom_(attribute_specifier_), lit::asm_, lit::rp_, string_literal_, lit::rp_, lit::semi_})->parse(nodes, source); });
xxx_parser_impl(linkage_specification_, { return seq_({lit::extern_, string_literal_, or_({seq_({lit::lbc_, zom_(declaration_), lit::rbc_}), name_declaration_})})->parse(nodes, source); });
xxx_parser_impl(attribute_specifier_, { return oom_(seq_({lit::lbk_, lit::lbk_, opt_(attribute_using_prefix_), attribute_list_, lit::rbk_, lit::rbk_}))->parse(nodes, source); });
xxx_parser_impl(alignment_specifier_, { return seq_({lit::alignas_, lit::lp_, or_({constant_expression_, type_id_}), opt_(lit::ellipsis_), lit::rp_})->parse(nodes, source); });
xxx_parser_impl(attribute_using_prefix_, { return seq_({lit::using_, attribute_namespace_, lit::semi_})->parse(nodes, source); });
xxx_parser_impl(attribute_list_, { return seq_({or_({seq_({attribute_, lit::ellipsis_}), opt_(attribute_)}), zom_(seq_({lit::comma_, or_({seq_({attribute_, lit::ellipsis_}), opt_(attribute_)})}))})->parse(nodes, source); });
xxx_parser_impl(attribute_, { return seq_({attribute_token_, opt_(attribute_argument_clause_)})->parse(nodes, source); });
xxx_parser_impl(attribute_token_, { return or_({attribute_scoped_token_, identifier_})->parse(nodes, source); });
xxx_parser_impl(attribute_scoped_token_, { return seq_({attribute_namespace_, lit::scope_, identifier_})->parse(nodes, source); });
xxx_parser_impl(attribute_namespace_, { return identifier_->parse(nodes, source); });
xxx_parser_impl(attribute_argument_clause_, { return seq_({lit::lp_, zom_(balanced_token_), lit::rp_})->parse(nodes, source); });
xxx_parser_impl(balanced_token_, {
	return or_({seq_({lit::lp_, opt_(balanced_token_), lit::rp_}),
				seq_({lit::lbk_, opt_(balanced_token_), lit::rbk_}),
				seq_({lit::lbc_, opt_(balanced_token_), lit::rbc_})})
		->parse(nodes, source);
	// any token other than a parenthesis, a bracket, or a brace
});

//	A.8 Modules

xxx_parser_impl(module_declaration_, { return seq_({opt_(export_keyword_), module_keyword_, module_name_, opt_(module_partition_), zom_(attribute_specifier_), lit::semi_})->parse(nodes, source); });
xxx_parser_impl(module_name_, { return seq_({opt_(module_name_qualifier_), identifier_})->parse(nodes, source); });
xxx_parser_impl(module_partition_, { return seq_({lit::col_, opt_(module_name_qualifier_), identifier_})->parse(nodes, source); });
xxx_parser_impl(module_name_qualifier_, { return seq_({opt_(module_name_qualifier_), identifier_, lit::dot_})->parse(nodes, source); });
xxx_parser_impl(export_declaration_, { return or_({seq_({lit::export_, or_({seq_({lit::lbc_, zom_(declaration_), lit::rbc_}), name_declaration_})}), seq_({export_keyword_, module_import_declaration_})})->parse(nodes, source); });
xxx_parser_impl(module_import_declaration_, { return seq_({import_keyword_, or_({header_name_, module_partition_, module_name_}), zom_(attribute_specifier_), lit::semi_})->parse(nodes, source); });
xxx_parser_impl(global_module_fragment_, { return seq_({module_keyword_, lit::semi_, zom_(declaration_)})->parse(nodes, source); });
xxx_parser_impl(private_module_fragment_, { return seq_({module_keyword_, lit::col_, lit::private_, lit::semi_, zom_(declaration_)})->parse(nodes, source); });

//	A.9 Classes [gram

xxx_parser_impl(class_specifier_, { return seq_({class_head_, lit::lbc_, opt_(member_specification_), lit::rbc_})->parse(nodes, source); });
xxx_parser_impl(class_head_, { return seq_({class_key_, zom_(attribute_specifier_), or_({opt_(base_clause_), seq_({class_head_name_, opt_(class_virt_specifier_), opt_(base_clause_)})})})->parse(nodes, source); });
xxx_parser_impl(class_head_name_, { return seq_({opt_(nested_name_specifier_), class_name_})->parse(nodes, source); });
xxx_parser_impl(class_virt_specifier_, { return lit::final_->parse(nodes, source); });
xxx_parser_impl(class_key_, { return kw_set_({lex::def::class_s_, lex::def::struct_s_, lex::def::union_s_})->parse(nodes, source); });
xxx_parser_impl(member_specification_, { return seq_({or_({seq_({access_specifier_, lit::semi_}), member_declaration_}), opt_(member_specification_)})->parse(nodes, source); });
xxx_parser_impl(member_declaration_, {
	return or_({
				   alias_declaration_,
				   opaque_enum_declaration_,
				   empty_declaration_,
				   function_definition_,
				   using_declaration_,
				   using_enum_declaration_,
				   static_assert_declaration_,
				   template_declaration_,
				   explicit_specialization_,
				   deduction_guide_,
				   seq_({zom_(attribute_specifier_), zom_(decl_specifier_), opt_(member_declarator_list_), lit::semi_}),
			   })
		->parse(nodes, source);
});
xxx_parser_impl(member_declarator_list_, { return seq_({member_declarator_, zom_(seq_({lit::comma_, member_declarator_}))})->parse(nodes, source); });
xxx_parser_impl(member_declarator_, {
	return or_({seq_({declarator_, or_({requires_clause_, seq_({zom_(virt_specifier_), opt_(pure_specifier_)}), opt_(brace_or_equal_initializer_)})}),
				seq_({opt_(identifier_), zom_(attribute_specifier_), lit::col_, constant_expression_, opt_(brace_or_equal_initializer_)})})
		->parse(nodes, source);
});
xxx_parser_impl(virt_specifier_, { return kw_set_({lex::def::override_s_, lex::def::final_s_})->parse(nodes, source); });
xxx_parser_impl(pure_specifier_, { return seq_({lit::eq_, lit::zero_})->parse(nodes, source); });
xxx_parser_impl(conversion_function_id_, { return seq_({lit::operator_, conversion_type_id_})->parse(nodes, source); });
xxx_parser_impl(conversion_type_id_, { return seq_({zom_(type_specifier_), opt_(conversion_declarator_)})->parse(nodes, source); });
xxx_parser_impl(conversion_declarator_, { return seq_({ptr_operator_, opt_(conversion_declarator_)})->parse(nodes, source); });
xxx_parser_impl(base_clause_, { return seq_({lit::col_, base_specifier_list_})->parse(nodes, source); });
xxx_parser_impl(base_specifier_list_, { return seq_({base_specifier_, opt_(lit::ellipsis_), zom_(seq_({lit::comma_, base_specifier_, opt_(lit::ellipsis_)}))})->parse(nodes, source); });
xxx_parser_impl(base_specifier_, { return seq_({zom_(attribute_specifier_), opt_(or_({seq_({lit::virtual_, opt_(access_specifier_)}), seq_({access_specifier_, opt_(lit::virtual_)})})), class_or_decltype_})->parse(nodes, source); });
xxx_parser_impl(class_or_decltype_, { return or_({seq_({nested_name_specifier_, lit::template_, simple_template_id_}), seq_({opt_(nested_name_specifier_), type_name_}), decltype_specifier_})->parse(nodes, source); });
xxx_parser_impl(access_specifier_, { return kw_set_({lex::def::private_s_, lex::def::protected_s_, lex::def::public_s_})->parse(nodes, source); });
xxx_parser_impl(ctor_initializer_, { return seq_({lit::col_, mem_initializer_list_})->parse(nodes, source); });
xxx_parser_impl(mem_initializer_list_, { return seq_({mem_initializer_, opt_(lit::ellipsis_), zom_(seq_({lit::comma_, mem_initializer_, opt_(lit::ellipsis_)}))})->parse(nodes, source); });
xxx_parser_impl(mem_initializer_, { return seq_({mem_initializer_id_, or_({seq_({lit::lp_, opt_(expression_list_), lit::rp_}), braced_init_list_})})->parse(nodes, source); });
xxx_parser_impl(mem_initializer_id_, { return or_({class_or_decltype_, identifier_})->parse(nodes, source); });

//	A.10 Overloading

xxx_parser_impl(operator_function_id_, { return seq_({lit::operator_, operator_})->parse(nodes, source); });
xxx_parser_impl(operator_, {
	return or_({seq_({lit::new_, opt_(seq_({lit::lbk_, lit::rbk_}))}),
				seq_({lit::delete_, opt_(seq_({lit::lbk_, lit::rbk_}))}),
				seq_({opt_(seq_({lit::lbk_, lit::rbk_}))}),
				seq_({opt_(seq_({lit::lp_, lit::rp_}))}),
				lit::co_await_,
				op_set_({"->*", "->", "+=", "-=", "*=", "/=", "%=", "^=", "&=", "|=", "==", "!=", "<=>", "<=", ">=", "++", "--", "&&", "||", "<<", ">>", "<<=", ">>=", "<", ">", ",", "|", "=", "~", "!", "+", "-", "*", "/", "%", "^", "&"})})
		->parse(nodes, source);
});
xxx_parser_impl(literal_operator_id_, { return seq_({lit::operator_, or_({seq_({string_literal_, identifier_}), user_defined_string_literal_})})->parse(nodes, source); });

//	A.11 Templates

xxx_parser_impl(template_declaration_, { return seq_({template_head_, or_({concept_definition_, declaration_})})->parse(nodes, source); });
xxx_parser_impl(template_head_, { return seq_({lit::template_, lit::lt_, template_parameter_list_, lit::gt_, opt_(requires_clause_)})->parse(nodes, source); });
xxx_parser_impl(template_parameter_list_, { return seq_({template_parameter_, zom_(seq_({lit::comma_, template_parameter_}))})->parse(nodes, source); });
xxx_parser_impl(requires_clause_, { return seq_({lit::requires_, constraint_logical_or_expression_})->parse(nodes, source); });
xxx_parser_impl(constraint_logical_or_expression_, { return seq_({constraint_logical_and_expression_, oom_(seq_({lit::vl2_, constraint_logical_and_expression_}))})->parse(nodes, source); });
xxx_parser_impl(constraint_logical_and_expression_, { return seq_({primary_expression_, oom_(seq_({lit::amp2_, primary_expression_}))})->parse(nodes, source); });
xxx_parser_impl(template_parameter_, { return or_({type_parameter_, parameter_declaration_})->parse(nodes, source); });
xxx_parser_impl(type_parameter_, {
	return or_({seq_({or_({type_constraint_, type_parameter_key_}), or_({seq_({opt_(identifier_), lit::eq_, type_id_}), seq_({opt_(lit::ellipsis_), opt_(identifier_)})})}),
				seq_({template_head_, type_parameter_key_, or_({seq_({opt_(identifier_), lit::eq_, id_expression_}), seq_({opt_(lit::ellipsis_), opt_(identifier_)})})})})
		->parse(nodes, source);
});
xxx_parser_impl(type_parameter_key_, { return or_({lit::class_, lit::typename_})->parse(nodes, source); });
xxx_parser_impl(type_constraint_, { return seq_({opt_(nested_name_specifier_), concept_name_, opt_(seq_({lit::lt_, opt_(template_argument_list_), lit::gt_}))})->parse(nodes, source); });
xxx_parser_impl(simple_template_id_, { return seq_({template_name_, lit::lt_, opt_(template_argument_list_), lit::gt_})->parse(nodes, source); });
xxx_parser_impl(template_id_, { return or_({seq_({or_({literal_operator_id_, operator_function_id_}), lit::lt_, opt_(template_argument_list_), lit::gt_}), simple_template_id_})->parse(nodes, source); });
xxx_parser_impl(template_argument_list_, { return seq_({seq_({template_argument_, opt_(lit::ellipsis_)}), zom_(seq_({lit::comma_, template_argument_, opt_(lit::ellipsis_)}))})->parse(nodes, source); });
xxx_parser_impl(template_argument_, { return or_({constant_expression_, type_id_, id_expression_})->parse(nodes, source); });
xxx_parser_impl(constraint_expression_, { return logical_or_expression_->parse(nodes, source); });
xxx_parser_impl(deduction_guide_, { return seq_({opt_(explicit_specifier_), template_name_, lit::lp_, parameter_declaration_clause_, lit::rp_, lit::arrow_, simple_template_id_, lit::semi_})->parse(nodes, source); });
xxx_parser_impl(concept_definition_, { return seq_({lit::concept_, concept_name_, zom_(attribute_specifier_), lit::eq_, constraint_expression_, lit::semi_})->parse(nodes, source); });
xxx_parser_impl(concept_name_, { return identifier_->parse(nodes, source); });
xxx_parser_impl(typename_specifier_, { return seq_({lit::typename_, nested_name_specifier_, or_({seq_({opt_(lit::template_), simple_template_id_}), identifier_})})->parse(nodes, source); });
xxx_parser_impl(explicit_instantiation_, { return seq_({opt_(lit::extern_), lit::template_, declaration_})->parse(nodes, source); });
xxx_parser_impl(explicit_specialization_, { return seq_({lit::template_, lit::lt_, lit::gt_, declaration_})->parse(nodes, source); });

//	A.12 Exception handling

xxx_parser_impl(try_block_, { return seq_({lit::try_, compound_statement_, oom_(handler_)})->parse(nodes, source); });
xxx_parser_impl(function_try_block_, { return seq_({lit::try_, opt_(ctor_initializer_), compound_statement_, oom_(handler_)})->parse(nodes, source); });
xxx_parser_impl(handler_, { return seq_({lit::catch_, lit::lp_, exception_declaration_, lit::rp_, compound_statement_})->parse(nodes, source); });
xxx_parser_impl(exception_declaration_, { return or_({lit::ellipsis_, seq_({zom_(attribute_specifier_), oom_(type_specifier_), or_({declarator_, opt_(abstract_declarator_)})})})->parse(nodes, source); });
xxx_parser_impl(noexcept_specifier_, { return seq_({lit::noexcept_, opt_(seq_({lit::lp_, constant_expression_, lit::rp_}))})->parse(nodes, source); });

}	 // namespace stx

std::shared_ptr<ast::node_t> parse(pp::tokens_t const& tokens) {
	if (auto const [nodes, rest] = stx::translation_unit_->parse(stx::parser_t::nodes_t{}, tokens); ! rest) {
		return nullptr;
	} else if (! rest->empty()) {
		return nullptr;
	} else if (nodes.size() != 1u) {
		return nullptr;
	} else {
		return nodes.at(0);
	}
}

}	 // namespace cxx
namespace conf {

using config_t = std::unordered_map<std::string, std::vector<std::string>>;

template<typename T = std::string>
inline std::vector<T> get(config_t const& config, std::string const& key, std::vector<T> const& def) {
	if (auto const itr = config.find(key); itr != config.end()) {
		std::vector<T> result;
		result.reserve(itr->second.size());

		// -------------------------------
		// Converts returing types if necessary.
		if constexpr (std::is_same_v<T, std::string>) {
			return itr->second;
		} else if constexpr (std::is_same_v<T, bool>) {
			std::ranges::transform(itr->second, std::back_inserter(result), [](auto const& a) { return ! a.empty() && a != "0" && a != "false" && a != "no"; });
		} else {
			std::ranges::transform(itr->second, std::back_inserter(result), [](auto const& a) { T t{}; std::istringstream iss{a}; iss >> t; return t; });
		}
		return result;
	}
	return def;
}

}	 // namespace conf
namespace tu {

class context_t {};

class translation_unit_t {
public:
	auto const& name() const noexcept { return paths_.path(); }
	auto const& tokens() const noexcept { return paths_.tokens(); }
	auto const& pp_tokens() const noexcept { return paths_.preprocessing_tokens(); }

	std::shared_ptr<cxx::ast::node_t> compile() {
		log::tracer_t tr{{paths_.path().string()}};
		if (! ! paths_.nodes()) return paths_.nodes();

		// -------------------------------
		// Proceeds preprocessing.

		// To keep address of lexical tokens and preprocessing tokens, it uses shared pointers in the path manager;
		// otherwise, const iterators would be invalidate.
		paths_.source(xxx::load_file(paths_.path()));
		paths_.tokens(xxx::lex::scan(paths_.source(), paths_.path()));
		std::ranges::for_each(paths_.tokens(), [](auto const& a) { std::clog << " ---> " << xxx::lex::to_string(a) << "\n"; });	   // TODO:
		paths_.preprocessing_tokens(xxx::pp::preprocess(conditions_, macros_, paths_, paths_.tokens(), paths_.path()));
		std::ranges::for_each(paths_.preprocessing_tokens(), [](auto const& a) { std::for_each(a.first, a.second, [](auto const& aa) { std::clog << " $---> " << xxx::lex::to_string(aa) << "\n"; }); });
		paths_.node(cxx::parse(paths_.tokens()));

		return paths_.nodes();	  // TODO:
	}
	explicit translation_unit_t(std::filesystem::path const& path, conf::config_t const& config) :
		contexts_{}, symbols_{}, paths_{conf::get<std::filesystem::path>(config, "I", {}), conf::get<std::filesystem::path>(config, "L", {}), conf::get(config, "l", {})}, macros_{}, conditions_{} {
		paths_.push(path);
	}

private:
	std::stack<context_t> contexts_;	///<    @brief  Context.

	pp::sm::symbol_manager_t	symbols_;		///<    @brief  Symbol manager.
	pp::pm::path_manager_t		paths_;			///<    @brief  Path manager.
	pp::mm::macro_manager_t		macros_;		///<    @brief  Macro manager.
	pp::cm::condition_manager_t conditions_;	///<    @brief  Condition manager.
};

}	 // namespace tu
}	 // namespace xxx
namespace msg {

static char const Unexpected[]{"Unexpected"};
static char const Title[]{
	"====================================\n"
	" xcp - xxx c++ compiler\n"
	"  v.0.2.0.0 - (c) 2023-, Mura.\n"
	"===================================="};
static char const Usage[]{
	"  $ ./xcp.exe  {options}  sources\n"
	" [options]\n"
	"  -h                   : Shows this message\n"
	"  -v                   : Shows version\n"
	"  -D:{macro}(={value}) : Defines macro definition (default value is 1)\n"
	"  -I:{path}            : Adds system including path to find headers\n"
	"  -L:{path}            : Adds linker path to find libraries\n"
	"  -l:{name}            : Links the library\n"};

}	 // namespace msg
namespace util {

template<typename F>
class stopwatch_t {
public:
	explicit stopwatch_t(F const& f) :
		f_{f}, now_{std::chrono::steady_clock::now()} {}

	~stopwatch_t() {
		auto const elapse = std::chrono::duration_cast<std::chrono::microseconds>(std::chrono::steady_clock::now() - now_).count();
		if constexpr (noexcept(f_(elapse))) {
			f_(elapse);
		} else {
			try {
				f_(elapse);
			} catch (...) {}
		}
	}

private:
	F const& f_;

	std::chrono::steady_clock::time_point now_;
};

}	 // namespace util

int main(int ac, char* av[]) {
	// -------------------------------
	// Initialzies primitive language features.
	std::locale::global(std::locale{"C"});
	std::ios::sync_with_stdio(false);
	std::clog << msg::Title << std::endl;
	try {
		// -------------------------------
		// Gets the arguments as options and sources.
		std::vector<std::string_view> const args{av + 1, av + ac};

		auto options{args | std::views::filter([](auto const& a) { return a != "-" && a.starts_with("-"); }) | std::views::common};
		auto sources{args | std::views::filter([](auto const& a) { return ! a.starts_with("-"); }) | std::views::common};

		if (xxx::contains(options, "-V")) {
			xxx::log::level_s = xxx::log::level_t::Verbose;
		}
		xxx::log::tracer_t tr{args};
		{
			// -------------------------------
			// Checks arguments.
			if (xxx::contains(options, "-h") || xxx::contains(options, "-v")) {
				std::cerr << msg::Usage << std::endl;
				return 1;
			}
			if (xxx::contains(options, "-")) {
				std::cerr << "Standard input is not supported." << std::endl;
				return -1;
			}
			if (std::ranges::empty(sources)) {
				std::cerr << "No sources" << std::endl;
				return -1;
			}
			auto missing = sources | std::views::filter([](auto const& a) { return ! std::filesystem::exists(a); }) | std::views::common;
			if (! std::ranges::empty(missing)) {
				std::cerr << "No such sources: \n";
				std::ranges::for_each(missing, [](auto const& a) { std::cerr << " - " << a << "\n"; });
				std::cerr << std::endl;
				return -1;
			}
		}

		// -------------------------------
		// Configures.
		xxx::conf::config_t configurations;
		std::ranges::for_each(options, [&configurations](auto const& a) {
			std::regex const re{R"(^-([A-Za-z])(?:[=:](.*))$)"};
			if (std::match_results<std::string_view::const_iterator> m; std::regex_match(std::ranges::begin(a), std::ranges::end(a), m, re)) { configurations[m.str(1)].push_back(1 < m.size() ? m.str(2) : ""); }
		});

		// -------------------------------
		// Compiles the sources.
		auto translation_units = sources | std::views::transform([&configurations](auto const& a) { return std::make_shared<xxx::tu::translation_unit_t>(a, configurations); }) | std::views::common;
		std::ranges::for_each(translation_units, [](auto /*copy of shared pointer*/ tu) {
			util::stopwatch_t sw{[name = tu->name()](auto const& a) { std::clog << "TODO:" << name << ": " << static_cast<float>(a) / 1000.0f / 1000.0f << "sec." << std::endl; }};	   // TODO:

			auto const result = tu->compile();

			std::clog << "TODO: source: " << tu->name() << std::endl;	 // TODO:
		});
		return 0;
		// -------------------------------
		// Catches exceptions.
	} catch (std::exception const& e) {
		std::cerr << msg::Unexpected << ":" << e.what() << std::endl;
	} catch (...) {
		std::cerr << msg::Unexpected << std::endl;
	}
	return -1;
}
