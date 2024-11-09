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

// literal list
std::unordered_set<std::string_view> const alternative_expressions{"and", "and_eq", "bitand", "bitor", "compl", "not", "not_eq", "or", "or_eq", "xor", "xor_eq"};
std::unordered_set<std::string_view> const keywords{
	"alignas",
	"alignof",
	"asm",
	"auto",

	"bool",
	"break",

	"case",
	"catch",
	"char",
	"char16_t",
	"char32_t",
	"class",
	"const",
	"constexpr",
	"const_cast",
	"continue",

	"decltype",
	"default",
	"do",

	"delete",
	"double",
	"dynamic_cast",

	"else",
	"enum",
	"explicit",
	"export",
	"extern",

	"false",
	"float",
	"for",
	"friend",

	"goto",

	"if",
	"inline",
	"int",

	"long",

	"mutable",

	"namespace",
	"new",
	"noexcept",
	"nullptr",

	"operator",

	"private",
	"protected",
	"public",

	"return",
	"reinterpret_cast",
	"register",

	"short",
	"signed",
	"sizeof",
	"static",
	"static_assert",
	"static_cast",
	"struct",
	"switch",

	"template",
	"this",
	"thread_local",
	"throw",
	"typedef",
	"typeid",
	"typename",
	"try",
	"true",

	"union",
	"unsigned",
	"using",

	"virtual",
	"void",
	"volatile",

	"wchar_t",
	"while",
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
namespace pp {

using tokens_t		= std::list<lex::token_t>;
using tokens_itr_t	= tokens_t::iterator;
using tokens_citr_t = tokens_t::const_iterator;
using line_tokens_t = std::pair<tokens_citr_t, tokens_citr_t>;
using lines_t		= std::vector<line_tokens_t>;
using strs_t		= std::vector<std::string_view>;	// TODO:

class node_t {
public:
	auto token() const noexcept { return token_; }
	auto children() const noexcept { return children_; }

	void push(std::shared_ptr<node_t> node) { children_.push_back(node); }
	void push(tokens_citr_t begin, tokens_citr_t end) {
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
	void node(std::shared_ptr<node_t> nodes) const { current_.top()->node = std::move(nodes); }

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
		std::filesystem::path	path;
		std::string				source;
		pp::tokens_t			tokens;
		pp::lines_t				lines;
		std::shared_ptr<node_t> node;
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

std::shared_ptr<pp::node_t> parse(pp::lines_t const& preprocessing_tokens) {
	return nullptr;
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

	std::shared_ptr<pp::node_t> compile() {
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
		paths_.node(cxx::parse(paths_.preprocessing_tokens()));

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
