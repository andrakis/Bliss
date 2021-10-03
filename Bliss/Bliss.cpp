// Bliss.cpp : Defines the entry point for the application.
//

#include <iostream>
#include <fstream> // Bliss::StandardaLibrary::File
#include <ctype.h> // { isspace, isdigit } Bliss::Parser::detail
#include "Bliss.h"

// Shared
namespace Bliss {
	BVarAtomLibrary AtomLibrary;
	BVar True(0), False(0), Nil(0); // Filled in later
	// Builtin keywords
	BAtomType builtin_quote, builtin_if, builtin_define, builtin_set, builtin_lambda, builtin_begin;
	namespace detail {
		BAtomType atom_dict;
	}

	// later
	bool Init() {
		True = BVar::Atom("true");
		False = BVar::Atom("false");
		Nil = BVar::Atom("nil");
		builtin_quote = AtomLibrary.declare("quote");
		builtin_if = AtomLibrary.declare("if");
		builtin_define = AtomLibrary.declare("define");
		builtin_set = AtomLibrary.declare("set!");
		builtin_lambda = AtomLibrary.declare("lambda");
		builtin_begin = AtomLibrary.declare("begin");
		detail::atom_dict = AtomLibrary.declare("dict");
		detail::non_custom_type = AtomLibrary.declare("non_custom_type");
		return false;
	}
}

// BVar
namespace Bliss {
	using namespace Containers;
	bool Debug = false;

	// Environment
	bool BEnvironment::TryFind(BAtomType name, /* out */ BVar &result) {
		auto it = map.find(name);
		if (it != map.end()) {
			result.assign(it->second);
			return true;
		}
		if (parent)
			return parent->TryFind(name, result);
		return false;
	} 
	bool BEnvironment::TryInsert(BAtomType name, const BVar &value) {
		auto result = map.emplace(name, value);
		return result.second;
	}
	bool BEnvironment::TrySet(BAtomType name, const BVar &value) {
		auto it = map.find(name);
		if (it != map.end()) {
			it->second.assign(value);
			return true;
		}
		if (parent)
			return parent->TrySet(name, value);
		return false;
	}

	// BVar
	BVar::BVar(BIntType value) : container(new BVarIntContainer(value)) { }
	BVar::BVar(std::string value) : container(new BVarStringContainer(value)) { }
	BVar::BVar(const BListType &list) : container(new BVarListContainer(list)) { }
	BVar::BVar(ProcType proc) : container(new BVarProcContainer(proc)) { }
	BVar::BVar(ProcWithEnvironmentType proc) : container(new BVarProcWithEnvironmentContainer(proc)) { }
	BVar::BVar(BExceptionType e) : container(new BVarExceptionContainer(e)) { }
	BVar::BVar(BVarContainer *cnt) : container(cnt) { }
	BVar::BVar(BVarContainer const &cnt) : container(cnt.duplicate()) { }
	BVar::BVar(BVar const &other) : container(other.container->duplicate()) { }
	BVar BVar::Atom(std::string name) { return BVar(new BVarAtomContainer(name)); }
	BVar BVar::Atom(BAtomType id) { return BVar(new BVarAtomContainer(id)); }
	BVar BVar::List() { return BVar(new BVarListContainer()); }
	BVar BVar::List(const BListType &l) { return BVar(new BVarListContainer(l)); }
	BVar BVar::Exception(BExceptionType ex) { return BVar(new BVarExceptionContainer(ex)); }
	BVar BVar::Exception(const std::string &msg) { return BVar(new BVarExceptionContainer(BRuntimeException(msg))); }
	BVar BVar::MakeBEnvPtr(BEnvPtr ptr) { return BVar(new BVarEnvironmentContainer(ptr)); }
	BVar::~BVar() { }
	BVarType BVar::Type() const { return container->type; }
	BCustomType BVar::CustomType() const { return container->CustomType(); }
	BAtomType BVar::AtomValue() const { return container->AtomValue(); }
	BIntType BVar::IntValue() const { return container->IntValue(); }
	std::string BVar::StringValue() const { return container->StringValue(); }
	std::string BVar::StringRepr() const { return container->StringRepr(); }
	bool BVar::CompEq (const BVar &other) const { return container->CompEq(*other.container); }
	const BVar& BVar::Head() const { return container->Head(); }
	BVar BVar::Tail() const { return container->Tail(); }
	const BVar& BVar::Index(size_t Index) const { return container->Index(Index); }
	BListType::iterator BVar::Begin() { return container->Begin(); }
	BListType::iterator BVar::End() { return container->End(); }
	BListType::const_iterator BVar::CBegin() const { return container->CBegin(); }
	BListType::const_iterator BVar::CEnd() const { return container->CEnd(); }
	size_t BVar::Length() const { return container->Length(); }
	BEnvPtr BVar::EnvPtr() { return container->EnvPtr(); }
	ProcType BVar::ProcValue() const { return container->ProcValue(); }
	ProcWithEnvironmentType BVar::ProcWithEnvironmentValue() const { return container->ProcWithEnvironmentValue(); }
	BExceptionType BVar::Exception() { return container->Exception(); }
	BExceptionType BVar::Exception() const { return container->Exception(); }
	void BVar::assign(BVarContainer* cnt) {
		container = std::unique_ptr<BVarContainer>(cnt->duplicate());
	}
	void BVar::assign(const BVar &other) {
		container = std::unique_ptr<BVarContainer>(other.container->duplicate());
	}
	BVar &BVar::operator+= (const BVar &rhs)& {
		container->add(*rhs.container);
		return *this;
	}
	BVar &BVar::operator-= (const BVar &rhs)& {
		container->subtract(*rhs.container);
		return *this;
	} 
	BVar &BVar::operator*= (const BVar &rhs)& {
		container->multiply(*rhs.container);
		return *this;
	}
	BVar &BVar::operator/= (const BVar &rhs)& {
		container->divide(*rhs.container);
		return *this;
	} 
	bool BVar::operator== (const BVar &rhs) const { return container->CompEq(*rhs.container); }
	bool BVar::operator!= (const BVar &rhs) const { return !(operator==(rhs)); }
	std::ostream &operator<<(std::ostream &os, const BVar &v)
	{
		os << v.StringValue();
		return os;
	}
	BVar &BVar::operator=(const BVar &&other) noexcept {
		//BVar tmp(other);
		//std::swap(container, tmp.container);
		assign(other);
		return *this;
	}
	BVar &BVar::operator=(BVar &other) noexcept {
		std::swap(container, other.container);
		//container = std::move(other.container);
		return *this;
	}

	void BVar::swap(BVar &other) noexcept {
		std::swap(container, other.container);
	}

	namespace detail {
		BListType emptyList;
		BRuntimeException non_exception("Non-exception");
		BCustomType non_custom_type;
		const char   indent_char = '-';
		std::string  debug_endstr = "\n"; // std::endl;
		DepthType EvalDepth = 0;
		const DepthType max_depth = 20;
		template<typename T>
		std::string PadNumber(T number, size_t padding) {
			std::string str = std::to_string(number);
			if (str.length() < padding)
				str = std::string(padding - str.length(), '0') + str;
			return str;
		}
		std::string DepthStr() {
			size_t depth = 1;
			if (EvalDepth > max_depth)
				return std::string(depth, indent_char) + PadNumber(EvalDepth, 3) + "> ";
			if (EvalDepth > 0)
				depth = (size_t)EvalDepth;
			return std::string(depth, indent_char) + "> ";
		}
	}

	// Parser
	namespace Parser {
		namespace detail {
			std::string::value_type startOf(const std::string &str) {
				if (str.empty()) return '\0';
				return *(str.cbegin());
			}
			std::string::value_type endOf(const std::string &str) {
				if (str.empty()) return '\0';
				return *(str.crbegin());
			}
			TokenListType tokenise(const TokenType &str) {
				TokenListType tokens;
				const char *s = str.c_str();
				while (*s) {
					// Skip whitespace
					while (isspace(*s))
						++s;
					// Skip comment lines
					if (*s == ';' && *(s + 1) == ';')
						while (*s && *s != '\n' && *s != '\r')
							++s;
					// List open or close
					else if (*s == '(' || *s == ')')
						tokens.push_back(*s++ == '(' ? "(" : ")");
					// "String" in quotes
					else if (*s == '"') {
						const char *t = s;
						int escape = 0;
						do {
							++t;
							if (escape != 0) escape--;
							if (*t == '\\')
								escape = 2; // skip this and the next character
						} while (*t && (escape != 0 || *t != '"'));
						++t;
						tokens.push_back(StringType(s, t));
						s = t;
					// A generic token
					} else {
						const char *t = s;
						while (*t && !isspace(*t) && *t != '(' && *t != ')')
							++t;
						tokens.push_back(StringType(s, t));
						s = t;
					}
				}
				return tokens;
			}
			BVar atom(const TokenType &token) {
				if(startOf(token) == '"' && endOf(token) == '"')
					return BVar(std::string(token.cbegin() + 1, token.cend() - 1));
				else if(isdigit(token[0]) || (token[0] == '-' && isdigit(token[1]))) {
					// Number
					// TODO: support more than just int
					BIntType number;
					if (util::TryStringToNumber(token, number)) {
						return BVar(number);
					}
				}
				return BVar::Atom(token);
			}
			BVar read_from(TokenListType &tokens) {
				if (tokens.empty()) throw ParserException("Missing opening token");

				const TokenType token(tokens.front());
				tokens.pop_front();
				if (token == "(") {
					BListType cells;
					while (tokens.front() != ")") {
						cells.push_back(read_from(tokens));
						if (tokens.empty()) throw ParserException("Missing closing )");
					}
					if (tokens.empty()) throw ParserException("Missing closing )");
					tokens.pop_front();
					return BVar(cells);
				} else if(token == "[") {
					// [: auto list open
					// [1 2 3] => (list 1 2 3)
					BListType cells({BVar::Atom("list")});
					while (tokens.front() != "]") {
						cells.push_back(read_from(tokens));
						if (tokens.empty()) throw ParserException("Missing closing ]");
					}
					if (tokens.empty()) throw ParserException("Missing closing ]");
					tokens.pop_front();
					return BVar(cells);
				} else if(startOf(token) == '\'') {
					/*BListType cell({BVar::Atom("quote")});
					if (token == "'")
						tokens.push_front(token.substr(1));
					cell.push_back(read_from(tokens));
					return BVar(cell);*/
					tokens.push_front(token.substr(1));
					BListType cell({BVar::Atom("quote"), read_from(tokens)});
					return BVar(cell);
				} else {
					return atom(token);
				}
			}
			BVar read(const StringType &str) {
				TokenListType tokens(tokenise(str));
				return read_from(tokens);
			}
		}

		BVar Read(const detail::StringType &str) {
			return detail::read(str);
		}
	}

	// Eval
	BVar Eval(BVar x, BVar env) {
		using namespace detail;

		for( ;; ) {
			EvalDepth++;
			const BVarType type = x.Type();
			if (Debug) std::cerr << DepthStr() << "Eval(" << x << ")" << debug_endstr;
			if (type == BVarType::Atom)
			{
				if (env.EnvPtr()->TryFind(x.AtomValue(), x)) {
					if (Debug) std::cerr << DepthStr() << "Eval(...) => " << x << debug_endstr;
					EvalDepth--;
					return x;
				}
				throw BRuntimeException(std::string("Key not found: ") + x.StringValue());
			} else if(type != BVarType::List) {
				if (Debug) std::cerr << DepthStr() << "Eval(" << x << ") => " << x << debug_endstr;
				EvalDepth--;
				return x;
			}
			if (x.Length() == 0) {
				if (Debug) std::cerr << DepthStr() << "Eval(" << x << ") => Nil" << debug_endstr;
				EvalDepth--;
				return Nil;
			}
			// List, invoke builtin or function call
			const BVar &first = x.Index(0);
			if (first.Type() == BVarType::Atom) {
				const BAtomType first_atom = first.AtomValue();
				if (first_atom == builtin_quote) {
					return x.Index(1);
				} else if (first_atom == builtin_if) {
					// TODO: length checks
					const BVar &cond = Eval(x.Index(1), env);
					const BVar &condeq = x.Index(2);
					const BVar &alt = (x.Length() > 3 ? x.Index(3) : Nil);
					x.assign((cond == False || cond == Nil) ? alt : condeq);
					// Tail recurse
					if (Debug) std::cerr << DepthStr() << "Eval(" << x << ") => " << x << debug_endstr;
					EvalDepth--;
					continue;
				} else if (first_atom == builtin_define) {
					const BVar &name = x.Index(1);
					const BVar &value = Eval(x.Index(2), env);
					if (!env.EnvPtr()->TryInsert(name.AtomValue(), value)) {
						throw BRuntimeException(std::string("Key already defined: ") + name.StringValue());
					}
					if (Debug) std::cerr << DepthStr() << "Eval(" << x << ") => " << value << debug_endstr;
					EvalDepth--;
					return value;
				} else if (first_atom == builtin_set) {
					const BVar &name = x.Index(1);
					const BVar &value = Eval(x.Index(2), env);
					if (!env.EnvPtr()->TrySet(name.AtomValue(), value)) {
						throw BRuntimeException(std::string("Key not defined: ") + name.StringValue());
					}
					if (Debug) std::cerr << DepthStr() << "Eval(" << x << ") => " << value << debug_endstr;
					EvalDepth--;
					return value;
				} else if (first_atom == builtin_lambda) {
					const BVar &result = BVar(new BVarLambdaContainer(x.Index(1), x.Index(2), env.EnvPtr()));
					if (Debug) std::cerr << DepthStr() << "Eval(" << x << ") => " << result << debug_endstr;
					EvalDepth--;
					return result;
				} else if (first_atom == builtin_begin) {
					auto it = x.CBegin() + 1;
					auto end = x.CEnd() - 1;
					for( ; it != end; ++it)
						Eval(*it, env);
					// Tail recurse
					x.assign(*it);
					if (Debug) std::cerr << DepthStr() << "Eval(" << x << ") => " << x << debug_endstr;
					EvalDepth--;
					continue;
				}
			}
			// Function call.
			BListType args;
			BVar fun = Eval(first, env);
			for (auto it = x.CBegin() + 1; it != x.CEnd(); ++it)
				args.push_back(Eval(*it, env));
			switch(fun.Type()) {
			case BVarType::Lambda: {
				// Create new environment
				BEnvPtr child_env = std::make_shared<BEnvironment>(fun.EnvPtr());
				BVar fun_args = fun.Index(0);
				if (fun_args.Type() == BVarType::Atom) {
					// Single argument, not a list. Assign all arguments to this name
					BListType tmp;
					tmp.push_back(BVar(args));
					args = BListType(tmp);
					BListType tmp2;
					tmp2.push_back(fun_args);
					fun_args = BVar(tmp2);
				}
				// Add arguments to environment
				for (auto it1 = args.cbegin(), it2 = fun_args.CBegin();
					it1 != args.cend() && it2 != fun_args.CEnd();
					++it1, ++it2) {
					child_env->TryInsert(it2->AtomValue(), *it1);
				}
				// Evaluate under lambda environment
				// Tail recurse
				x.assign(fun.Index(1));
				env = BVar::MakeBEnvPtr(child_env);
				if (Debug) std::cerr << DepthStr() << "Eval(" << x << ") => " << x << debug_endstr;
				EvalDepth--;
				continue;
			}

			case BVarType::Proc: {
				const BVar &result = fun.ProcValue()(args);
				if (Debug) std::cerr << DepthStr() << "Eval(" << x << ") => " << result << debug_endstr;
				EvalDepth--;
				return result;
			}
				
			case BVarType::ProcWithEnvironment: {
				const BVar &result = fun.ProcWithEnvironmentValue()(args, env.EnvPtr());
				if (Debug) std::cerr << DepthStr() << "Eval(" << x << ") => " << result << debug_endstr;
				EvalDepth--;
				return result;
			}
				
			default:
				std::cerr << "Invoke attempted on something not a function\n" <<
							 "  Type: " << (int)fun.Type() << std::endl <<
							 "  Repr: " << fun.StringRepr() << std::endl;
				throw BRuntimeException("Invalid type (A)");
			}
		}
	}

	BVar TryEval(const BVar &code, BVar env) {
		try {
			return Eval(code, env);
		} catch (BExceptionType &e) {
			return e;
		}
	}

	namespace StandardLibrary {
		BVar Print(const BListType &args) {
			// Map and print
			for (auto it = args.cbegin(); it != args.cend(); ++it) {
				if (it != args.cbegin())
					std::cout << " ";
				std::cout << it->StringValue();
			}
			std::cout << "\n";
			return Nil;
		}

		namespace detail {
			template<class Callback>
			BVar FoldLeft(const BListType &args, Callback callback) {
				if (args.empty()) return BVar(0);
				BVar value = args.front();
				for(auto it = args.cbegin() + 1; it != args.cend(); ++it)
					callback(value, *it);
				return value;
			}
		}

#define B_FOLDLEFT(List,CallbackAccumulatorName,CallbackXName,Code) detail::FoldLeft(List, [](BVar &CallbackAccumulatorName, const BVar &CallbackXName) Code)
		BVar Plus(const BListType &args) {
			return detail::FoldLeft(args, [](BVar &value, const BVar &x) { value += x; });
		}
		BVar Minus(const BListType &args) {
			return detail::FoldLeft(args, [](BVar &value, const BVar &x) { value -= x; });
		}
		BVar Multiply(const BListType &args) {
			return detail::FoldLeft(args, [](BVar &value, const BVar &x) { value *= x; });
		}
		BVar Divide(const BListType &args) {
			return detail::FoldLeft(args, [](BVar &value, const BVar &x) { value /= x; });
		}
		BVar Equals(const BListType &args) {
			// TODO: express across all vars
			return args.at(0) == args.at(1) ? True : False;
		}
		BVar NotEquals(const BListType &args) {
			return args.at(0) != args.at(1) ? True : False;
		}
		BVar List(const BListType &args) {
			return BVar::List(args);
		}
		
		// Dictionary functions
		namespace Dict {
			using namespace ::Bliss::detail;
			bool is_dict(const BVar &var) { return BVarDictionaryContainer::IsDict(var); }
			// (dict:new [DictToCopy]) => dict()
			BVar New(const BListType &args) {
				if (args.empty())
					return BVar(new BVarDictionaryContainer());
				// Assuming desire to copy existing dict
				return BVar(BVarDictionaryContainer::Copy(args.front()));
			}
			// (dict:key? Key Dict) => True | False
			BVar Key(const BListType &args) {
				const BVar &key = args.at(0);
				const BVar &dict = args.at(1);
				if (is_dict(dict)) {
					const BVarDictionaryContainer *p = dynamic_cast<const BVarDictionaryContainer*>(dict._const_container());
					if (p) {
						return p->GetDict()->find(key.AtomValue()) != p->GetDict()->end();
					}
				}
				return Nil;
			}
			// (dict:set! Key Value Dict) => Value
			BVar Set(const BListType &args) {
				const BVar &key = args.at(0);
				const BVar &value = args.at(1);
				BVar dict = args.at(2);
				if (is_dict(dict)) {
					BVarDictionaryContainer *p = dynamic_cast<BVarDictionaryContainer*>(dict._container());
					if (p) {
						auto it = p->GetDict()->find(key.AtomValue()); 
						if (it != p->GetDict()->end()) {
							it->second.assign(value);
						} else {
							p->GetDict()->emplace(key.AtomValue(), value);
						}
						return value;
					}
				}
				return Nil;
			}
			// (dict:get Key Dict) => Value
			BVar Get(const BListType &args) {
				const BAtomType key = args.at(0).AtomValue();
				const BVar &dict = args.at(1);
				if (is_dict(dict)) {
					const BVarDictionaryContainer *p = dynamic_cast<const BVarDictionaryContainer*>(dict._const_container());
					if (p) {
						auto it = p->GetDict()->find(key); 
						if (it != p->GetDict()->end()) {
							return it->second;
						}
					}
				}
				return Nil;
			}
		}

#define RUNTIME_CHECK(Args,Check) do { /* TODO */ } while(0)

		namespace File {
			namespace detail {
				const char *search_paths[] = {
					"",
					"./",
					"./lisp/",
					"./bliss_modules/",
					0
				};

				bool exists(const std::string &file) {
					std::ifstream fs(file);
					return fs.good();
				}

				std::string path(const std::string &spec) {
					for (auto it = &detail::search_paths[0]; *it != nullptr; ++it) {
						std::string path = *it;
						std::string full = path + spec;
						if (exists(full))
							return full;
					}
					return spec;
				}
			}

			// (file:exists File) -> bool()
			// No search paths used.
			BVar Exists(const BListType &args) {
				RUNTIME_CHECK(args, runtime_check::has_atleast_one_argument);
				return detail::exists(args[0].StringValue());
			}

			// (file:path File) -> Path
			// and Path is subpath of search_paths().
			BVar Path(const BListType &args) {
				RUNTIME_CHECK(args, runtime_check::has_atleast_one_argument);
				return detail::path(args[0].StringValue());
			}

			BVar Read(const BListType &args) {
				RUNTIME_CHECK(args, runtime_check::has_atleast_one_argument);
				const std::string spec = detail::path(args[0].StringValue());
				std::ifstream fs(spec);
				if (!fs.good())
					throw BRuntimeException("Failed to open file: " + spec);
				std::stringstream buffer;
				buffer << fs.rdbuf();
				return buffer.str();
			}
		}
	}

	void AddStandardLibrary(BEnvPtr env) {
		env->TryInsert(False.AtomValue(), False);
		env->TryInsert(True.AtomValue(), True);
		env->TryInsert(Nil.AtomValue(), Nil);
		env->TryInsert(AtomLibrary.declare("print"), BVar(StandardLibrary::Print));
		env->TryInsert(AtomLibrary.declare("+"), BVar(StandardLibrary::Plus));
		env->TryInsert(AtomLibrary.declare("-"), BVar(StandardLibrary::Minus));
		env->TryInsert(AtomLibrary.declare("*"), BVar(StandardLibrary::Multiply));
		env->TryInsert(AtomLibrary.declare("/"), BVar(StandardLibrary::Divide));
		env->TryInsert(AtomLibrary.declare("=="), BVar(StandardLibrary::Equals));
		env->TryInsert(AtomLibrary.declare("!="), BVar(StandardLibrary::NotEquals));
		env->TryInsert(AtomLibrary.declare("list"), BVar(StandardLibrary::List));
		env->TryInsert(AtomLibrary.declare("exec"), BVar(StandardLibrary::Exec));
		env->TryInsert(AtomLibrary.declare("dict"), BVar(StandardLibrary::Dict::New));
		env->TryInsert(AtomLibrary.declare("dict:set!"), BVar(StandardLibrary::Dict::Set));
		env->TryInsert(AtomLibrary.declare("dict:get"), BVar(StandardLibrary::Dict::Get));
		env->TryInsert(AtomLibrary.declare("file:exists"), BVar(StandardLibrary::File::Exists));
		env->TryInsert(AtomLibrary.declare("file:path"), BVar(StandardLibrary::File::Path));
		env->TryInsert(AtomLibrary.declare("file:read"), BVar(StandardLibrary::File::Read));
	}
}

using namespace Bliss;

enum TestFlags {
	Primitive = 1 << 1,
	Swapping = 1 << 2,
	EnvTest = 1 << 3,
	AddTest = 1 << 4,
	FacTest = 1 << 5,
	RecursiveTest = 1 << 6,
	ParserTest = 1 << 7,
	ExecTest = 1 << 8,
	DictTest = 1 << 9,
};

int main()
{
	int tf = /*FacTest;//*/Primitive | Swapping | EnvTest | AddTest | FacTest | RecursiveTest | ParserTest | DictTest;
	//int tf = RecursiveTest;//FacTest;
	//int tf = ExecTest;
	//int tf = DictTest;

	if (Bliss::Init()) {
		printf("Init failed?\n");
		return 1;
	}

	// if (!silent)
	std::cout << BANNER << std::endl;

	BEnvPtr env = std::make_shared<BEnvironment>();
	BVar _env = BVar::MakeBEnvPtr(BEnvPtr(env));
	AddStandardLibrary(env);

	BVar a(0), b("1"), c = BVar::Atom("test"), d = BVar::Atom("test");
	BVar l = BVar::List();
	if (tf & Primitive) {
		std::cout << "a: " << a << "\n"; a += 1;
		std::cout << "a: " << a << "\n"; a += 1;
		std::cout << "b: " << b << "\n"; b += 1;
		std::cout << "b: " << b << "\n"; b += 1;
		std::cout << "c: " << c << ", repr: " << c.StringRepr() << "\n";
		std::cout << "l: " << l << "\n";
	}

	if (tf & Swapping) {
		BVar tmp(a);
		a = b;
		b = c;
		c = tmp;
		l += a;
		l += b;
		l += c;
		
		std::cout << "a: " << a << "\n"; 
		std::cout << "b: " << b << "\n"; 
		std::cout << "c: " << c << ", repr: " << c.StringRepr() << "\n";
		std::cout << "l: " << l << "\n";
	}

	if (tf & EnvTest) {
		BVar x = BVar::Atom("x");
		env->TryInsert(AtomLibrary.declare("x"), BVar("test"));
		std::cout << "Code: " << x << "\n";
		std::cout << "Result: " << TryEval(x, _env) << "\n";
	}

	BVar code;

	if (tf & AddTest) {
		/* Code:
		 * (begin
		 *   (define add (lambda (x y) (+ x y)))
		 *   (print (add 1 2))
		 * )
		 */
		code = BListType({
			BVar::Atom("begin"),
			BListType({BVar::Atom("define"),
				BVar::Atom("add"),
				BListType({BVar::Atom("lambda"),
					BListType({BVar::Atom("x"), BVar::Atom("y")}),
					BListType({BVar::Atom("+"), BVar::Atom("x"), BVar::Atom("y")})
				})
			}),
			BListType({BVar::Atom("print"), BListType({BVar::Atom("add"), 1, 2})})
		});
		std::cout << "Code: " << code << "\n";
		std::cout << "Result: " << TryEval(code, _env) << "\n";
	}

	if (tf & FacTest) {
		/*
		 * Code:
		 * (begin
		 *    (define fac (lambda (n) (if (== n 1) 1 (* n (fac (- n 1))))))
		 *    (print (fac 4))
		 * )
		 */
		code = BListType({
			BVar::Atom("begin"),
			BListType({BVar::Atom("define"), BVar::Atom("fac"),
				BListType({BVar::Atom("lambda"),
					BListType({BVar::Atom("n")}),
					BListType({BVar::Atom("if"),
						BListType({BVar::Atom("=="), BVar::Atom("n"), BVar(1)}),
						BVar(1),
						BListType({BVar::Atom("*"),
							BVar::Atom("n"),
							BListType({BVar::Atom("fac"), BListType({BVar::Atom("-"), BVar::Atom("n"), BVar(1)})})
						})
					})
				})
			}),
			BListType({BVar::Atom("print"), BListType({BVar::Atom("fac"), BVar(4)})})
		});
		std::cout << "Code: " << code << "\n";
		std::cout << "Result: " << TryEval(code, _env) << "\n";
	}

	if (tf & RecursiveTest) {
		/*
		 * Code:
		 * (begin
		 *    (define fac_tr (lambda (n a) (if (== n 1) a (fac_tr (- n 1) (* n a)))))
		 *    (define fac (lambda (n) (fac_tr n 1)))
		 *    (print (fac 10))
		 * )
		 */
		code = BListType({
			BVar::Atom("begin"),
			BListType({BVar::Atom("define"), BVar::Atom("fac_tr2"),
				BListType({BVar::Atom("lambda"),
					BListType({BVar::Atom("n"), BVar::Atom("a")}),
					BListType({BVar::Atom("if"),
						BListType({BVar::Atom("=="), BVar::Atom("n"), BVar(1)}),
						BVar::Atom("a"),
						BListType({BVar::Atom("fac_tr2"),
							BListType({BVar::Atom("-"), BVar::Atom("n"), BVar(1)}),
							BListType({BVar::Atom("*"), BVar::Atom("n"), BVar::Atom("a")})
						})
					})
				})
			}),
			BListType({BVar::Atom("define"), BVar::Atom("fac_tr"),
				BListType({BVar::Atom("lambda"),
					BListType({BVar::Atom("n")}),
					BListType({BVar::Atom("fac_tr2"), BVar::Atom("n"), BVar(1)})
				})
			}),
			BListType({BVar::Atom("print"), BListType({BVar::Atom("fac_tr"), BVar(10)})})
		});
		std::cout << "Code: " << code << "\n";
		std::cout << "Result: " << TryEval(code, _env) << "\n";
	}

	if (tf & ParserTest) {
		code = Parser::Read("\
			(begin \
				(define sub (lambda (x y) (- x y))) \
				(print (sub 1 2)) \
			)");
		std::cout << "Code: " << code << "\n";
		std::cout << "Result: " << TryEval(code, _env) << "\n";
	}

	if (tf & ExecTest) {
		code = Parser::Read("(begin (define R (exec \"curl -svk http://192.168.83.1:8001/\")) (print \"CURL Response:\" R))");
		std::cout << "Code: " << code << "\n";
		std::cout << "Result: " << TryEval(code, _env) << "\n";
	}

	if (tf & DictTest) {
		code = Parser::Read("(begin (define D1 (dict)) (define D2 (dict)) (dict:set! 'a 1 D1) (dict:set! 'a 2 D2) (print \"Dict:\" D1 D2 \"IsEqual D1,D1:\" (== D1 D1) \"IsEqual D1,D2:\" (== D1 D2)))");
		std::cout << "Code: " << code << "\n";
		std::cout << "Result: " << TryEval(code, _env) << "\n";
	}

	// Cleanup
	env->Clear();

	return 0;
}
