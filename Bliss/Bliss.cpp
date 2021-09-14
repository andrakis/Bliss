// Bliss.cpp : Defines the entry point for the application.
//

#include <iostream>
#include "Bliss.h"

// Shared
namespace Bliss {
	BVarAtomLibrary AtomLibrary;
	BVar True(0), False(0), Nil(0); // Filled in later
	// Builtin keywords
	BAtomType builtin_if, builtin_define, builtin_set, builtin_lambda, builtin_begin;

	// later
	bool Init() {
		True = BVar::Atom("true");
		False = BVar::Atom("false");
		Nil = BVar::Atom("nil");
		builtin_if = AtomLibrary.declare("if");
		builtin_define = AtomLibrary.declare("define");
		builtin_set = AtomLibrary.declare("set!");
		builtin_lambda = AtomLibrary.declare("lambda");
		builtin_begin = AtomLibrary.declare("begin");
		return false;
	}
}

// BVar
namespace Bliss {
	using namespace Containers;
	bool Debug = true;

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
	BVar::BVar(int value) : container(new BVarIntContainer(value)) { }
	BVar::BVar(std::string value) : container(new BVarStringContainer(value)) { }
	BVar::BVar(const BListType &list) : container(new BVarListContainer(list)) { }
	BVar::BVar(ProcType proc) : container(new BVarProcContainer(proc)) { }
	BVar::BVar(ProcWithEnvironmentType proc) : container(new BVarProcWithEnvironmentContainer(proc)) { }
	BVar::BVar(BVarContainer *cnt) : container(cnt) { }
	BVar::BVar(BVarContainer const &cnt) : container(cnt.duplicate()) { }
	BVar::BVar(BVar const &other) : container(other.container->duplicate()) { }
	BVar BVar::Atom(std::string name) { return BVar(new BVarAtomContainer(name)); }
	BVar BVar::List() { return BVar(new BVarListContainer()); }
	BVar BVar::List(const BListType &l) { return BVar(new BVarListContainer(l)); }
	BVar BVar::MakeBEnvPtr(BEnvPtr ptr) { return BVar(new BVarEnvironmentContainer(ptr)); }
	BVar::~BVar() { }
	BVarType BVar::Type() const { return container->type; }
	BAtomType BVar::AtomValue() const { return container->AtomValue(); }
	BIntType BVar::IntValue() const { return container->IntValue(); }
	std::string BVar::StringValue() const { return container->StringValue(); }
	std::string BVar::StringRepr() const { return container->StringRepr(); }
	bool BVar::CompEq (const BVar &other) const { return container->CompEq(*other.container); }
	BVar BVar::Head() const { return container->Head(); }
	BVar BVar::Tail() const { return container->Tail(); }
	BVar BVar::Index(size_t Index) const { return container->Index(Index); }
	BListType::iterator BVar::Begin() { return container->Begin(); }
	BListType::iterator BVar::End() { return container->End(); }
	BListType::const_iterator BVar::CBegin() const { return container->CBegin(); }
	BListType::const_iterator BVar::CEnd() const { return container->CEnd(); }
	size_t BVar::Length() const { return container->Length(); }
	BEnvPtr BVar::EnvPtr() { return container->EnvPtr(); }
	ProcType BVar::ProcValue() const { return container->ProcValue(); }
	ProcWithEnvironmentType BVar::ProcWithEnvironmentValue() const { return container->ProcWithEnvironmentValue(); }
	void BVar::assign(BVarContainer* cnt) {
		container = std::unique_ptr<BVarContainer>(cnt->duplicate());
	}
	void BVar::assign(const BVar &other) {
		container = std::unique_ptr<BVarContainer>(other.container->duplicate());
	}
	BVar &BVar::operator+= (const BVar &rhs)& throw(BInvalidOperationException) {
		container->add(*rhs.container);
		return *this;
	}
	BVar &BVar::operator-= (const BVar &rhs)& throw (BInvalidOperationException) {
		container->subtract(*rhs.container);
		return *this;
	} 
	BVar &BVar::operator*= (const BVar &rhs)& throw(BInvalidOperationException) {
		container->multiply(*rhs.container);
		return *this;
	}
	BVar &BVar::operator/= (const BVar &rhs)& throw (BInvalidOperationException) {
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
		BVar tmp(other);
		std::swap(container, tmp.container);
		return *this;
	}
	BVar &BVar::operator=(BVar &other) noexcept {
		std::swap(container, other.container);
		return *this;
	}

	namespace internal {
		BListType emptyList;
		const size_t max_depth = 20;
		const char   indent_char = '-';
		std::string  debug_endstr = "\n"; // std::endl;
		signed EvalDepth = 0;  // has a tendency to underflow
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

	// Eval
	BVar Eval(const BVar &x, BVar env) throw(BRuntimeException) {
		using namespace internal;
		EvalDepth++;
		const BVarType type = x.Type();
		if (Debug) std::cerr << DepthStr() << "Eval(" << x << ")" << debug_endstr;
		if (type == BVarType::Atom)
		{
			BEnvPtr in_env = env.EnvPtr();
			BVar out(0);
			if (in_env->TryFind(x.AtomValue(), out)) {
				if (Debug) std::cerr << DepthStr() << "Eval(" << x << ") => " << out << debug_endstr;
				EvalDepth--;
				return out;
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
		const BVar &first = x.Head();
		// Could be const if we could get iterators into BVar
		BVar rest = x.Tail();
		if (first.Type() == BVarType::Atom) {
			const BAtomType first_atom = first.AtomValue();
			if (first_atom == builtin_if) {
				// TODO: length checks
				const BVar &cond = Eval(x.Index(1), env);
				const BVar &condeq = x.Index(2);
				BVar alt = Nil;
				if (x.Length() > 3)
					alt = x.Index(3);
				const BVar result = Eval(cond == False ? condeq : alt, env);
				if (Debug) std::cerr << DepthStr() << "Eval(" << x << ") => " << result << debug_endstr;
				EvalDepth--;
				return result;
			} else if (first_atom == builtin_define) {
				BEnvPtr in_env = env.EnvPtr();
				const BVar &name = rest.Head();
				const BVar &value = Eval(rest.Tail().Head(), env);
				if (!in_env->TryInsert(name.AtomValue(), value)) {
					throw BRuntimeException(std::string("Key already defined: ") + name.StringValue());
				}
				if (Debug) std::cerr << DepthStr() << "Eval(" << x << ") => " << value << debug_endstr;
				EvalDepth--;
				return value;
			} else if (first_atom == builtin_set) {
				///
				BEnvPtr in_env = env.EnvPtr();
				const BVar &name = rest.Head();
				const BVar &value = Eval(rest.Tail().Head(), env);
				if (!in_env->TrySet(name.AtomValue(), value)) {
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
				auto it = rest.CBegin();
				auto end = rest.CEnd() - 1;
				for( ; it != end; ++it)
					Eval(*it, env);
				const BVar &result = Eval(*it, env);
				if (Debug) std::cerr << DepthStr() << "Eval(" << x << ") => " << result << debug_endstr;
				EvalDepth--;
				return result;
			}
		}
		// Function call.
		BListType args;
		BVar fun = Eval(first, env);
		auto it = rest.CBegin();
		auto end = rest.CEnd();
		for ( ; it != end; ++it)
			args.push_back(Eval(*it, env));
		const auto tfun = fun.Type();
		if (tfun == BVarType::Lambda) {
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
			const BVar result = Eval(fun.Index(1), BVar::MakeBEnvPtr(child_env));
			if (Debug) std::cerr << DepthStr() << "Eval(" << x << ") => " << result << debug_endstr;
			EvalDepth--;
			return result;
		} else if (tfun == BVarType::Proc) {
			const auto result = fun.ProcValue()(args);
			if (Debug) std::cerr << DepthStr() << "Eval(" << x << ") => " << result << debug_endstr;
			EvalDepth--;
			return result;
		} else if (tfun == BVarType::ProcWithEnvironment) {
			const auto result = fun.ProcWithEnvironmentValue()(args, env.EnvPtr());
			if (Debug) std::cerr << DepthStr() << "Eval(" << x << ") => " << result << debug_endstr;
			EvalDepth--;
			return result;
		} else {
			std::cerr << "Invoke attempted on something not a function\n" <<
						 "  Type: " << (int)fun.Type() << std::endl <<
						 "  Repr: " << fun.StringRepr() << std::endl;
			throw BRuntimeException("Invalid type (A)");
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

		namespace internal {
			template<class Callback>
			BVar FoldLeft(const BListType &args, Callback callback) {
				if (args.empty()) return BVar(0);
				BVar value = args.front();
				for(auto it = args.cbegin() + 1; it != args.cend(); ++it)
					callback(value, *it);
				return value;
			}
		}

		BVar Plus(const BListType &args) {
			return internal::FoldLeft(args, [](BVar &value, const BVar &x) { value += x; });
		}
		BVar Minus(const BListType &args) {
			return internal::FoldLeft(args, [](BVar &value, const BVar &x) { value -= x; });
		}
		BVar Multiply(const BListType &args) {
			return internal::FoldLeft(args, [](BVar &value, const BVar &x) { value *= x; });
		}
		BVar Divide(const BListType &args) {
			return internal::FoldLeft(args, [](BVar &value, const BVar &x) { value /= x; });
		}
		BVar Equals(const BListType &args) {
			// TODO: express across all vars
			return args.at(0) == args.at(1) ? True : False;
		}
		BVar NotEquals(const BListType &args) {
			return args.at(0) != args.at(1) ? True : False;
		}
	}
}

using namespace Bliss;

enum TestFlags {
	Primitive = 1 << 1,
	Swapping = 1 << 2,
	EnvTest = 1 << 3,
	AddTest = 1 << 4,
	FacTest = 1 << 5,
};

int main()
{
	TestFlags tf = FacTest;

	if (Bliss::Init()) {
		printf("Init failed?\n");
		return 0;
	}

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

	/* Code:
	 * (begin
	 *   (define add (lambda (x y) (+ x y)))
	 *   (print (add 1 2))
	 * )
	 */
	BListType code({
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
	
	BEnvPtr env = std::make_shared<BEnvironment>();
	env->TryInsert(AtomLibrary.declare("print"), BVar(StandardLibrary::Print));
	env->TryInsert(AtomLibrary.declare("+"), BVar(StandardLibrary::Plus));
	env->TryInsert(AtomLibrary.declare("-"), BVar(StandardLibrary::Minus));
	env->TryInsert(AtomLibrary.declare("*"), BVar(StandardLibrary::Multiply));
	env->TryInsert(AtomLibrary.declare("/"), BVar(StandardLibrary::Divide));
	env->TryInsert(AtomLibrary.declare("=="), BVar(StandardLibrary::Equals));
	env->TryInsert(AtomLibrary.declare("!="), BVar(StandardLibrary::NotEquals));

	BVar _env = BVar::MakeBEnvPtr(BEnvPtr(env));

	if (tf & EnvTest) {
		BVar x = BVar::Atom("x");
		env->TryInsert(AtomLibrary.declare("x"), BVar("test"));
		std::cout << "Code: " << x << "\n";
		std::cout << "Result: " << Eval(x, _env) << "\n";
	}

	if (tf & AddTest) {
		std::cout << "Code: " << code << "\n";
		try {
			std::cout << "Result: " << Eval(code, _env) << "\n";
		} catch (BRuntimeException &bre) {
			std::cerr << "Exception: " << bre.what() << "\n";
		} catch (...) {
			std::cerr << "Exception caught.\n";
		}
	}

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
	if (tf & FacTest) {
		std::cout << "Code: " << code << "\n";
		try {
			std::cout << "Result: " << Eval(code, _env) << "\n";
		} catch (BRuntimeException &bre) {
			std::cerr << "Exception: " << bre.what() << "\n";
		} catch (...) {
			std::cerr << "Exception caught.\n";
		}
	}

	// Cleanup
	env->Clear();

	return 0;
}
