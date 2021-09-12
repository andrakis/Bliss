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
		return 0;
	}
}

// BVar
namespace Bliss {
	using namespace Containers;

	// Environment
	bool BEnvironment::TryFind(BAtomType name, /* out */ BVar &result) {
		auto it = map.find(name);
		if (it != map.end()) {
			result = it->second;
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
	}

	// Eval
	BVar Eval(const BVar &x, BVar env) throw(BRuntimeException) {
		const BVarType type = x.Type();
		if (type == BVarType::Atom)
		{
			BEnvPtr in_env = env.EnvPtr();
			BVar out(0);
			if (in_env->TryFind(x.AtomValue(), out)) {
				return out;
			}
			throw new BRuntimeException(std::string("Key not found: ") + x.StringValue());
		} else if(type != BVarType::List) {
			return x;
		}
		if (x.Length() == 0)
			return Nil;
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
				return Eval(cond == False ? condeq : alt, env);
			} else if (first_atom == builtin_define) {
				BEnvPtr in_env = env.EnvPtr();
				const BVar &name = rest.Head();
				const BVar &value = Eval(rest.Tail().Head(), env);
				if (!in_env->TryInsert(name.AtomValue(), value)) {
					throw new BRuntimeException(std::string("Key already defined: ") + name.StringValue());
				}
				return value;
			} else if (first_atom == builtin_set) {
				///
				BEnvPtr in_env = env.EnvPtr();
				const BVar &name = rest.Head();
				const BVar &value = Eval(rest.Tail().Head(), env);
				if (!in_env->TrySet(name.AtomValue(), value)) {
					throw new BRuntimeException(std::string("Key not defined: ") + name.StringValue());
				}
				return value;
			} else if (first_atom == builtin_lambda) {
				return BVar(new BVarLambdaContainer(x.Index(1), x.Index(2), env.EnvPtr()));
			} else if (first_atom == builtin_begin) {
				// TODO: ugly
				auto it = rest.CBegin();
				auto end = rest.CEnd() - 1;
				for( ; it != end; ++it)
					Eval(*it, env);
				return Eval(*it, env);
			}
			// Function call.
			BListType args;
			BVar fun = Eval(first, env);
			auto it = rest.CBegin();
			auto end = rest.CEnd();
			for ( ; it != end; ++it)
				args.push_back(Eval(*it, env));
			switch (fun.Type()) {
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

					if(!child_env->TryInsert(it2->AtomValue(), *it1))
						throw new BRuntimeException(std::string("Key already defined: ") + it2->StringValue());
				}
				// Evaluate under lambda environment
				return Eval(fun.Index(1), BVar::MakeBEnvPtr(child_env));
			} break;
			case BVarType::Proc:
				return fun.ProcValue()(args);
			case BVarType::ProcWithEnvironment:
				return fun.ProcWithEnvironmentValue()(args, env.EnvPtr());
			default:
				break;
			}
			throw new BRuntimeException("Invalid type");
		}
		throw new BRuntimeException("Invalid type");
		// todo...
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

		/*BVar Plus(const BListType &args) {
			if (args.empty()) return BVar(0);
			BVar value = args.front();
			for(auto it = args.cbegin() + 1; it != args.cend(); ++it)
				value += *it;
			return value;
		}*/

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
	}
}

using namespace Bliss;

int main()
{
	if (Bliss::Init()) {
		printf("Init failed?\n");
		return 0;
	}

	BVar a(0), b("1"), c = BVar::Atom("test"), d = BVar::Atom("test");
	BVar l = BVar::List();

	std::cout << "a: " << a << "\n"; a += 1;
	std::cout << "a: " << a << "\n"; a += 1;
	std::cout << "b: " << b << "\n"; b += 1;
	std::cout << "b: " << b << "\n"; b += 1;
	std::cout << "c: " << c << ", repr: " << c.StringRepr() << "\n";
	std::cout << "l: " << l << "\n";

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

	/* Code:
	 * (begin
	 *   (define add (lambda (x y) (+ x y)))
	 *   (print (add 1 2))
	 * )
	 */
	BListType code({
		BListType({BVar::Atom("begin"),
			BListType({BVar::Atom("define"),
				BVar::Atom("add"),
				BListType({BVar::Atom("lambda"),
					BListType({BVar::Atom("x"), BVar::Atom("y")}),
					BListType({BVar::Atom("+"), BVar::Atom("x"), BVar::Atom("y")})
				})
			}),
			BListType({BVar::Atom("print"), BListType({BVar::Atom("add"), 1, 2})})
		})
	});
	BVar x = BVar::Atom("x");
	
	BEnvPtr env = std::make_shared<BEnvironment>();
	env->TryInsert(AtomLibrary.declare("print"), BVar(StandardLibrary::Print));
	env->TryInsert(AtomLibrary.declare("+"), BVar(StandardLibrary::Plus));

	BVar _env = BVar::MakeBEnvPtr(BEnvPtr(env));
	env->TryInsert(AtomLibrary.declare("x"), BVar("test"));
	std::cout << "Code: " << x << "\n";
	std::cout << "Result: " << Eval(x, _env) << "\n";

	std::cout << "Code: " << code << "\n";
	std::cout << "Result: " << Eval(code, _env) << "\n";

	return 0;
}
