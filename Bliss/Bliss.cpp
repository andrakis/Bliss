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
	BVar BVar::Atom(std::string name) {
		return BVar(new BVarAtomContainer(name));
	}
	BVar BVar::List() {
		return BVar(new BVarListContainer());
	}
	BVar BVar::List(const BListType &l) {
		return BVar(new BVarListContainer(l));
	}
	BVar BVar::MakeBEnvPtr(BEnvPtr ptr) {
		return BVar(new BVarEnvironmentContainer(ptr));
	}
	BVar::~BVar() { }
	BVarContainer* BVar::_container() { return container->_container(); }
	BVarType BVar::Type() const { return container->type; }
	BAtomType BVar::AtomValue() const { return container->AtomValue(); }
	BIntType BVar::IntValue() const { return container->IntValue(); }
	std::string BVar::StringValue() const { return container->StringValue(); }
	std::string BVar::StringRepr() const { return container->StringRepr(); }
	bool BVar::CompEq (const BVar &other) const { return container->CompEq(*other.container); }
	BVar BVar::Head() const { return container->Head(); }
	BVar BVar::Tail() const { return container->Tail(); }
	BVar BVar::Index(size_t Index) const { return container->Index(Index); }
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
				return BVar(new BVarLambdaContainer(x.Index(1), x.Index(2)));
			} else if (first_atom == builtin_begin) {
				// TODO: ugly
				auto it = rest._container()->CBegin();
				auto end = rest._container()->CEnd() - 1;
				for( ; it != end; ++it)
					Eval(*it, env);
				return Eval(*it, env);
			}
			// Function call.
			BListType args;
			const BVar &fun = x.Head();
			auto it = rest._container()->CBegin();
			auto end = rest._container()->CEnd();
			for ( ; it != end; ++it)
				args.push_back(Eval(*it, env));
			switch (fun.Type()) {
			case BVarType::Lambda: {
				// Create new environment
				BEnvPtr child_env = std::make_shared<BEnvironment>(env.EnvPtr());
				// Add arguments to environment
				BVar fun_args = fun.Head();
				if (fun_args.Type() == BVarType::Atom) {
					// Single argument, not a list. Assign all arguments to this name
					BListType tmp;
					tmp.push_back(BVar(args));
					args = BListType(tmp);
					BListType tmp2;
					tmp2.push_back(fun_args);
					fun_args = BVar(tmp2);
				}
				for (auto it1 = args.cbegin(), it2 = fun_args._container()->CBegin();
					it1 != args.cend(), it2 != fun_args._container()->CEnd();
					++it1, ++it2) {
					child_env->TryInsert(it2->AtomValue(), *it1);
				}
				return Eval(fun.Tail().Head(), BVar::MakeBEnvPtr(child_env));
			} break;
			case BVarType::Proc:
				return fun.ProcValue()(args);
			case BVarType::ProcWithEnvironment:
				return fun.ProcWithEnvironmentValue()(args, env.EnvPtr());
			default:
				break;
			}
		}
		throw new BRuntimeException("Invalid type");
		// todo...
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

	std::cout << "a: " << a << std::endl; a += 1;
	std::cout << "a: " << a << std::endl; a += 1;
	std::cout << "b: " << b << std::endl; b += 1;
	std::cout << "b: " << b << std::endl; b += 1;
	std::cout << "c: " << c << ", repr: " << c.StringRepr() << std::endl;
	std::cout << "l: " << l << std::endl;

	BVar tmp(a);
	a = b;
	b = c;
	c = tmp;
	l += a;
	l += b;
	l += c;
	
	std::cout << "a: " << a << std::endl; 
	std::cout << "b: " << b << std::endl; 
	std::cout << "c: " << c << ", repr: " << c.StringRepr() << std::endl;
	std::cout << "l: " << l << std::endl;

	BListType code;
	code.push_back(BVar(1));
	//BVar x(code);
	BVar x = BVar::Atom("x");
	BEnvPtr _env = std::make_shared<BEnvironment>();
	BVar env = BVar::MakeBEnvPtr(BEnvPtr(_env));
	_env->TryInsert(AtomLibrary.declare("x"), BVar("test"));
	std::cout << "Code: " << x << std::endl;
	std::cout << "Result: " << Eval(x, env) << std::endl;

	return 0;
}
