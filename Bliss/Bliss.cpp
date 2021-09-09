// Bliss.cpp : Defines the entry point for the application.
//

#include <iostream>
#include "Bliss.h"

// Shared
namespace Bliss {
	BVarAtomLibrary AtomLibrary;
	BVar True(0), False(0), Nil(0); // Filled in later

	// later
	bool Init() {
		True = BVar::Atom("true");
		False = BVar::Atom("false");
		Nil = BVar::Atom("nil");
		return 0;
	}
}

// BVar
namespace Bliss {
	using namespace Containers;

	BVar::BVar(int value) : container(new BVarIntContainer(value)) { }
	BVar::BVar(std::string value) : container(new BVarStringContainer(value)) { }
	BVar::BVar(BVarContainer *cnt) : container(cnt) { }
	BVar::BVar(BVarContainer const &cnt) : container(cnt.duplicate()) { }
	BVar::BVar(BVar const &other) : container(other.container->duplicate()) { }
	BVar BVar::Atom(std::string name) {
		return BVar(new BVarAtomContainer(name));
	}
	BVar BVar::List() {
		return BVar(new BVarListContainer());
	}
	BVar BVar::List(const ListType &l) {
		return BVar(new BVarListContainer(l));
	}
	BVar::~BVar() { delete container; }
	int BVar::IntValue() const { return container->IntValue(); }
	std::string BVar::StringValue() const { return container->StringValue(); }
	std::string BVar::StringRepr() const { return container->StringRepr(); }
	bool BVar::CompEq (const BVar &other) const { return container->CompEq(*other.container); }
	void BVar::assign(BVarContainer* cnt) {
		free(container);
		container = cnt->duplicate();
	}
	BVar &BVar::operator+= (const BVar &rhs)& throw(BInvalidOperationException) {
		container->add(*rhs.container);
		return *this;
	}
	BVar &BVar::operator-= (const BVar &rhs)& throw (BInvalidOperationException) {
		container->subtract(*rhs.container);
		return *this;
	} 
	bool BVar::operator== (const BVar &rhs) { return container->CompEq(*rhs.container); }
	bool BVar::operator!= (const BVar &rhs) { return !(operator==(rhs)); }
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

	return 0;
}
