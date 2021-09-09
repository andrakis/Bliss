// Bliss.cpp : Defines the entry point for the application.
//

#include <iostream>
#include "Bliss.h"

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


	BVar BVarContainer::head() const { return Nil; }
	BVar BVarContainer::tail() const { return Nil; }

	BVarContainer *BVarIntContainer::duplicate() const {
		return new BVarIntContainer(value);
	}
	BVarContainer *BVarStringContainer::duplicate() const {
		return new BVarStringContainer(value);
	}
	BVarContainer *BVarAtomContainer::duplicate() const {
		return new BVarAtomContainer(*this);
	}
	BVarContainer *BVarListContainer::duplicate() const {
		return new BVarListContainer(*this);
	}

	BVar BVar::List() {
		return BVar(new BVarListContainer());
	}
	BVar BVar::List(const ListType &l) {
		return BVar(new BVarListContainer(l));
	}
	BVar BVarContainer::index(size_t index) const
	{
		return Nil;
	}

	bool BVarListContainer::CompEq(const BVarContainer &other) const {
		if (other.type != BVarType::List)
			return false;
		const BVarListContainer *lc = dynamic_cast<const BVarListContainer*>(&other);
		if (!lc)
			return false;
		auto it1 = value.cbegin(), it2 = lc->value.cbegin();
		for (; 
			it1 != value.cend() && it2 != lc->value.cend();
			++it1, ++it2) {
			if (!it1->CompEq(*it2))
				return false;
		}
		return it1 == value.cend() && it2 == lc->value.cend();
	}

	BVar BVarStringContainer::index(size_t index) const {
		auto it = value.cbegin() + index;
		if (it != value.cend())
			return BVar(std::string(value, index, 1));
		return Nil;
	}
	BVar BVarStringContainer::head() const { 
		auto it = value.cbegin();
		if (it != value.cend())
			return BVar(std::string(1, *it));
		return Nil;
	}
	BVar BVarStringContainer::tail() const {
		auto it = value.crbegin();
		if (it != value.crend())
			return BVar(std::string(1, *it));
		return Nil;
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
