// Bliss.h : Include file for standard system include files,
// or project specific include files.

#pragma once

#include <deque>
#include <exception>
#include <map>
#include <string>
#include <vector>

#define Str(X) ((X)->StringValue())
#define Repr(X) ((X)->StringRepr())

namespace Bliss {
	class BVar;
	extern BVar True, False, Nil;
	bool Init();

	enum class BVarType {
		Integer,
		String,
		Atom,
		List
	};

	struct BInvalidOperationException : public std::exception {
		BInvalidOperationException() : message("Invalid operation") { }
		BInvalidOperationException(std::string msg) : message(msg) { }
		const char *what() const throw() {
			return message.c_str();
		}
	private:
		std::string message;
	};

	class BVarContainer {
	protected:
		BVarContainer(BVarType t) : type(t) { }
	public:
		const BVarType type;
		virtual ~BVarContainer() { }
		virtual bool CanCastInt() const = 0;
		virtual bool CanCastString() const = 0;
		virtual int IntValue() const = 0;
		virtual std::string StringValue() const = 0;
		virtual std::string StringRepr() const = 0;
		virtual BVarContainer *duplicate() const = 0;
		virtual void add(const BVarContainer &other) throw(BInvalidOperationException) {
			throw new BInvalidOperationException("add: not implemented for this type");
		}
		virtual void subtract(const BVarContainer &other) throw(BInvalidOperationException) {
			throw new BInvalidOperationException("subtract: not implemented for this type");
		}
		virtual void multiply(const BVarContainer &other) throw(BInvalidOperationException) {
			throw new BInvalidOperationException("multiply: not implemented for this type");
		}
		virtual void divide(const BVarContainer &other) throw(BInvalidOperationException) {
			throw new BInvalidOperationException("divide: not implemented for this type");
		}
		virtual bool CompEq(const BVarContainer &other) const {
			return false;
		}
		virtual BVar index(size_t index) const;
		virtual BVar head() const;
		virtual BVar tail() const;
	};

	class BVarIntContainer : public BVarContainer {
	protected:
		int value;
	public:
		BVarIntContainer(int v) : BVarContainer(BVarType::Integer), value(v) { }
		int IntValue() const { return value; }
		std::string StringValue() const { return std::to_string(value); }
		std::string StringRepr() const { return StringValue(); }
		BVarContainer* duplicate() const;
		void add(const BVarContainer &other) throw(BInvalidOperationException) {
			// TODO: type checking
			if (!other.CanCastInt())
				throw new BInvalidOperationException();
			value += other.IntValue();
		}
		void subtract(const BVarContainer &other) throw(BInvalidOperationException) {
			// TODO: type checking
			if (!other.CanCastInt())
				throw new BInvalidOperationException();
			value -= other.IntValue();
		}
		void multiply(const BVarContainer &other) throw(BInvalidOperationException) {
			// TODO: type checking
			if (!other.CanCastInt())
				throw new BInvalidOperationException();
			value *= other.IntValue();
		}
		void divide(const BVarContainer &other) throw(BInvalidOperationException) {
			// TODO: type checking
			if (!other.CanCastInt())
				throw new BInvalidOperationException();
			value /= other.IntValue();
		}
		bool CompEq(const BVarContainer &other) const {
			return value == other.IntValue();
		}
		bool CanCastInt() const { return true; }
		bool CanCastString() const { return true; }
	};

	class BVarStringContainer : public BVarContainer {
	protected:
		std::string value;
	public:
		BVarStringContainer(std::string v) : BVarContainer(BVarType::String), value(v) { }
		int IntValue() const { return std::stoi(value); }
		std::string StringValue() const { return value; }
		std::string StringRepr() const { return "\"" + value + "\""; }
		BVarContainer* duplicate() const;
		void add(const BVarContainer &other) throw(BInvalidOperationException) {
			// TODO: type checking
			if (!other.CanCastString())
				throw new BInvalidOperationException();
			value += other.StringValue();
		}
		bool CompEq(const BVarContainer &other) const {
			return value == other.StringValue();
		}
		bool CanCastInt() const { return true; }
		bool CanCastString() const { return true; }
		virtual BVar index(size_t index) const;
		virtual BVar head() const;
		virtual BVar tail() const;
	};

	class BVarAtomLibrary {
	public:
		typedef std::map<int,std::string> AtomById;
		typedef std::map<std::string,int> AtomByName;

	protected:
		AtomById ById;
		AtomByName ByName;
		int AtomCounter = 0;
	public:
		int declare(std::string name) {
			AtomByName::iterator it = ByName.find(name);
			if (it == ByName.cend()) {
				auto itIn = ByName.emplace(name, AtomCounter);
				ById.emplace(AtomCounter++, name);
				it = itIn.first;
			}
			return it->second;
		}
		std::string find(int id) {
			auto it = ById.find(id);
			return it->second;
		}
		int find(std::string name) {
			auto it = ByName.find(name);
			return it->second;
		}
	};
	extern BVarAtomLibrary AtomLibrary;
	class BVarAtomContainer : public BVarContainer {
		std::string name;
		int value;
	public:
		BVarAtomContainer(std::string n) : BVarContainer(BVarType::Atom),
			value(AtomLibrary.declare(n)), name(n) { }
		BVarAtomContainer(const BVarAtomContainer &other) : BVarContainer(BVarType::Atom),
			value(other.value), name(other.name) { }
		int IntValue() const { return value; }
		std::string StringValue() const { return name; }
		std::string StringRepr() const { return "'" + name; }
		bool CompEq(const BVarContainer &other) const {
			if (other.type != BVarType::Atom)
				return false;
			return value == other.IntValue();
		}

		BVarContainer* duplicate() const;
		bool CanCastInt() const { return false; }
		bool CanCastString() const { return false; }
	};

	class BVar;
	typedef typename std::deque<BVar> ListType;
	class BVar {
		BVarContainer* container;
	public:
		BVar(int value) : container(new BVarIntContainer(value)) { }
		BVar(std::string value) : container(new BVarStringContainer(value)) { }
		BVar(BVarContainer *cnt) : container(cnt) { }
		BVar(BVarContainer const &cnt) : container(cnt.duplicate()) { }
		BVar(BVar const &other) : container(other.container->duplicate()) { }
		static BVar Atom(std::string name) {
			return BVar(new BVarAtomContainer(name));
		}
		static BVar List();
		static BVar List(const ListType &l);
		~BVar() { delete container; }
		int IntValue() const { return container->IntValue(); }
		std::string StringValue() const { return container->StringValue(); }
		std::string StringRepr() const { return container->StringRepr(); }
		bool CompEq (const BVar &other) const { return container->CompEq(*other.container); }
		void assign(BVarContainer* cnt) {
			free(container);
			container = cnt->duplicate();
		}
		BVar &operator+= (const BVar &rhs)& throw(BInvalidOperationException) {
			container->add(*rhs.container);
			return *this;
		}
		BVar &operator-= (const BVar &rhs)& throw (BInvalidOperationException) {
			container->subtract(*rhs.container);
			return *this;
		}
		bool operator== (const BVar &rhs) {
			return container->CompEq(*rhs.container);
		}
		bool operator!= (const BVar &rhs) {
			return !(operator==(rhs));
		}
		friend std::ostream &operator<<(std::ostream &os, const BVar &v) {
			os << v.container->StringValue();
			return os;
		}
		BVar &operator=(const BVar &&other) noexcept {
			BVar tmp(other);
			std::swap(container, tmp.container);
			return *this;
		}
		BVar &operator=(BVar &other) noexcept {
			std::swap(container, other.container);
			return *this;
		}
	};

	class BVarListContainer : public BVarContainer {
	public:
		typedef typename ListType::iterator iterator;
		typedef typename ListType::const_iterator const_iterator;
	private:
		ListType value;
		std::string StringMap(bool repr = false) const {
			std::string result = "(";
			for(auto it = value.cbegin(); it != value.cend(); ++it) {
				if (it != value.cbegin())
					result += " ";
				result += repr ? it->StringRepr() : it->StringValue();
			}
			result += ")";
			return result;
		}
	public:
		BVarListContainer() : BVarContainer(BVarType::List), value() { }
		BVarListContainer(ListType l) : BVarContainer(BVarType::List), value(l) { }
		void add(const BVarContainer &other) throw(BInvalidOperationException) {
			value.push_back(BVar(other.duplicate()));
		}

		int IntValue() const { return 0; }
		std::string StringValue() const { return StringMap(); }
		std::string StringRepr() const { return StringMap(true); }
		BVarContainer* duplicate() const;
		bool CompEq(const BVarContainer &other) const;
		bool CanCastInt() const { return false; }
		bool CanCastString() const { return false; }

		BVar index(size_t index) const {
			auto it = value.cbegin() + index;
			if (it != value.cend())
				return *it;
			return Nil;
		}
		BVar head() const { 
			auto it = value.cbegin();
			if (it != value.cend())
				return *it;
			return Nil;
		}
		BVar tail() const {
			auto it = value.crbegin();
			if (it != value.crend())
				return *it;
			return Nil;
		}
	};

}
