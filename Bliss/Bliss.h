// Bliss.h : Include file for standard system include files,
// or project specific include files.

#pragma once

#include <exception>
#include <functional>
#include <list>
#include <map>
#include <memory>
#include <string>
#include <tuple>
#include <vector>

#define Str(X) ((X)->StringValue())
#define Repr(X) ((X)->StringRepr())

namespace Bliss {
	enum class BVarType {
		Integer,
		String,
		Atom,
		List,
		Lambda,
		Environment,
		Proc,
		ProcWithEnvironment
	};
	typedef size_t BAtomType;
	typedef int BIntType;

	class BVar;
	class BVarContainer;
	class BEnvironment;

	extern BVar True, False, Nil;

	typedef typename std::shared_ptr<BEnvironment> BEnvPtr;
	typedef typename std::map<BAtomType,BVar> BEnvMapType;

	class BEnvironment {
	public:
		BEnvironment() : parent(nullptr) { }
		BEnvironment(BEnvPtr Parent) : parent(Parent) { }
		bool TryFind(BAtomType name, /* out */ BVar &result);
		bool TryInsert(BAtomType name, const BVar &value);
		bool TrySet(BAtomType name, const BVar &value);
	private:
		BEnvMapType map;
	public:
		BEnvPtr parent;
	};

	typedef typename std::vector<BVar> BListType;
	typedef std::function<BVar(const BListType &)> ProcType;
	typedef std::function<BVar(const BListType &, BEnvPtr)> ProcWithEnvironmentType;

	bool Init();

	struct BInvalidOperationException : public std::exception {
		BInvalidOperationException() : message("Invalid operation") { }
		BInvalidOperationException(std::string msg) : message(msg) { }
		const char *what() const throw() {
			return message.c_str();
		}
	private:
		std::string message;
	};

	struct BRuntimeException : public std::exception {
		BRuntimeException() : message("Invalid operation") { }
		BRuntimeException(std::string msg) : message(msg) { }
		const char *what() const throw() {
			return message.c_str();
		}
	private:
		std::string message;
	};

	class BVar {
		std::unique_ptr<BVarContainer> container;
	public:
		BVar() : BVar(0) { }
		BVar(int value);
		BVar(std::string value);
		BVar(const BListType &list);
		BVar(ProcType proc);
		BVar(ProcWithEnvironmentType proc);
		BVar(BVarContainer *cnt);
		BVar(BVarContainer const &cnt);
		BVar(BVar const &other);
		static BVar Atom(std::string name);
		static BVar List();
		static BVar List(const BListType &l);
		static BVar MakeBEnvPtr(BEnvPtr ptr);
		~BVar();
		BVarType Type() const;
		BAtomType AtomValue() const;
		BIntType IntValue() const;
		std::string StringValue() const;
		std::string StringRepr() const;
		BListType::iterator Begin();
		BListType::iterator End();
		BListType::const_iterator CBegin() const;
		BListType::const_iterator CEnd() const;
		BVar Head() const;
		BVar Tail() const;
		BVar Index(size_t Index) const;
		size_t Length() const;
		BEnvPtr EnvPtr();
		ProcType ProcValue() const;
		ProcWithEnvironmentType ProcWithEnvironmentValue() const;
		bool CompEq (const BVar &other) const;
		void assign(BVarContainer* cnt);
		void assign(const BVar &other);
		BVar &operator+= (const BVar &rhs)& throw(BInvalidOperationException);
		BVar &operator-= (const BVar &rhs)& throw (BInvalidOperationException);
		bool operator== (const BVar &rhs) const;
		bool operator!= (const BVar &rhs) const;
		BVar &operator=(const BVar &&other) noexcept;
		BVar &operator=(BVar &other) noexcept;
	};

	std::ostream &operator<<(std::ostream &os, const BVar &v);

	namespace internal {
		extern BListType emptyList;
	}

	class BVarContainer {
	protected:
		BVarContainer(BVarType t) : type(t) { }
	public:
		const BVarType type;
		virtual ~BVarContainer() { }
		virtual bool CanCastInt() const { return false; }
		virtual bool CanCastString() const { return false; }
		virtual BAtomType AtomValue() const { return 0; }
		virtual BIntType IntValue() const { return 0; }
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
		virtual BVar Head() const { return Nil; }
		virtual BVar Tail() const { return Nil; }
		virtual BVar Index(size_t Index) const { return Nil; }
		virtual size_t Length() const { return 0; }
		virtual BListType::iterator Begin () { return internal::emptyList.begin(); }
		virtual BListType::iterator End () { return internal::emptyList.end(); }
		virtual BListType::const_iterator CBegin () const { return internal::emptyList.cbegin(); }
		virtual BListType::const_iterator CEnd () const { return internal::emptyList.cend(); }

		virtual BEnvPtr EnvPtr() { return nullptr; }
		virtual ProcType ProcValue() const { return NotAProc; }
		virtual ProcWithEnvironmentType ProcWithEnvironmentValue() const { return NotAProcWithEnvironment; }
	private:
		static BVar NotAProc(const BListType &args) {
			return Nil;
		}
		static BVar NotAProcWithEnvironment(const BListType &args, BEnvPtr env) {
			return Nil;
		}
	};

	class BVarAtomLibrary {
	public:
		typedef std::map<BAtomType,std::string> AtomById;
		typedef std::map<std::string,BAtomType> AtomByName;

	protected:
		AtomById ById;
		AtomByName ByName;
		BAtomType AtomCounter = 0;
	public:
		BAtomType declare(std::string name) {
			AtomByName::iterator it = ByName.find(name);
			if (it == ByName.cend()) {
				auto itIn = ByName.emplace(name, AtomCounter);
				ById.emplace(AtomCounter++, name);
				it = itIn.first;
			}
			return it->second;
		}
		std::string find(BAtomType id) const {
			auto it = ById.find(id);
			return it->second;
		}
		BAtomType find(std::string name) {
			auto it = ByName.find(name);
			if (it == ByName.end())
				return declare(name);
			return it->second;
		}
	};

	extern BVarAtomLibrary AtomLibrary;

	namespace Containers {
		class BVarIntContainer : public BVarContainer {
		protected:
			int value;
		public:
			BVarIntContainer(BIntType v) : BVarContainer(BVarType::Integer), value(v) { }
			BIntType IntValue() const { return value; }
			std::string StringValue() const { return std::to_string(value); }
			std::string StringRepr() const { return StringValue(); }
			BVarContainer *duplicate() const {
				return new BVarIntContainer(value);
			}
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
			BIntType IntValue() const { return std::stoi(value); }
			std::string StringValue() const { return value; }
			std::string StringRepr() const { return "\"" + value + "\""; }
			BVarContainer *duplicate() const {
				return new BVarStringContainer(value);
			}
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
			BVar Index(size_t Index) const {
				auto it = value.cbegin() + Index;
				if (it != value.cend())
					return BVar(std::string(value, Index, 1));
				return Nil;
			}
			BVar Head() const {
				auto it = value.cbegin();
				if (it != value.cend())
					return BVar(std::string(1, *it));
				return Nil;
			}
			BVar Tail() const {
				auto it = value.crbegin();
				if (it != value.crend())
					return BVar(std::string(1, *it));
				return Nil;
			}
			size_t Length() const { return value.length(); }
		};

		class BVarAtomContainer : public BVarContainer {
			BAtomType value;
		public:
			BVarAtomContainer(std::string n) : BVarContainer(BVarType::Atom),
				value(AtomLibrary.declare(n)) { }
			BVarAtomContainer(const BVarAtomContainer &other) : BVarContainer(BVarType::Atom),
				value(other.value) { }
			BAtomType AtomValue() const { return value; }
			std::string StringValue() const { return "'" + AtomLibrary.find(value); }
			std::string StringRepr() const { return StringValue(); }
			bool CompEq(const BVarContainer &other) const {
				if (other.type != BVarType::Atom)
					return false;
				return value == other.IntValue();
			}

			BVarContainer *duplicate() const {
				return new BVarAtomContainer(*this);
			}
			bool CanCastString() const { return true; }
		};

		class BVarListContainer : public BVarContainer {
		public:
			typedef typename BListType::iterator iterator;
			typedef typename BListType::const_iterator const_iterator;
		private:
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
		protected:
			BListType value;
			BVarListContainer(BVarType type) : BVarContainer(type), value() { }
			BVarListContainer(BVarType type, const BListType &other) : BVarContainer(type), value(other) { }
			template<typename Iterator>
			BVarListContainer(BVarType type, Iterator start, Iterator end) : BVarContainer(type), value(start, end) { }
		public:
			BVarListContainer() : BVarContainer(BVarType::List), value() { }
			BVarListContainer(const BVarListContainer &other) : BVarContainer(BVarType::List), value(other.value) { }
			BVarListContainer(BListType l) : BVarContainer(BVarType::List), value(l) { }
			template<typename iterator_type>
			BVarListContainer(iterator_type begin, iterator_type end) : BVarContainer(BVarType::List),
				value(begin, end) { }
			void add(const BVarContainer &other) throw(BInvalidOperationException) {
				value.push_back(BVar(other.duplicate()));
			}

			// Internal function
			void _push_back(const BVar &ele) {
				value.push_back(ele);
			}

			std::string StringValue() const { return StringMap(); }
			std::string StringRepr() const { return StringMap(true); }
			BVarContainer *duplicate() const {
				return new BVarListContainer(*this);
			}
			bool CompEq(const BVarContainer &other) const {
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

			BVar Index(size_t Index) const {
				auto it = value.cbegin() + Index;
				if (it != value.cend())
					return *it;
				return Nil;
			}
			BVar Head() const { 
				auto it = value.cbegin();
				if (it != value.cend())
					return *it;
				return Nil;
			}
			BVar Tail() const {
				return BVar(new BVarListContainer(value.cbegin() + 1, value.cend()));
			}
			size_t Length() const { return value.size(); }
			BListType::iterator Begin () { return value.begin(); }
			BListType::iterator End () { return value.end(); }
			BListType::const_iterator CBegin () const { return value.cbegin(); }
			BListType::const_iterator CEnd () const { return value.cend(); }
		};

		class BVarLambdaContainer : public BVarListContainer {
			BEnvPtr env;
		public:
			BVarLambdaContainer(const BVarLambdaContainer &other) : BVarListContainer(BVarType::Lambda, other.value), env(other.env) { }
			BVarLambdaContainer(const BVar &args, const BVar &body, BEnvPtr _env) : BVarListContainer(BVarType::Lambda), env(_env) {
				value.push_back(args);
				value.push_back(body);
			}
			std::string StringValue() const { return "#Lambda"; /* TODO */ }
			std::string StringRepr() const { return StringValue(); }
			BVarContainer *duplicate() const { return new BVarLambdaContainer(*this); }
			BEnvPtr EnvPtr() { return env; }
		};

		class BVarProcContainer : public BVarContainer {
			ProcType proc;
		public:
			BVarProcContainer(const BVarProcContainer &other) : BVarContainer(BVarType::Proc), proc(other.proc) { }
			BVarProcContainer(ProcType _proc) : BVarContainer(BVarType::Proc), proc(_proc) { }
			std::string StringValue() const { return "#Proc"; /* TODO */ }
			std::string StringRepr() const { return StringValue(); }
			BVarContainer *duplicate() const { return new BVarProcContainer(*this); }
			ProcType ProcValue() const { return proc; }
		};

		class BVarProcWithEnvironmentContainer : public BVarContainer {
			ProcWithEnvironmentType proc;
		public:
			BVarProcWithEnvironmentContainer(const BVarProcWithEnvironmentContainer &other) : BVarContainer(BVarType::ProcWithEnvironment), proc(other.proc) { }
			BVarProcWithEnvironmentContainer(ProcWithEnvironmentType _proc) : BVarContainer(BVarType::ProcWithEnvironment), proc(_proc) { }
			std::string StringValue() const { return "#ProcWithEnvironment"; /* TODO */ }
			std::string StringRepr() const { return StringValue(); }
			BVarContainer *duplicate() const { return new BVarProcWithEnvironmentContainer(*this); }
			ProcWithEnvironmentType ProcWithEnvironmentValue() const { return proc; }
		};

		class BVarEnvironmentContainer : public BVarContainer {
			BEnvPtr env;
		public:
			BVarEnvironmentContainer(const BVarEnvironmentContainer &other) : BVarContainer(BVarType::Environment), env(other.env) { }
			BVarEnvironmentContainer(BEnvPtr _env) : BVarContainer(BVarType::Environment), env(_env) { }
			std::string StringValue() const { return "#Env"; /* TODO */ }
			std::string StringRepr() const { return StringValue(); }
			BVarContainer *duplicate() const { return new BVarEnvironmentContainer(*this); }
			BEnvPtr EnvPtr() { return env; }
		};
	}
}
