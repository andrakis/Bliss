// Bliss.h : Include file for standard system include files,
// or project specific include files.

#pragma once

#include <exception>    // Bliss::BRuntimeException
#include <functional>   // Bliss::ProcType, Bliss::ProcWithEnvironmentType
#include <list>         // Bliss::Parser::detail::TokenType
#include <map>          // Bliss::BEnvMapType
#include <memory>       // Bliss::BEnvPtr, Bliss::BVarContainerType
#include <string>
#include <sstream>      // Bliss::BVar::operator<<
#include <vector>       // Bliss::BListType

#define Str(X) ((X)->StringValue())
#define Repr(X) ((X)->StringRepr())

/* Determine whether we're in 32 or 64bit mode */
#if !defined(ENV64BIT) && !defined(ENV32BIT)
    /* Check windows */
    #if _WIN32 || _WIN64
        #if _WIN64
            #define ENV64BIT 1
        #else
            #define ENV32BIT 1
        #endif
    #endif

    /* Check GCC */
    #if __GNUC__
        #if __x86_64__ || __ppc64__
            #define ENV64BIT 1
        #else
            #define ENV32BIT 1
        #endif
    #endif
#endif /* !ENV64BIT && !ENV32BIT */

namespace Bliss {
	enum class BVarType {
		Integer,
		String,
		Atom,
		List,
		Lambda,
		Environment,
		Proc,
		ProcWithEnvironment,
		Exception,
		/// <summary>
		/// Use the CustomType() member
		/// </summary>
		Custom
	};
	typedef size_t BAtomType;

#if ENV64BIT
	typedef long BIntType;
#else
	typedef int BIntType;
#endif

	typedef std::string BCustomType;

	class BVar;
	class BVarContainer;
	class BEnvironment;

	extern BVar True, False, Nil;

	typedef typename std::shared_ptr<BEnvironment> BEnvPtr;
	typedef typename std::map<BAtomType,BVar> BEnvMapType;
	typedef typename std::exception BExceptionType;

	class BEnvironment {
	public:
		BEnvironment() : parent(nullptr) { }
		BEnvironment(BEnvPtr Parent) : parent(Parent) { }
		bool TryFind(BAtomType name, /* out */ BVar &result);
		bool TryInsert(BAtomType name, const BVar &value);
		bool TrySet(BAtomType name, const BVar &value);
		void Clear() { map.clear(); }
	private:
		BEnvMapType map;
	public:
		BEnvPtr parent;
	};

	typedef typename std::vector<BVar> BListType;
	typedef std::function<BVar(const BListType &)> ProcType;
	typedef std::function<BVar(const BListType &, BEnvPtr)> ProcWithEnvironmentType;

	bool Init();

	class BRuntimeException : public std::exception {
	public:
	#if _MSC_VER
		BRuntimeException(std::string message) : std::exception(message.c_str()) { }
	#else
		std::string msg;
		BRuntimeException(std::string message) : msg(message) { }
		#ifndef _GLIBCXX_TXN_SAFE_DYN 
		  #define _GLIBCXX_TXN_SAFE_DYN 
		#endif
		#ifndef _GLIBCXX_USE_NOEXCEPT 
		  #define _GLIBCXX_USE_NOEXCEPT 
		#endif
		const char *what() const _GLIBCXX_TXN_SAFE_DYN _GLIBCXX_USE_NOEXCEPT { return msg.c_str(); }
	#endif
	};

	class BInvalidOperationException : public BRuntimeException {
	public:
		BInvalidOperationException() : BRuntimeException("Invalid operation") { }
		BInvalidOperationException(std::string msg) : BRuntimeException(msg) { }
	};

	class BVar {
#ifdef BLISS_ALWAYS_SHARED
		typedef std::shared_ptr<BVarContainer> BVarContainerType;
#else
		typedef std::unique_ptr<BVarContainer> BVarContainerType;
#endif
		BVarContainerType container;

	public:
		BVar() : BVar(0) { }
		BVar(int value);
		BVar(std::string value);
		BVar(const BListType &list);
		BVar(ProcType proc);
		BVar(ProcWithEnvironmentType proc);
		BVar(BExceptionType);
		BVar(BVarContainer *cnt);
		BVar(BVarContainer const &cnt);
		BVar(BVar const &other);
		static BVar Atom(std::string name);
		static BVar Atom(BAtomType id);
		static BVar List();
		static BVar List(const BListType &l);
		static BVar Exception(BExceptionType);
		static BVar Exception(const std::string &);
		static BVar MakeBEnvPtr(BEnvPtr ptr);
		~BVar();
		BVarType Type() const;
		BCustomType CustomType() const;
		BAtomType AtomValue() const;
		BIntType IntValue() const;
		std::string StringValue() const;
		std::string StringRepr() const;
		BListType::iterator Begin();
		BListType::iterator End();
		BListType::const_iterator CBegin() const;
		BListType::const_iterator CEnd() const;
		const BVar &Head() const;
		BVar Tail() const;
		const BVar &Index(size_t Index) const;
		size_t Length() const;
		BEnvPtr EnvPtr();
		ProcType ProcValue() const;
		ProcWithEnvironmentType ProcWithEnvironmentValue() const;
		BExceptionType Exception();
		BExceptionType Exception() const;
		bool CompEq (const BVar &other) const;
		void assign(BVarContainer* cnt);
		void assign(const BVar &other);
		BVar &operator+= (const BVar &rhs)&;
		BVar &operator-= (const BVar &rhs)&;
		BVar &operator*= (const BVar &rhs)&;
		BVar &operator/= (const BVar &rhs)&;
		bool operator== (const BVar &rhs) const;
		bool operator!= (const BVar &rhs) const;
		BVar &operator=(const BVar &&other) noexcept;
		BVar &operator=(BVar &other) noexcept;
	};

	std::ostream &operator<<(std::ostream &os, const BVar &v);

	namespace detail {
		extern BListType emptyList;
		extern BRuntimeException non_exception;
		extern BCustomType non_custom_type;
		using DepthType = signed;  // has a tendency to underflow if an exception occurs
	}

	namespace util {
		template<typename T>
		bool TryStringToNumber(const std::string &str, T& result) {
			std::stringstream stream(str);
			stream >> result;
			return !stream.fail();
		}
	}

	namespace Parser {
		class ParserException : public BRuntimeException {
		public:
			ParserException() : BRuntimeException("Parser exception") { }
			ParserException(std::string msg) : BRuntimeException(msg) { }
		};

		namespace detail {
			typedef std::string StringType;
			using TokenType = std::string;
			typedef std::list<TokenType> TokenListType;
			TokenListType tokenise(const TokenType &str);
			BVar atom(const TokenType &token);
			BVar read_from(TokenListType &tokens);
			BVar read(const StringType &str);
		}

		BVar Read(const detail::StringType &str);
	}

	namespace StandardLibrary {
		BVar Exec (const BListType &args);
	}

	class BVarContainer {
	protected:
		BVarContainer(BVarType t) : type(t) { }
	public:
		const BVarType type;
		virtual BCustomType CustomType() const { return detail::non_custom_type; }
		virtual ~BVarContainer() { }
		virtual bool CanCastInt() const { return false; }
		virtual bool CanCastString() const { return false; }
		virtual BAtomType AtomValue() const { return 0; }
		virtual BIntType IntValue() const { return 0; }
		virtual std::string StringValue() const = 0;
		virtual std::string StringRepr() const = 0;
		virtual BVarContainer *duplicate() const = 0;
		virtual void add(const BVarContainer &other) {
			(void)other;
			throw BInvalidOperationException("add: not implemented for this type");
		}
		virtual void subtract(const BVarContainer &other) {
			(void)other;
			throw BInvalidOperationException("subtract: not implemented for this type");
		}
		virtual void multiply(const BVarContainer &other) {
			(void)other;
			throw BInvalidOperationException("multiply: not implemented for this type");
		}
		virtual void divide(const BVarContainer &other) {
			(void)other;
			throw BInvalidOperationException("divide: not implemented for this type");
		}
		virtual bool CompEq(const BVarContainer &other) const {
			(void)other;
			return false;
		}
		virtual const BVar &Head() const { return Nil; }
		virtual BVar Tail() const { return Nil; }
		virtual const BVar &Index(size_t Index) const { (void)Index; return Nil; }
		virtual size_t Length() const { return 0; }
		virtual BListType::iterator Begin () { return detail::emptyList.begin(); }
		virtual BListType::iterator End () { return detail::emptyList.end(); }
		virtual BListType::const_iterator CBegin () const { return detail::emptyList.cbegin(); }
		virtual BListType::const_iterator CEnd () const { return detail::emptyList.cend(); }

		virtual BEnvPtr EnvPtr() { return nullptr; }
		virtual ProcType ProcValue() const { return NotAProc; }
		virtual ProcWithEnvironmentType ProcWithEnvironmentValue() const { return NotAProcWithEnvironment; }
		virtual BExceptionType Exception() { return detail::non_exception; }
		virtual BExceptionType Exception() const { return detail::non_exception; }
	private:
		static BVar NotAProc(const BListType &args) {
			(void)args;
			return Nil;
		}
		static BVar NotAProcWithEnvironment(const BListType &args, BEnvPtr env) {
			(void)args;
			(void)env;
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
			void add(const BVarContainer &other) {
				// TODO: type checking
				if (!other.CanCastInt())
					throw new BInvalidOperationException();
				value += other.IntValue();
			}
			void subtract(const BVarContainer &other) {
				// TODO: type checking
				if (!other.CanCastInt())
					throw new BInvalidOperationException();
				value -= other.IntValue();
			}
			void multiply(const BVarContainer &other) {
				// TODO: type checking
				if (!other.CanCastInt())
					throw new BInvalidOperationException();
				value *= other.IntValue();
			}
			void divide(const BVarContainer &other) {
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
			void add(const BVarContainer &other) {
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
			/*BVar &Index(size_t Index) const {
				auto it = value.cbegin() + Index;
				if (it != value.cend())
					return BVar(std::string(value, Index, 1));
				return Nil;
			}
			BVar &Head() const {
				auto it = value.cbegin();
				if (it != value.cend())
					return BVar(std::string(1, *it));
				return Nil;
			}
			BVar &Tail() const {
				auto it = value.crbegin();
				if (it != value.crend())
					return BVar(std::string(1, *it));
				return Nil;
			}*/
			size_t Length() const { return value.length(); }
		};

		class BVarAtomContainer : public BVarContainer {
			BAtomType value;
		public:
			BVarAtomContainer(std::string n) : BVarContainer(BVarType::Atom),
				value(AtomLibrary.declare(n)) { }
			BVarAtomContainer(BAtomType id) : BVarContainer(BVarType::Atom),
				value(id) { }
			BVarAtomContainer(const BVarAtomContainer &other) : BVarContainer(BVarType::Atom),
				value(other.value) { }
			BAtomType AtomValue() const { return value; }
			std::string StringValue() const { return AtomLibrary.find(value); }
			std::string StringRepr() const { return "'" + StringValue(); }
			bool CompEq(const BVarContainer &other) const {
				if (other.type != BVarType::Atom)
					return false;
				return value == other.AtomValue();
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
			void add(const BVarContainer &other) {
				value.push_back(BVar(other.duplicate()));
			}

			std::string StringValue() const { return StringMap(); }
			std::string StringRepr() const { return StringMap(true); }
			BVarContainer *duplicate() const {
				return new BVarListContainer(*this);
			}
			bool CompEq(const BVarContainer &other) const {
				if (other.type != BVarType::List)
					return false;
				auto it1 = value.cbegin(), it2 = other.CBegin();
				for (; 
					it1 != value.cend() && it2 != other.CEnd();
					++it1, ++it2) {
					if (!it1->CompEq(*it2))
						return false;
				}
				return it1 == value.cend() && it2 == other.CEnd();
			}

			const BVar &Index(size_t Index) const {
				auto it = value.cbegin() + Index;
				if (it != value.cend())
					return value[Index];
				return Nil;
			}
			const BVar &Head() const { 
				auto it = value.cbegin();
				if (it != value.cend())
					return value[0];
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

		class BVarExceptionContainer : public BVarContainer {
			BExceptionType exception;
		public:
			BVarExceptionContainer(const BVarExceptionContainer &other) : BVarContainer(BVarType::Exception), exception(other.exception) { }
			BVarExceptionContainer(BExceptionType ex) : BVarContainer(BVarType::Exception), exception(ex) { }
			std::string StringValue() const { return std::string("#Exception: ") + exception.what(); /* TODO */ }
			std::string StringRepr() const { return StringValue(); }
			BVarContainer *duplicate() const { return new BVarExceptionContainer(*this); }
			BExceptionType Exception() { return exception; }
			BExceptionType Exception() const { return exception; }
		};

		/// <summary>
		/// A custom type (abstract) allows for implementing any custom subtype, programmatically.
		/// Custom types cannot be used in math operations, and their underlying value may not necessarily
		/// be one that can be represented by standard types. It is a container that may
		/// be passed around and used in function calls, but not directly interacted with.
		/// </summary>
		class BVarCustomTypeContainer : public BVarContainer {
			BCustomType custom_type;
		protected:
			BVarCustomTypeContainer(const BVarCustomTypeContainer &other) : BVarContainer(BVarType::Custom), custom_type(other.custom_type) { }
			BVarCustomTypeContainer(BCustomType type) : BVarContainer(BVarType::Custom), custom_type(type) { }
			virtual BVarContainer *custom_duplicate() const = 0;
		public:
			BVarContainer *duplicate() const { return custom_duplicate(); }
		};
	}
}
