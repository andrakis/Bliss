#include <cstdio>
#include <exception>
#include <iostream>
#include <memory>
#include <stdexcept>
#include <string>
#include <array>

#ifdef _WINDOWS
#define popen _popen
#define pclose _pclose
#endif

#include "Bliss.h"

namespace Bliss {
    namespace StandardLibrary {
		BVar Exec(const BListType &args) {
			try {
				std::string arg = args[0].StringValue();
				const char *cmd = arg.c_str();
				std::array<char, 128> buffer;
				std::string result;
				std::unique_ptr<FILE, decltype(&pclose)> pipe(popen(cmd, "r"), pclose);
				if (!pipe) {
					return BVar::Exception("Exec: pipe failed");
				}
				while (fgets(buffer.data(), (int)buffer.size(), pipe.get()) != nullptr) {
					result += buffer.data();
				}
				return BVar(result);
			} catch (BExceptionType &bex) {
				return BVar::Exception(bex);
			}
		}
    }
}