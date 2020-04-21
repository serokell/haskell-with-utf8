// SPDX-FileCopyrightText: 2020 Serokell <https://serokell.io/>
//
// SPDX-License-Identifier: MPL-2.0

#define INLINE

#include <HsBase.h>

#if !defined(mingw32_HOST_OS)

#if defined(HAVE_LIBCHARSET)
#include <libcharset.h>
const char* libcharsetEncoding(void) {
  return locale_charset();
}
#endif

#if defined(HAVE_LANGINFO_H)
#include <langinfo.h>
const char* langinfoEncoding(void) {
  return nl_langinfo(CODESET);
}
#endif

#endif
