/***************************************************************************
 *                                  _   _ ____  _
 *  Project                     ___| | | |  _ \| |
 *                             / __| | | | |_) | |
 *                            | (__| |_| |  _ <| |___
 *                             \___|\___/|_| \_\_____|
 *
 * Copyright (C) 1998 - 2020, Daniel Stenberg, <daniel@haxx.se>, et al.
 *
 * This software is licensed as described in the file COPYING, which
 * you should have received as part of this distribution. The terms
 * are also available at https://curl.se/docs/copyright.html.
 *
 * You may opt to use, copy, modify, merge, publish, distribute and/or sell
 * copies of the Software, and permit persons to whom the Software is
 * furnished to do so, under the terms of the COPYING file.
 *
 * This software is distributed on an "AS IS" basis, WITHOUT WARRANTY OF ANY
 * KIND, either express or implied.
 *
 ***************************************************************************/
/* <DESC>
 * Get a web page, extract the title with libxml.
 * </DESC>

 Written by Lars Nilsson

 GNU C++ compile command line suggestion (edit paths accordingly):

 g++ -Wall -I/usr/include -I/usr/include/libxml2/ url-name-3.cpp \
 -L/usr/lib  -lcurl -lxml2 -pthread -fpermissive
*/
#include <ctime>
#include <curl/curl.h>
#include <iostream>
#include <libxml/HTMLparser.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <string>
#include <thread>
#include <vector>
//
//  Case-insensitive string comparison
//

#ifdef _MSC_VER
#define COMPARE(a, b) (!_stricmp((a), (b)))
#else
#define COMPARE(a, b) (!strcasecmp((a), (b)))
#endif

//
//  libxml callback context structure
//

struct Context {
  Context() : addTitle(false) {}

  bool addTitle;
  std::string title;
};

//
//  libcurl write callback function
//

static int writer(char *data, size_t size, size_t nmemb,
                  std::string *writerData) {
  if (writerData == NULL)
    return 0;

  writerData->append(data, size * nmemb);

  return size * nmemb;
}

//
//  libcurl connection initialization
//

static bool init(CURL *&conn, const char *url, char *errorBuffer,
                 std::string &buffer) {
  CURLcode code;

  conn = curl_easy_init();

  if (conn == NULL) {
    // fprintf(stderr, "Failed to create CURL connection\n");
    return false;
  }

  code = curl_easy_setopt(conn, CURLOPT_ERRORBUFFER, errorBuffer);
  if (code != CURLE_OK) {
    // fprintf(stderr, "Failed to set error buffer [%d]\n", code);
    return false;
  }

  code = curl_easy_setopt(conn, CURLOPT_URL, url);
  if (code != CURLE_OK) {
    // fprintf(stderr, "Failed to set URL [%s]\n", errorBuffer);
    return false;
  }

  code = curl_easy_setopt(conn, CURLOPT_FOLLOWLOCATION, 1L);
  if (code != CURLE_OK) {
    // fprintf(stderr, "Failed to set redirect option [%s]\n", errorBuffer);
    return false;
  }

  code = curl_easy_setopt(conn, CURLOPT_WRITEFUNCTION, writer);
  if (code != CURLE_OK) {
    // fprintf(stderr, "Failed to set writer [%s]\n", errorBuffer);
    return false;
  }

  code = curl_easy_setopt(conn, CURLOPT_WRITEDATA, &buffer);
  if (code != CURLE_OK) {
    // fprintf(stderr, "Failed to set write data [%s]\n", errorBuffer);
    return false;
  }

  code = curl_easy_setopt(conn, CURLOPT_TIMEOUT_MS, 1000);
  if (code != CURLE_OK) {
    // fprintf(stderr, "Failed to set timeout [%s]\n", errorBuffer);
    return false;
  }

  return true;
}

//
//  libxml start element callback function
//

static void StartElement(void *voidContext, const xmlChar *name,
                         const xmlChar **attributes) {
  Context *context = static_cast<Context *>(voidContext);

  if (COMPARE((char *)(name), "TITLE")) {
    context->title = "";
    context->addTitle = true;
  }
  (void)attributes;
}

//
//  libxml end element callback function
//

static void EndElement(void *voidContext, const xmlChar *name) {
  Context *context = static_cast<Context *>(voidContext);

  if (COMPARE((char *)(name), "TITLE"))
    context->addTitle = false;
}

//
//  Text handling helper function
//

static void handleCharacters(Context *context, const xmlChar *chars,
                             int length) {
  if (context->addTitle)
    context->title.append((char *)(chars), length);
}

//
//  libxml PCDATA callback function
//

static void Characters(void *voidContext, const xmlChar *chars, int length) {
  Context *context = static_cast<Context *>(voidContext);

  handleCharacters(context, chars, length);
}

//
//  libxml CDATA callback function
//

static void cdata(void *voidContext, const xmlChar *chars, int length) {
  Context *context = static_cast<Context *>(voidContext);

  handleCharacters(context, chars, length);
}

//
//  libxml SAX callback structure
//

static htmlSAXHandler saxHandler = {
    NULL, NULL, NULL, NULL, NULL, NULL,         NULL,       NULL,  NULL,
    NULL, NULL, NULL, NULL, NULL, StartElement, EndElement, NULL,  Characters,
    NULL, NULL, NULL, NULL, NULL, NULL,         NULL,       cdata, NULL};

//
//  Parse given (assumed to be) HTML text and return the title
//

static void parseHtml(const std::string &html, std::string &title) {
  htmlParserCtxtPtr ctxt;
  Context context;

  ctxt = htmlCreatePushParserCtxt(&saxHandler, &context, "", 0, "",
                                  XML_CHAR_ENCODING_NONE);

  htmlParseChunk(ctxt, html.c_str(), html.size(), 0);
  htmlParseChunk(ctxt, "", 0, 1);

  htmlFreeParserCtxt(ctxt);

  title = context.title;
}

std::string getUrlTitle(const char *url) {
  CURL *conn = NULL;
  CURLcode code;
  std::string title;
  char errorBuffer[CURL_ERROR_SIZE];
  std::string buffer;


  // Initialize CURL connection
  if (!init(conn, url, errorBuffer, buffer)) {
    // fprintf(stderr, "Connection initializion failed\n");
    return "";
  }

  // Retrieve content for the URL
  code = curl_easy_perform(conn);
  curl_easy_cleanup(conn);

  if (code != CURLE_OK) {
    // fprintf(stderr, "Failed to get '%s' [%s]\n", url, errorBuffer);
    return "";
  }

  parseHtml(buffer, title);

  return title;
}

std::string *titles;
void threadMod(std::size_t idx, std::string url) {
  std::string title = getUrlTitle(url.c_str());
  titles[idx] = title;
}

int main(int argc, char *argv[]) {
  std::vector<std::string> urls{
  "https://www.google.com/search?q=mlperf&client=firefox-b-1-d&ei=HY7NYN6gCInx-wS0-qbIDQ&oq=mlperf&gs_lcp=Cgdnd3Mtd2l6EAMyBQgAELEDMgIIADICCAAyAggAMgIIADICCAAyAggAMgIIADICCAAyAggAOggIABDqAhCPAToOCC4QxwEQowIQkQIQkwI6BQgAEJECOggILhCxAxCDAToLCC4QsQMQxwEQowI6CAgAELEDEIMBOgIILjoICAAQsQMQkQI6CAguEMcBEKMCOg4ILhDHARCvARCRAhCTAjoFCC4QsQM6CAguELEDEJMCSgUIOBIBMVDoBVjTHmD5H2gCcAB4AIABtwGIAYQGkgEDNi4ymAEAoAEBqgEHZ3dzLXdperABCsABAQ&sclient=gws-wiz&ved=0ahUKEwieo_CtiKPxAhWJ-J4KHTS9CdkQ4dUDCA4&uact=5",
      "https://www.youtube.com/channel/UCRXFk3Ow6PdyqYUCOeEe8ag",
      "https://www.youtube.com/watch?v=CS-GK6xS8-4",
      "https://www.youtube.com/",
      "https://www.youtube.com/c/TigraiMassMediaAgency",
      "https://www.youtube.com/c/TigraiMassMediaAgency/videos",
      "https://www.youtube.com/watch?v=sDE1P2H68g8",
      "https://www.youtube.com/c/AdebabayMedia/videos",
      "https://www.google.com/search?client=firefox-b-1-d&q=query+emacs",
      "https://www.google.com/"
      "search?client=firefox-b-1-d&q=python+sqlite3+fetchall",
      "https://docs.python.org/3/library/sqlite3.html",
      "https://gist.github.com/olejorgenb/9418bef65c65cd1f489557cfc08dde96",
      "https://www.google.com/"
      "search?client=firefox-b-1-d&q=unixephoch+sqlite+minute",
      "https://stackoverflow.com/questions/47587773/"
      "convert-sqlite-unix-epoch-in-milliseconds-to-datetime-yyyy-mm-dd-hhmmss-"
      "sss",
      "https://www.google.com/"
      "search?q=string+to+unixephoch+&client=firefox-b-1-d&ei=h1n9YPvpBdG8-"
      "gTN1L-ADQ&oq=string+to+unixephoch+&gs_lcp="
      "Cgdnd3Mtd2l6EAMyBggAEA0QHjIICAAQDRAFEB46BwgAEEcQsAM6CAgAEAgQDRAeOgQIABAN"
      "OgUIABCGA0oFCEASATFKBAhBGABQ-"
      "CVYpHtgjH1oAnABeACAAYYBiAGjEJIBBDEzLjiYAQCgAQGqAQdnd3Mtd2l6yAEIwAEB&"
      "sclient=gws-wiz&ved=0ahUKEwi7ydb6nP7xAhVRnp4KHU3qD9AQ4dUDCA4&uact=5",
      "https://www.google.com/"
      "search?q=string+to+unixephoch+&client=firefox-b-1-d&ei=h1n9YPvpBdG8-"
      "gTN1L-ADQ&oq=string+to+unixephoch+&gs_lcp="
      "Cgdnd3Mtd2l6EAMyBggAEA0QHjIICAAQDRAFEB46BwgAEEcQsAM6CAgAEAgQDRAeOgQIABAN"
      "OgUIABCGA0oFCEASATFKBAhBGABQ-"
      "CVYpHtgjH1oAnABeACAAYYBiAGjEJIBBDEzLjiYAQCgAQGqAQdnd3Mtd2l6yAEIwAEB&"
      "sclient=gws-wiz&ved=0ahUKEwi7ydb6nP7xAhVRnp4KHU3qD9AQ4dUDCA4&uact=5",
      "https://www.google.com/"
      "search?q=string+to+unix+epoch+sqlite&client=firefox-b-1-d&ei="
      "mFn9YJCfAcaL-wTU0Zf4BA&oq=string+to+unix+epoch+sqlite&gs_lcp="
      "Cgdnd3Mtd2l6EAMyCAghEBYQHRAeMggIIRAWEB0QHjIICCEQFhAdEB4yCAghEBYQHRAeOgcI"
      "ABBHELADOgYIABAWEB5KBQhAEgExSgQIQRgAUJYeWOJCYNJDaABwAXgAgAGZAogB1giSAQU0"
      "LjMuMZgBAKABAaoBB2d3cy13aXrIAQjAAQE&sclient=gws-wiz&ved=0ahUKEwiQy9-"
      "Cnf7xAhXGxZ4KHdToBU8Q4dUDCA4&uact=5",
      "https://www.sqlite.org/lang_datefunc.html",
      "https://stackoverflow.com/questions/14629347/"
      "how-to-convert-unix-epoch-time-in-sqlite",
      "https://stackoverflow.com/questions/17681439/"
      "convert-string-time-to-unix-timestamp",
      "https://www.google.com/search?client=firefox-b-1-d&q=get+time+sqlite3",
      "https://www.sqlite.org/lang_datefunc.html",
      "https://www.google.com/search?client=firefox-b-1-d&q=text+value+to+time+sqite3",
      "https://www.microsoft.com",
      "https://opensource.org",
      "https://www.google.com",
      "https://www.yahoo.com",
      "https://www.ibm.com",
      "https://www.mysql.com",
      "https://www.oracle.com",
      "https://www.ripe.net",
      "https://www.iana.org",
      "https://www.amazon.com",
      "https://www.netcraft.com",
      "https://www.heise.de",
      "https://www.chip.de",
      "https://www.ca.com",
      "https://www.cnet.com",
      "https://www.mozilla.org",
      "https://www.cnn.com",
      "https://www.wikipedia.org",
      "https://www.dell.com",
      "https://www.hp.com",
      "https://www.cert.org",
      "https://www.mit.edu",
      "https://www.nist.gov",
      "https://www.ebay.com",
      "https://www.playstation.com",
      "https://www.uefa.com",
      "https://www.ieee.org",
      "https://www.apple.com",
      "https://www.symantec.com",
      "https://www.zdnet.com",
      "https://www.fujitsu.com/global/",
      "https://www.supermicro.com",
      "https://www.hotmail.com",
      "https://www.ietf.org",
      "https://www.bbc.co.uk",
      "https://news.google.com",
      "https://www.foxnews.com",
      "https://www.msn.com",
      "https://www.wired.com",
      "https://www.sky.com",
      "https://www.usatoday.com",
      "https://www.cbs.com",
      "https://www.nbc.com/",
      "https://slashdot.org",
      "https://www.informationweek.com",
      "https://apache.org",
      "https://www.un.org",
      "https://en.cppreference.com/w/cpp/thread/thread/joinable"};
  urls.push_back("https://www.youtube.com/watch?v=CS-GK6xS8-4");
  urls.push_back("https://www.youtube.com/watch?v=CS-GK6xS8-4");
  std::vector<std::string> urls_new;
  for (std::size_t i = 0; i < 20; i++){
    urls_new.push_back(urls[i]);
  }
  urls = urls_new;
  titles = new std::string[urls.size()];
  std::vector<std::thread> workers;
  printf("%ld\n", urls.size());

  curl_global_init(CURL_GLOBAL_DEFAULT);
  for (std::size_t i = 0; i < urls.size(); i++) {
    workers.push_back(std::thread(threadMod, i, urls[i]));
    //threadMod(i, urls[i]);
  }
  std::size_t counter = 0;
  for (std::size_t i = 0; i < urls.size(); i++) {
      workers[i].join();
      if (! titles[i].empty()) {
        counter ++;
        printf("title: %s\n", titles[i].c_str());
      }
  }
  printf("%zu\n", counter);
  curl_global_cleanup();
  return 0;
}
