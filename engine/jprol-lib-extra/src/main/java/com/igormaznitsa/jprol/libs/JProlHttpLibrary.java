package com.igormaznitsa.jprol.libs;

import static com.igormaznitsa.jprol.utils.ProlAssertions.assertList;
import static java.net.HttpURLConnection.HTTP_OK;
import static java.util.Objects.requireNonNull;

import com.igormaznitsa.jprol.annotations.JProlPredicate;
import com.igormaznitsa.jprol.data.NumericTerm;
import com.igormaznitsa.jprol.data.Term;
import com.igormaznitsa.jprol.data.TermList;
import com.igormaznitsa.jprol.data.TermOperator;
import com.igormaznitsa.jprol.data.TermStruct;
import com.igormaznitsa.jprol.data.TermType;
import com.igormaznitsa.jprol.data.Terms;
import com.igormaznitsa.jprol.exceptions.ProlCriticalError;
import com.igormaznitsa.jprol.exceptions.ProlDomainErrorException;
import com.igormaznitsa.jprol.exceptions.ProlException;
import com.igormaznitsa.jprol.exceptions.ProlPermissionErrorException;
import com.igormaznitsa.jprol.exceptions.RuntimeIOException;
import com.igormaznitsa.jprol.logic.JProlChoicePoint;
import com.igormaznitsa.prologparser.tokenizer.OpAssoc;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.net.HttpURLConnection;
import java.net.MalformedURLException;
import java.net.ProtocolException;
import java.net.URL;
import java.nio.charset.StandardCharsets;
import java.util.ArrayList;
import java.util.List;
import java.util.Locale;
import java.util.Map;
import java.util.function.Function;
import java.util.stream.Collectors;

@SuppressWarnings({"EmptyMethod", "unused", "checkstyle:AbbreviationAsWordInName"})
public class JProlHttpLibrary extends AbstractJProlLibrary {

  private static final Term DEFAULT_REQUEST_METHOD = Terms.newAtom("GET");
  private final Function<URL, HttpURLConnection> urlConnectionFactory;

  public JProlHttpLibrary() {
    this("jprol-http-lib", JProlHttpLibrary::makeConnection);
  }

  public JProlHttpLibrary(final String libraryUid,
                          final Function<URL, HttpURLConnection> urlConnectionFactory) {
    super(libraryUid);
    this.urlConnectionFactory = requireNonNull(urlConnectionFactory);
  }

  private static HttpURLConnection makeConnection(final URL url) {
    try {
      return (HttpURLConnection) url.openConnection();
    } catch (IOException ex) {
      throw new RuntimeIOException(ex);
    }
  }

  private static TermList readResponse(final HttpURLConnection connection) {
    return null;
  }

  private Term findValue(final String key, final TermList list) {
    final TermStruct found = list.streamChildren()
        .filter(f -> f.getTermType() == TermType.STRUCT)
        .map(f -> (TermStruct) f)
        .filter(f -> f.getArity() == 2
            && "=".equals(f.getFunctor().getText())
            && f.getArgumentAt(0).tryGround().getText().equalsIgnoreCase(key))
        .findFirst().orElse(null);
    return found == null ? null : found.getArgumentAt(1).tryGround();
  }

  private Term findMandatory(final HttpRequestParameters key, final TermList list) {
    final Term found = findValue(key.name().toLowerCase(Locale.ROOT), list);
    if (found == null) {
      throw new ProlDomainErrorException(
          "Expected mandatory value: " + key.name().toLowerCase(Locale.ROOT), list);
    }
    return found;
  }

  private Term findNonMandatory(final HttpRequestParameters key, final TermList list,
                                final Term defaultTerm) {
    final Term found = findValue(key.name().toLowerCase(Locale.ROOT), list);
    if (found == null) {
      return defaultTerm;
    }
    return found;
  }

  @JProlPredicate(determined = true, signature = "http_req/2", args = {
      "+list,?list"}, reference = "Make HTTP request which parameters from the left list, the response represented as the right list. The format of lists is [name1=value1,name2=value2,..nameN=valueN]")
  public boolean predicateHTTP_REQ(final JProlChoicePoint goal,
                                   final TermStruct predicate) {
    final TermOperator equalsOperator =
        goal.getContext().findSystemOperatorForNameAndAssociativity("=", OpAssoc.XFX);
    if (equalsOperator == null) {
      throw new ProlCriticalError("Can't find required operator =/2 XFX");
    }

    final Term arequest = predicate.getArgumentAt(0).tryGround();
    final Term argResponse = predicate.getArgumentAt(1).tryGround();

    final TermList requestList = (TermList) arequest;

    HttpURLConnection httpConnection = null;
    try {
      final String url = findMandatory(HttpRequestParameters.URL, requestList).getText();
      httpConnection = this.urlConnectionFactory.apply(new URL(url));

      httpConnection.setRequestMethod(findNonMandatory(HttpRequestParameters.METHOD, requestList,
          DEFAULT_REQUEST_METHOD).getText());

      final Term connectionTimeout =
          findValue(HttpRequestParameters.CONNECT_TIMEOUT.name(), requestList);
      if (connectionTimeout != null) {
        httpConnection.setConnectTimeout(connectionTimeout.toNumber().intValue());
      }

      final Term readTimeout = findValue(HttpRequestParameters.READ_TIMEOUT.name(), requestList);
      if (readTimeout != null) {
        httpConnection.setReadTimeout(readTimeout.toNumber().intValue());
      }

      final Term headers = findValue(HttpRequestParameters.HEADERS.name(), requestList);
      if (headers != null) {
        assertList(headers);
        for (final Term h : (TermList) headers) {
          if (h.getTermType() == TermType.STRUCT
              && ((TermStruct) h).getFunctor().getText().equals("=")
              && ((TermStruct) h).getArity() == 2) {
            final String headerName =
                ((TermStruct) h).getArgumentAt(0).tryGround().getText();
            final String headerValue =
                ((TermStruct) h).getArgumentAt(1).tryGround().getText();
            httpConnection.setRequestProperty(headerName, headerValue);
          } else {
            throw new ProlDomainErrorException("Expected name=value", h);
          }
        }
      }

      final Term followRedirections =
          findValue(HttpRequestParameters.FOLLOW_REDIRECTIONS.name(), requestList);
      if (followRedirections != null) {
        httpConnection.setInstanceFollowRedirects(
            "true".equalsIgnoreCase(followRedirections.getText()));
      }

      final Term requestBody = findValue(HttpRequestParameters.BODY.name(), requestList);
      if (requestBody != null) {
        httpConnection.setDoOutput(true);

        try (final OutputStream outputStream = httpConnection.getOutputStream()) {
          if (requestBody.getTermType() == TermType.LIST) {
            for (final Term t : (TermList) requestBody) {
              if (t instanceof NumericTerm) {
                outputStream.write(t.toNumber().intValue());
              } else {
                final String text = t.getText();
                outputStream.write(text.getBytes(StandardCharsets.UTF_8));
              }
            }
          } else {
            final String text = requestBody.getText();
            outputStream.write(text.getBytes(StandardCharsets.UTF_8));
          }
          outputStream.flush();
        }
      } else {
        httpConnection.setDoOutput(false);
      }

      final int responseCode = httpConnection.getResponseCode();
      final String responseMessage = httpConnection.getResponseMessage();

      final List<Term> responseListTerms = new ArrayList<>();
      responseListTerms.add(Terms.newStruct(equalsOperator,
          new Term[] {
              Terms.newAtom(HttpRespnseParameters.RESPONSE_CODE.name().toLowerCase(Locale.ROOT)),
              Terms.newLong(responseCode)}));
      responseListTerms.add(Terms.newStruct(equalsOperator,
          new Term[] {Terms.newAtom(
              HttpRespnseParameters.RESPONSE_MESSAGE.name().toLowerCase(Locale.ROOT)),
              Terms.newAtom(responseMessage == null ? "" : responseMessage)}));

      if (responseCode == HTTP_OK) {
        final List<Term> responseHeaders = new ArrayList<>();
        for (final Map.Entry<String, List<String>> e : httpConnection.getHeaderFields()
            .entrySet()) {
          if (e.getKey() != null && e.getValue() != null) {
            responseHeaders.add(
                Terms.newStruct(equalsOperator, new Term[] {Terms.newAtom(e.getKey()),
                    TermList.asList(
                        e.getValue().stream().map(Terms::newAtom).collect(Collectors.toList()))}));
          }
        }
        responseListTerms.add(Terms.newStruct(equalsOperator,
            new Term[] {
                Terms.newAtom(HttpRespnseParameters.HEADERS.name().toLowerCase(Locale.ROOT)),
                TermList.asList(responseHeaders)}));

        final List<Term> listBody = new ArrayList<>();
        try (final InputStream inputStream = httpConnection.getInputStream()) {
          int value;
          while ((value = inputStream.read()) >= 0) {
            listBody.add(Terms.newLong(value));
          }
        }
        final TermList bodyAsBin = TermList.asList(listBody);
        responseListTerms.add(Terms.newStruct(equalsOperator,
            new Term[] {
                Terms.newAtom(HttpRespnseParameters.BODY.name().toLowerCase(Locale.ROOT)),
                bodyAsBin}));
      }

      return argResponse.unifyTo(TermList.asList(responseListTerms));
    } catch (ProtocolException ex) {
      throw new ProlDomainErrorException(
          "Expected request method: ['GET', 'POST', 'HEAD', 'OPTIONS', 'PUT', 'DELETE', 'TRACE']",
          arequest);
    } catch (MalformedURLException ex) {
      throw new ProlDomainErrorException("Expected URL in correct syntax", arequest);
    } catch (IOException ex) {
      throw new ProlPermissionErrorException("write", "text_output", predicate, ex);
    } catch (Exception ex) {
      if (httpConnection != null) {
        try {
          httpConnection.disconnect();
        } catch (Exception e) {
          // do nothing
        }
      }

      if (ex instanceof ProlException) {
        throw (ProlException) ex;
      }
    }
    return true;
  }

  public enum HttpRequestParameters {
    URL,
    METHOD,
    CONNECT_TIMEOUT,
    READ_TIMEOUT,
    HEADERS,
    FOLLOW_REDIRECTIONS,
    BODY
  }

  public enum HttpRespnseParameters {
    HEADERS,
    RESPONSE_CODE,
    RESPONSE_MESSAGE,
    BODY
  }

}
