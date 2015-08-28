package com.codacy.client.bitbucket.client

import com.codacy.client.bitbucket.util.HTTPStatusCodes
import play.api.libs.json.{JsValue, Json, Reads}

import scala.util.{Failure, Properties, Success, Try}
import scalaj.http.{Http, HttpRequest, Token}

class BitbucketClient(key: String, secretKey: String, token: String, secretToken: String) {

  private lazy val KEY = Token(key, secretKey)
  private lazy val TOKEN = Token(token, secretToken)

  /*
   * Does an API request and parses the json output into a class
   */
  def execute[T](request: Request[T])(implicit reader: Reads[T]): RequestResponse[T] = {
    get(request.url) match {
      case Right(json) => RequestResponse(json.asOpt[T])
      case Left(error) => RequestResponse(None, error.detail, hasError = true)
    }
  }

  /*
   * Does a paginated API request and parses the json output into a sequence of classes
   */
  def executePaginated[T](request: Request[Seq[T]])(implicit reader: Reads[T]): RequestResponse[Seq[T]] = {
    get(request.url) match {
      case Right(json) =>
        val nextPage = (json \ "next").asOpt[String]
        val nextRepos = nextPage.map {
          nextUrl =>
            executePaginated(Request(nextUrl, request.classType)).value.getOrElse(Seq())
        }.getOrElse(Seq())

        val values = (json \ "values").asOpt[Seq[T]].getOrElse(Seq())
        RequestResponse(Some(values ++ nextRepos))

      case Left(error) =>
        RequestResponse[Seq[T]](None, error.detail, hasError = true)
    }
  }

  /*
   * Does an API post
   */
  def post[T](request: Request[T], values: JsValue)(implicit reader: Reads[T]): RequestResponse[T] = withClientRequest(request.url) { client =>
    val response = client.postData(values.toString()).asString

    val value = if (Seq(HTTPStatusCodes.OK, HTTPStatusCodes.CREATED).contains(response.code)) {
      val jsValue = parseJson(response.body)
      jsValue match {
        case Right(responseObj) =>
          RequestResponse(responseObj.asOpt[T])
        case Left(message) =>
          RequestResponse[T](None, message = message.detail, hasError = true)
      }
    } else {
      RequestResponse[T](None, response.statusLine, hasError = true)
    }

    value
  }

  /* copy paste from post ... */
  def delete[T](url: String): RequestResponse[Boolean] = withClientRequest(url) { client =>
    val response = client.postData("").method("delete").asString

    val value = if (Seq(HTTPStatusCodes.OK, HTTPStatusCodes.CREATED, HTTPStatusCodes.NO_CONTENT).contains(response.code)) {
      RequestResponse(Option(true))
    } else {
      RequestResponse[Boolean](None, response.statusLine, hasError = true)
    }

    value
  }

  private def get(url: String): Either[ResponseError, JsValue] = withClientEither(url) { client =>
    val response = client.asString

    val value = if (Seq(HTTPStatusCodes.OK, HTTPStatusCodes.CREATED).contains(response.code)) {
      parseJson(response.body)
    } else {
      Left(ResponseError(java.util.UUID.randomUUID().toString, response.statusLine, response.statusLine))
    }

    value
  }

  private def parseJson(input: String): Either[ResponseError, JsValue] = {
    val json = Json.parse(input)

    val errorOpt = (json \ "error").asOpt[ResponseError]

    errorOpt.map {
      error =>
        Left(error)
    }.getOrElse(Right(json))
  }

  private def withClientEither[T](url: String)(block: HttpRequest => Either[ResponseError, T]): Either[ResponseError, T] = {
    withClient(url)(block) match {
      case Success(res) => res
      case Failure(error) =>
        Left(ResponseError("Request failed", getFullStackTrace(error), error.getMessage))
    }
  }

  private def withClientRequest[T](url: String)(block: HttpRequest => RequestResponse[T]): RequestResponse[T] = {
    withClient(url)(block) match {
      case Success(res) => res
      case Failure(error) =>
        val statusMessage =
          s"""
             |Failed request:
             |
             |${getFullStackTrace(error)}
          """.stripMargin
        RequestResponse[T](None, statusMessage, hasError = true)
    }
  }

  private def withClient[T](url: String)(block: HttpRequest => T): Try[T] = {
    val client = Http(url).oauth(KEY, TOKEN)
    Try(block(client))
  }

  private def getFullStackTrace(throwableOpt: Throwable, accumulator: String = ""): String = {
    Option(throwableOpt).map { throwable =>
      val newAccumulator = s"$accumulator${Properties.lineSeparator}${throwable.getStackTraceString}"
      getFullStackTrace(throwable.getCause, newAccumulator)
    }.getOrElse(accumulator)
  }

}
