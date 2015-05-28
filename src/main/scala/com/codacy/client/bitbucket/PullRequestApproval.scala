package com.codacy.client.bitbucket

import play.api.libs.functional.syntax._
import play.api.libs.json._

case class PullRequestApproval(username: String, fullName: String, approved: Boolean)

object PullRequestApproval {
  implicit val reader: Reads[PullRequestApproval] = (
    (__ \ "user" \ "username").read[String] and
      (__ \ "user" \ "display_name").read[String] and
      (__ \ "approved").read[Boolean]
    )(PullRequestApproval.apply _)
}
