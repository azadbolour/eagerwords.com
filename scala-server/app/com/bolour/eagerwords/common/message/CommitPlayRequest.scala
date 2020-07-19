package com.bolour.eagerwords.common.message

import com.bolour.app.kernel.common.domain.AuthEvidence
import com.bolour.eagerwords.common.domain.PlayPiece

case class CommitPlayRequest(
  loginEvidence: Option[AuthEvidence],
  playPieces: List[PlayPiece]
)
