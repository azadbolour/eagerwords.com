
// TODO. Move to game/util.
import {
  errorClassifiers,
  defaultMessageTransformer,
  mkResponseToResultPromiseMapper, mkErrorData
} from "../../base/domain/BaseErrors";
import {stringify} from "../../base/util/Logger";

export const gamePart = "game";

export const errorTags = {
  // Server errors.
  unsupportedLanguage: 'UnsupportedLanguageError',
  missingDictionary: 'MissingDictionaryError',
  missingPiece: 'MissingPieceError',
  missingGame: 'MissingGameError',
  inactiveGame: 'InactiveGameError',
  invalidWord: 'InvalidWordError',
  invalidCrosswords: 'InvalidCrosswordsError',
  malformedPlay: 'MalformedPlayError',

  // Illegal move errors checked in UI.
  noMoves: 'no moves',
  multiplePlayLines: 'multiple play lines',
  incompleteWord: 'incomplete word',
  disconnectedWord: 'disconnectedWord',
  terminatedGame: 'terminatedGame',
};

export const gameClassifiers = {
  [errorTags.unsupportedLanguage]: errorClassifiers.recoverable,
  [errorTags.missingDictionary]: errorClassifiers.unrecoverable,
  [errorTags.missingPiece]: errorClassifiers.unrecoverable,
  [errorTags.missingGame]: errorClassifiers.recoverable,
  [errorTags.inactiveGame]: errorClassifiers.recoverable,
  [errorTags.invalidWord]: errorClassifiers.recoverable,
  [errorTags.invalidCrosswords]: errorClassifiers.recoverable,
  [errorTags.malformedPlay]: errorClassifiers.recoverable,

  [errorTags.noMoves]: errorClassifiers.recoverable,
  [errorTags.multiplePlayLines]: errorClassifiers.recoverable,
  [errorTags.incompleteWord]: errorClassifiers.recoverable,
  [errorTags.disconnectedWord]: errorClassifiers.recoverable,
  [errorTags.terminatedGame]: errorClassifiers.recoverable,
};

const uiMessages = {
  [errorTags.noMoves]: "no moves found for play",
  [errorTags.multiplePlayLines]: "multiple lines in both directions have moves",
  [errorTags.incompleteWord]: "moves do not result in a single word",
  [errorTags.disconnectedWord]: "played word does not include an existing tile",
  [errorTags.terminatedGame]: "attempt to operate on a terminated game - ignore",
};

export const mkUiErrorResult = (tag) => {
  let message = uiMessages[tag];
  if (!message)
    message = "internal error";
  let classifier = gameClassifiers[tag];
  if (!classifier)
    classifier = errorClassifiers.unrecoverable;
  return {
    ok: false,
    data: mkErrorData(gamePart, tag, classifier, message)
  }
};

// At a later time may wish to improve upon the server-side messages.
// export const gameMessageTransformer = defaultMessageTransformer;

// Special-case the UI error messages.
export const gameMessageTransformer = (jsonError) => {
  console.log(`jsonError: ${stringify(jsonError)}`);
  if (!jsonError)
    return "internal error";
  let tag = jsonError.tag;
  if (!tag)
    return "internal error";
  console.log(`trying to check uiMessages`);
  if (Object.keys(uiMessages).includes(tag))
    return uiMessages[tag];
  console.log(`message transformer calling default transformer`);
  return defaultMessageTransformer(jsonError);
};

export const gameResponseToResultPromiseMapper =
  mkResponseToResultPromiseMapper(gamePart, gameClassifiers, gameMessageTransformer);
