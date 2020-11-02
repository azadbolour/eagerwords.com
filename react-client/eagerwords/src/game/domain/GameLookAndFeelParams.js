
// TODO. Capitalize to conform to standard naming conventions.

export const squareSize = {
  small: 'Small',
  normal: 'Normal',
  large: 'Large'
};
export const validSquareSizes = [squareSize.small, squareSize.normal, squareSize.large];
export const defaultSquareSize = squareSize.normal;

export const defaultGameLookAndFeelParams = {
  squareSize: defaultSquareSize,
  preferredDevice: null  // This is an option. None is encoded as null.
};

export const squareSizeToPixels = {
  [squareSize.small]: 25,
  [squareSize.normal]: 40,
  [squareSize.large]: 55
};

// TODO. Check users of valueFont.
export const squareSizeToPointValueFont = {
  [squareSize.small]: 7,
  [squareSize.normal]: 9,
  [squareSize.large]: 11
};

// TODO. Check users of pieceFont.
export const squareSizeToPieceFont = {
  [squareSize.small]: 12,
  [squareSize.normal]: 22,
  [squareSize.large]: 24
};